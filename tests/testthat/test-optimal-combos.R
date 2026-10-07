## Brute-force reference test: enumerates every combination directly over a small filtered
## subset and applies the same weight/minima constraints and scoring formula, independent of
## the shell-expansion and priority-queue machinery that get.optimal.armor.combos and
## optimal_armor_combinations use internally. This is the test that would have caught the
## minima-indexing bug fixed alongside this test suite (see git history for
## R/optimal-combos.R): a wrong index into `minima` there can inflate the starting
## shell size and cause the C++ search to silently skip feasible combinations.
test_that("get.optimal.armor.combos matches a brute-force reference over a small filtered subset", {

    set.seed(20260814)
    pool.head <- setdiff(head.data.unupgraded$ARMOR, "Mask of the Father")
    head.sel <- sample(pool.head, 5)
    chest.sel <- sample(chest.data.unupgraded$ARMOR, 5)
    hands.sel <- sample(hands.data.unupgraded$ARMOR, 5)
    legs.sel <- sample(legs.data.unupgraded$ARMOR, 5)

    minima <- c(0, 0, 0, 0, 0, 0, 0, 5, 3, 2, 1, 0)
    weapons <- c(right.1 = "Zweihander")  ## weight 10
    endurance.level <- 40
    movement <- "Fat"

    actual <-
        get.optimal.armor.combos(
            max.table.size = 5000,
            head.filter = head.sel, chest.filter = chest.sel, hands.filter = hands.sel, legs.filter = legs.sel,
            movement = movement,
            weapons = weapons,
            endurance.level = endurance.level,
            minima = minima
        )$data

    ## True brute force: every (head, chest, hands, legs) combination in the filtered subset.
    h <- head.data.unupgraded[ARMOR %in% head.sel]
    c <- chest.data.unupgraded[ARMOR %in% chest.sel]
    g <- hands.data.unupgraded[ARMOR %in% hands.sel]
    l <- legs.data.unupgraded[ARMOR %in% legs.sel]

    metric.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")

    grid <- data.table::CJ(H = seq_len(nrow(h)), C = seq_len(nrow(c)), G = seq_len(nrow(g)), L = seq_len(nrow(l)))
    for(col in metric.cols){
        data.table::set(grid, j = col, value = h[[col]][grid$H] + c[[col]][grid$C] + g[[col]][grid$G] + l[[col]][grid$L])
    }
    grid[, POISE := h$POISE[H] + c$POISE[C] + g$POISE[G] + l$POISE[L]]
    grid[, DURABILITY := pmin(h$DURABILITY[H], c$DURABILITY[C], g$DURABILITY[G], l$DURABILITY[L])]
    grid[, WEIGHT := h$WEIGHT[H] + c$WEIGHT[C] + g$WEIGHT[G] + l$WEIGHT[L]]

    ## Same score formula get.optimal.armor.combos uses (means/stddevs/corrs are internal,
    ## R/sysdata.rda, so accessed here via :::): the package computes each slot's score as
    ## score.scalars[i]*(metric_i - 0.25*means[i]) and sums the four slots, so summing metrics
    ## across slots first requires the full means[i] (four slots x 0.25*means[i] = means[i])
    ## applied once here.
    stddevs <- darksoulsarmor:::stddevs
    corrs <- darksoulsarmor:::corrs
    means <- darksoulsarmor:::means
    weights <- c(0.16, 0.16, 0.16, 0.16, 0.08, 0.08, 0.08, 0.04, 0.04, 0.04)
    score.scalars <- weights / (stddevs * sqrt((t(weights) %*% corrs %*% weights)[1, 1]))
    grid[, SCORE := 0]
    for(i in seq_along(metric.cols)){
        grid[, SCORE := SCORE + score.scalars[i] * (get(metric.cols[i]) - means[i])]
    }

    ## The game's equip-load check, written out: the Zweihander, then head, chest, hands and legs,
    ## added in 32-bit, against the Fat line (100% of 40 + 40, no rings)
    f32 <- darksoulsarmor:::float32
    total <- f32(10)
    for(w in list(h$WEIGHT[grid$H], c$WEIGHT[grid$C], g$WEIGHT[grid$G], l$WEIGHT[grid$L])){ total <- f32(total + f32(w)) }

    eps <- 1e-8
    ok <-
        (total <= f32(endurance.level + 40)) &
        (grid$PHYS_DEF   >= minima[1]  - eps) & (grid$STRIKE_DEF >= minima[2]  - eps) &
        (grid$SLASH_DEF  >= minima[3]  - eps) & (grid$THRUST_DEF >= minima[4]  - eps) &
        (grid$MAG_DEF    >= minima[5]  - eps) & (grid$FIRE_DEF   >= minima[6]  - eps) &
        (grid$LITNG_DEF  >= minima[7]  - eps) & (grid$POISE      >= minima[8]  - eps) &
        (grid$BLEED_RES  >= minima[9]  - eps) & (grid$POIS_RES   >= minima[10] - eps) &
        (grid$CURSE_RES  >= minima[11] - eps) & (grid$DURABILITY >= minima[12] - eps)

    expected <- grid[ok]
    expected[, HEAD := h$ARMOR[H]]
    expected[, CHEST := c$ARMOR[C]]
    expected[, HANDS := g$ARMOR[G]]
    expected[, LEGS := l$ARMOR[L]]
    expected[, KEY := paste(HEAD, CHEST, HANDS, LEGS, sep = "|")]

    actual.key <- paste(actual$HEAD, actual$CHEST, actual$HANDS, actual$LEGS, sep = "|")

    ## Should be well under max.table.size, so `actual` holds every feasible combination.
    expect_true(nrow(actual) < 5000)
    expect_setequal(actual.key, expected$KEY)

    match.idx <- match(actual.key, expected$KEY)
    expect_equal(actual$SCORE_RAW, expected$SCORE[match.idx], tolerance = 1e-6)
})

## score.quality() counts combinations scoring at least (or at most) as well via sorted sums and
## binary search rather than by visiting them. Checked here against literally enumerating every
## combination of small random slot score lists - including tied scores (rounded scores make
## many ties), scores exactly equal to a real combination's total, and both Top and Bottom.
test_that("score.quality matches a brute-force count over every combination", {
    set.seed(20261006)
    for(trial in 1:20){
        h <- round(rnorm(sample(1:8, 1)), 1); c <- round(rnorm(sample(1:8, 1)), 1)
        g <- round(rnorm(sample(1:8, 1)), 1); l <- round(rnorm(sample(1:8, 1)), 1)
        all.totals <- as.vector(outer(outer(outer(h, c, `+`), g, `+`), l, `+`))
        ## Real totals, summed in SCORE_RAW's order (head+chest+hands+legs), plus off-grid values
        targets <- c(sample(all.totals, 10, replace = TRUE), runif(5, min(all.totals), max(all.totals)))
        at.least <- sapply(targets, function(s) sum(all.totals >= s - 1e-9))
        at.most <- sapply(targets, function(s) sum(all.totals <= s + 1e-9))
        n <- length(all.totals)
        expected <- ifelse(
            at.least <= n/2,
            paste0("Top 1 in ", format(round(n/at.least), big.mark = ",", scientific = FALSE, trim = TRUE)),
            paste0("Bottom 1 in ", format(round(n/at.most), big.mark = ",", scientific = FALSE, trim = TRUE))
        )
        expect_equal(darksoulsarmor:::score.quality(targets, h, c, g, l), expected)
    }
})

## Every one of the 68 x 57 x 54 x 57 = 11,930,328 combinations at a level counts toward
## SCORE_QUALITY regardless of filters, so the best combination at +10/+5 with no load limit -
## which no other combination ties or beats - is exactly "Top 1 in 11,930,328".
test_that("SCORE_QUALITY ranks the best combination at a level against every combination there", {
    result <- get.optimal.armor.combos(max.table.size = 20, movement = "Poop", regular.level = "+10", twinkling.level = "+5")$data
    expect_equal(names(result)[1:2], c("SCORE_RAW", "SCORE_QUALITY"))
    expect_equal(result$SCORE_QUALITY[1], "Top 1 in 11,930,328")
    expect_true(all(grepl("^Top 1 in ", result$SCORE_QUALITY)))
})

test_that("SCORE_QUALITY reads 'Bottom 1 in N' when even the best feasible combination is in the bottom half", {
    ## The default endurance.level = 10 and movement = "Light", carrying a Zweihander (10), leave
    ## only 2.5 units of equip load for armor, so even the best feasible combination is a
    ## below-median one.
    result <- get.optimal.armor.combos(max.table.size = 20, weapons = c(right.1 = "Zweihander"))$data
    expect_true(all(grepl("^Bottom 1 in ", result$SCORE_QUALITY)))
})

## EQUIP_LOAD is the game's exact 32-bit equip load: (40 + Endurance) times the rings' rates
## multiplied together, e.g. 41 x 1.2 = 49.2000008 (shown in-game as 49.2), and the Mask of the
## Father's own load with its x1.05 combined in. TOTAL_WEIGHT is the game's 32-bit total, so a Light
## set's PCT_LOAD is at most exactly 25%.
test_that("EQUIP_LOAD and TOTAL_WEIGHT are the game's exact 32-bit values", {
    f32 <- darksoulsarmor:::float32
    result <- get.optimal.armor.combos(max.table.size = 200, endurance.level = 1, favor.ring = TRUE, movement = "Light")$data
    non.motf <- result[HEAD != "Mask of the Father"]
    expect_true(nrow(non.motf) > 0)
    expect_equal(unique(non.motf$EQUIP_LOAD), f32(41*f32(1.2)), tolerance = 0)
    motf <- result[HEAD == "Mask of the Father"]
    expect_true(nrow(motf) > 0)
    expect_equal(unique(motf$EQUIP_LOAD), f32(41*f32(f32(1.2)*f32(1.05))), tolerance = 0)
    expect_true(all(result$PCT_LOAD <= 0.25))
    expect_true(all(f32(result$TOTAL_WEIGHT) == result$TOTAL_WEIGHT))
})

## optimal_armor_combinations reserves heap storage up front, capped at the number of combinations
## the filtered tables can form - without the cap, max.table.size = 1e8 against 2 pieces per slot
## committed ~2.4 GB for at most 16 results. testthat can't observe peak memory, so this guards the
## behavior: a huge max.table.size over tiny tables still returns exactly every combination.
test_that("a max.table.size far above the possible combinations returns exactly those combinations", {
    result <- get.optimal.armor.combos(
        max.table.size = 1e8,
        head.filter = head.data.unupgraded$ARMOR[1:2], chest.filter = chest.data.unupgraded$ARMOR[1:2],
        hands.filter = hands.data.unupgraded$ARMOR[1:2], legs.filter = legs.data.unupgraded$ARMOR[1:2],
        movement = "Poop"
    )$data
    expect_equal(nrow(result), 16)
    expect_equal(nrow(unique(result[, .(HEAD, CHEST, HANDS, LEGS)])), 16)
})

## Randomized brute-force comparison covering what the fixed brute-force test above doesn't:
## Mask of the Father in the pool (its own x1.05 load threshold and equip load), Wolf Ring,
## upgrade levels above +0, rings, uneven slot sizes (the C++ search's capped-dimension shell
## jumps), and max.table.size below the feasible count - the only case where the results heap
## fills and the branch-and-bound breaks run. The coverage asserts at the end make sure the random
## setup keeps reaching those last two paths.
test_that("get.optimal.armor.combos matches brute force across randomized searches", {

    set.seed(20261007)
    means <- darksoulsarmor:::means
    stddevs <- darksoulsarmor:::stddevs
    corrs <- darksoulsarmor:::corrs
    metric.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
    heap.filled <- 0
    father.mask.returned <- 0
    cutoff.ties <- 0

    for(trial in 1:40){

        head.sel <- sample(head.data.unupgraded$ARMOR, sample(2:12, 1))
        if(runif(1) < 0.7){
            head.sel <- union(head.sel, "Mask of the Father")
        }
        args <- list(
            head.filter = head.sel,
            chest.filter = sample(chest.data.unupgraded$ARMOR, sample(1:12, 1)),
            hands.filter = sample(hands.data.unupgraded$ARMOR, sample(1:12, 1)),
            legs.filter = sample(legs.data.unupgraded$ARMOR, sample(1:12, 1)),
            regular.level = paste0("+", sample(0:10, 1)),
            twinkling.level = paste0("+", sample(0:5, 1)),
            movement = sample(c("Light", "Mid", "Fat"), 1),
            endurance.level = sample(5:60, 1),
            ## 0-4 weapons in random slots, talismans (not exact in 32-bit) often among them
            weapons = {
                slots <- sample(c("left.1", "right.1", "left.2", "right.2"), sample(0:4, 1))
                stats::setNames(sample(c(weapon.data$WEAPON, rep("Talisman", 20)), length(slots)), slots)
            },
            havel.ring = runif(1) < 0.3,
            favor.ring = runif(1) < 0.3,
            wolf.ring = runif(1) < 0.4,
            minima = c(sample(c(0, 0, 0, 20, 60), 7, replace = TRUE), sample(c(0, 0, 10, 30, 50), 1), sample(c(0, 0, 5, 20), 3, replace = TRUE), sample(c(0, 0, 0, 200, 300), 1)),
            ## A single scored resistance (whole numbers with few distinct values) makes exact score ties
            ## common, exercising the tie-break - including at the max.table.size cutoff
            weights = if(runif(1) < 0.5) replace(rep(0, 10), sample(8:10, 1), 1) else runif(10)*(runif(10) < 0.8) + c(1e-3, rep(0, 9)),
            max.table.size = sample(c(1, 3, 10, 50, 200, 1e5), 1)
        )
        actual <- do.call(get.optimal.armor.combos, args)$data

        ## Brute force: every combination of the selected pieces at the selected levels
        reg <- as.numeric(args$regular.level)
        twink <- as.numeric(args$twinkling.level)
        h <- darksoulsarmor:::get.interp.data(head.data.unupgraded, reg, twink)[ARMOR %in% args$head.filter]
        c <- darksoulsarmor:::get.interp.data(chest.data.unupgraded, reg, twink)[ARMOR %in% args$chest.filter]
        g <- darksoulsarmor:::get.interp.data(hands.data.unupgraded, reg, twink)[ARMOR %in% args$hands.filter]
        l <- darksoulsarmor:::get.interp.data(legs.data.unupgraded, reg, twink)[ARMOR %in% args$legs.filter]

        ## Each piece's score and each slot's order exactly as get.optimal.armor.combos computes
        ## them, so totals are bit-identical to SCORE_RAW and row positions match the C++ search's
        ## indices (its last tie-break)
        weights <- args$weights/sum(args$weights)
        score.scalars <- weights/(stddevs*sqrt((t(weights) %*% corrs %*% weights)[1, 1]))
        for(slot in list(h, c, g, l)){
            slot[, SCORE := 0]
            for(i in seq_along(metric.cols)){
                slot[, SCORE := SCORE+score.scalars[i]*(get(metric.cols[i])-0.25*means[i])]
            }
            data.table::setorder(slot, -SCORE, WEIGHT)
        }

        grid <- data.table::CJ(H = seq_len(nrow(h)), C = seq_len(nrow(c)), G = seq_len(nrow(g)), L = seq_len(nrow(l)))
        for(col in c(metric.cols, "POISE", "WEIGHT")){
            data.table::set(grid, j = col, value = h[[col]][grid$H] + c[[col]][grid$C] + g[[col]][grid$G] + l[[col]][grid$L])
        }
        grid[, DURABILITY := pmin(h$DURABILITY[H], c$DURABILITY[C], g$DURABILITY[G], l$DURABILITY[L])]
        grid[, KEY := paste(h$ARMOR[H], c$ARMOR[C], g$ARMOR[G], l$ARMOR[L], sep = "|")]
        grid[, FATHER_MASK := h$ARMOR[H] == "Mask of the Father"]
        grid[, SCORE := h$SCORE[H] + c$SCORE[C] + g$SCORE[G] + l$SCORE[L]]
        ## The C++ search's score comparison key: llround(score*1e9)
        grid[, SCORE_KEY := sign(SCORE)*floor(abs(SCORE)*1e9 + 0.5)]

        ## The game's equip-load check, written out (see R/equip-load.R): rates multiplied together,
        ## then the 32-bit load and line; the weapons in slot order, then head, chest, hands, legs,
        ## each added in 32-bit; at or below the line
        f32 <- darksoulsarmor:::float32
        rate <- f32(f32(ifelse(args$havel.ring, f32(1.5), 1))*ifelse(args$favor.ring, f32(1.2), 1))
        load <- f32((args$endurance.level + 40)*rate)
        load.father.mask <- f32((args$endurance.level + 40)*f32(rate*f32(1.05)))
        share <- c(Light = 0.25, Mid = 0.5, Fat = 1)[[args$movement]]
        carried <- 0
        for(slot in c("left.1", "right.1", "left.2", "right.2")){
            if(slot %in% names(args$weapons)){ carried <- f32(carried + f32(weapon.data[WEAPON == args$weapons[[slot]], WEIGHT])) }
        }
        grid[, TOTAL := f32(f32(f32(f32(carried + f32(h$WEIGHT[H])) + f32(c$WEIGHT[C])) + f32(g$WEIGHT[G])) + f32(l$WEIGHT[L]))]
        grid[, ARMOR32 := f32(f32(f32(f32(h$WEIGHT[H]) + f32(c$WEIGHT[C])) + f32(g$WEIGHT[G])) + f32(l$WEIGHT[L]))]
        grid[, LOAD := ifelse(FATHER_MASK, load.father.mask, load)]
        eps <- 1e-10
        mn <- args$minima
        expected <- grid[
            TOTAL <= f32(LOAD*share) &
            PHYS_DEF >= mn[1] - eps & STRIKE_DEF >= mn[2] - eps & SLASH_DEF >= mn[3] - eps & THRUST_DEF >= mn[4] - eps &
            MAG_DEF >= mn[5] - eps & FIRE_DEF >= mn[6] - eps & LITNG_DEF >= mn[7] - eps & POISE + 40*args$wolf.ring >= mn[8] - eps &
            BLEED_RES >= mn[9] - eps & POIS_RES >= mn[10] - eps & CURSE_RES >= mn[11] - eps & DURABILITY >= mn[12] - eps
        ][order(-SCORE_KEY, WEIGHT, -POISE, -DURABILITY, H, C, G, L)]

        k <- min(args$max.table.size, nrow(expected))
        if(k > 0 && k < nrow(expected) && expected$SCORE_KEY[k] == expected$SCORE_KEY[k + 1]){
            cutoff.ties <- cutoff.ties + 1
        }
        if(k < nrow(expected)){
            heap.filled <- heap.filled + 1
        }
        if("Mask of the Father" %in% actual$HEAD){
            father.mask.returned <- father.mask.returned + 1
        }

        expect_equal(nrow(actual), k, info = paste("trial", trial))
        if(k > 0){
            ## The tie-break (lighter, then more poise, then more durability, then row position)
            ## makes the result fully determined: exactly these rows, in exactly this order,
            ## including which tied combinations make the cut at max.table.size
            matched <- expected[seq_len(k)]
            expect_identical(paste(actual$HEAD, actual$CHEST, actual$HANDS, actual$LEGS, sep = "|"), matched$KEY, info = paste("trial", trial))
            expect_identical(actual$SCORE_RAW, matched$SCORE, info = paste("trial", trial))
            expect_equal(actual$TOTAL_POISE, matched$POISE + 40*args$wolf.ring, info = paste("trial", trial))
            expect_equal(actual$DURABILITY, matched$DURABILITY, info = paste("trial", trial))
            expect_identical(actual$ARMOR_WEIGHT, matched$ARMOR32, info = paste("trial", trial))
            expect_identical(actual$TOTAL_WEIGHT, matched$TOTAL, info = paste("trial", trial))
            expect_identical(actual$EQUIP_LOAD, matched$LOAD, info = paste("trial", trial))
            expect_equal(actual$PCT_LOAD, matched$TOTAL/matched$LOAD, info = paste("trial", trial))
        }

    }

    expect_gte(heap.filled, 10)
    expect_gte(father.mask.returned, 5)
    expect_gte(cutoff.ties, 3)

})

## The result's columns are written out by hand in three places: the C++ DataFrame::create, the
## two empty-result tables get.optimal.armor.combos returns early with, and the app's initial
## empty table (inst/shiny/server.R). A search with no results must still have exactly the
## columns, order, and types of a normal one.
test_that("empty results and the app's initial table match a normal result's columns", {
    column.types <- function(dt){ vapply(dt, function(x) class(x)[1], character(1)) }
    normal <- get.optimal.armor.combos(max.table.size = 5)$data
    expect_gt(nrow(normal), 0)

    ## No head piece left after filtering: Mask of the Father needs areas that aren't completed
    no.pieces <- get.optimal.armor.combos(head.filter = "Mask of the Father", areas.completed = character(0))$data
    ## Pieces left, but no combination can meet the minima
    no.combos <- get.optimal.armor.combos(minima = c(999, rep(0, 11)))$data
    for(empty in list(no.pieces, no.combos)){
        expect_equal(nrow(empty), 0)
        expect_identical(column.types(empty), column.types(normal))
    }

    ## The app shows the four armor columns as factors (for its column filters)
    app.normal <- data.table::copy(normal)[, c("HEAD", "CHEST", "HANDS", "LEGS") := lapply(.SD, as.factor), .SDcols = c("HEAD", "CHEST", "HANDS", "LEGS")]
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        expect_identical(column.types(armordata()$data), column.types(app.normal))
    })
})

test_that("named minima/weights match their positional equivalents in any order", {
    positional <- get.optimal.armor.combos(
        max.table.size = 50, endurance.level = 40, movement = "Mid",
        minima = c(0, 0, 0, 0, 20, 0, 0, 30, 0, 0, 0, 200),
        weights = c(2, 0, 0, 0, 1, 0, 0, 0.5, 0, 0)
    )
    named <- get.optimal.armor.combos(
        max.table.size = 50, endurance.level = 40, movement = "Mid",
        minima = c(DURABILITY = 200, POISE = 30, MAG_DEF = 20),
        weights = c(BLEED_RES = 0.5, MAG_DEF = 1, PHYS_DEF = 2)
    )
    expect_identical(named$data, positional$data)
    ## args stay unnamed, in positional order (the app compares them with identical())
    expect_identical(named$args$minima, positional$args$minima)
    expect_identical(named$args$weights, positional$args$weights)
    expect_null(names(named$args$weights))

    ## A full named vector in shuffled order
    w <- c(PHYS_DEF = 0.16, STRIKE_DEF = 0.16, SLASH_DEF = 0.16, THRUST_DEF = 0.16, MAG_DEF = 0.08, FIRE_DEF = 0.08, LITNG_DEF = 0.08, BLEED_RES = 0.04, POIS_RES = 0.04, CURSE_RES = 0.04)
    expect_identical(get.optimal.armor.combos(max.table.size = 50, weights = rev(w))$data, get.optimal.armor.combos(max.table.size = 50)$data)
})
