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
    unarmored.weight <- 10
    endurance.level <- 40
    roll <- "Fat"

    actual <-
        get.optimal.armor.combos(
            max.table.size = 5000,
            head.filter = head.sel, chest.filter = chest.sel, hands.filter = hands.sel, legs.filter = legs.sel,
            roll = roll,
            unarmored.weight = unarmored.weight,
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

    base.load <- endurance.level + 40
    load.threshold <- base.load * 1.0 ## roll = "Fat"

    eps <- 1e-8
    ok <-
        (grid$WEIGHT <= (-unarmored.weight + load.threshold + eps)) &
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
    result <- get.optimal.armor.combos(max.table.size = 20, roll = "None", regular.level = "+10", twinkling.level = "+5")$data
    expect_equal(names(result)[1:2], c("SCORE_RAW", "SCORE_QUALITY"))
    expect_equal(result$SCORE_QUALITY[1], "Top 1 in 11,930,328")
    expect_true(all(grepl("^Top 1 in ", result$SCORE_QUALITY)))
})

test_that("SCORE_QUALITY reads 'Bottom 1 in N' when even the best feasible combination is in the bottom half", {
    ## The default constraints (endurance.level = 10, roll = "Fast") leave only 2.5 units of
    ## equip load for armor, so even the best feasible combination is a below-median one.
    result <- get.optimal.armor.combos(max.table.size = 20)$data
    expect_true(all(grepl("^Bottom 1 in ", result$SCORE_QUALITY)))
})

## EQUIP_LOAD is derived from (endurance.level+40)*ring multipliers, always exactly a multiple of
## 0.1 in true decimal arithmetic, but computed in floating point - which can land a hair below
## the true value (e.g. true 49.2 stored as 49.199999999999996). floor()ing that directly used to
## chop off a whole 0.1 (49.2 -> 49.1) instead of recovering the exact value; endurance.level=1
## with favor.ring=TRUE is one of the affected cases (true base.load = 41*1.2 = 49.2).
test_that("EQUIP_LOAD recovers the exact capacity despite floating-point noise", {
    result <- get.optimal.armor.combos(max.table.size = 50, endurance.level = 1, favor.ring = TRUE, roll = "Fast")
    non.motf <- result$data[HEAD != "Mask of the Father"]
    expect_true(nrow(non.motf) > 0)
    expect_equal(unique(non.motf$EQUIP_LOAD), 49.2)
    expect_true(all(non.motf$PCT_LOAD <= 0.25))
})
