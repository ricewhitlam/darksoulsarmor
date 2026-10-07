## get.armor.tradeoffs: the best achievable value of a stat at each armor-weight limit. Checked
## against brute force over small random piece subsets, for every metric, with the Mask of the
## Father (with its own equip load bonus), weapons, rings, minima, and upgrade levels in
## play - which also exercises the search's ranking by a single stat, and the curve's reuse of one
## limit's best combination at lower limits.
test_that("get.armor.tradeoffs matches brute force for every metric", {

    set.seed(20261010)
    means <- darksoulsarmor:::means
    stddevs <- darksoulsarmor:::stddevs
    corrs <- darksoulsarmor:::corrs
    scored.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
    metrics <- c("SCORE", "POISE", scored.cols)

    for(trial in seq_along(metrics)){
        metric <- metrics[trial]
        head.sel <- union(sample(head.data.unupgraded$ARMOR, sample(2:8, 1)), if(runif(1) < 0.6) "Mask of the Father")
        args <- list(
            head.filter = head.sel,
            chest.filter = sample(chest.data.unupgraded$ARMOR, sample(2:8, 1)),
            hands.filter = sample(hands.data.unupgraded$ARMOR, sample(2:8, 1)),
            legs.filter = sample(legs.data.unupgraded$ARMOR, sample(2:8, 1)),
            regular.level = paste0("+", sample(0:10, 1)),
            twinkling.level = paste0("+", sample(0:5, 1)),
            movement = sample(c("Light", "Mid", "Fat", "Poop"), 1),
            endurance.level = sample(10:50, 1),
            weapons = { slots <- sample(c("left.1", "right.1", "left.2", "right.2"), sample(0:3, 1)); stats::setNames(sample(c(weapon.data$WEAPON, rep("Talisman", 10)), length(slots)), slots) },
            havel.ring = runif(1) < 0.3,
            wolf.ring = runif(1) < 0.4,
            minima = c(POISE = sample(c(0, 0, 10), 1), BLEED_RES = sample(c(0, 0, 5), 1)),
            weights = runif(10) + 0.01
        )
        step <- sample(c(1, 2.5), 1)
        actual <- do.call(get.armor.tradeoffs, c(list(metric = metric, weight.step = step), args))$data

        ## Brute force over every combination of the selected pieces
        reg <- as.numeric(args$regular.level)
        twink <- as.numeric(args$twinkling.level)
        slot <- function(u, sel){ darksoulsarmor:::get.interp.data(u, reg, twink)[ARMOR %in% sel] }
        h <- slot(head.data.unupgraded, args$head.filter)
        c <- slot(chest.data.unupgraded, args$chest.filter)
        g <- slot(hands.data.unupgraded, args$hands.filter)
        l <- slot(legs.data.unupgraded, args$legs.filter)
        grid <- data.table::CJ(H = seq_len(nrow(h)), C = seq_len(nrow(c)), G = seq_len(nrow(g)), L = seq_len(nrow(l)))
        for(col in c(scored.cols, "POISE", "WEIGHT")){
            data.table::set(grid, j = col, value = h[[col]][grid$H] + c[[col]][grid$C] + g[[col]][grid$G] + l[[col]][grid$L])
        }
        w <- args$weights/sum(args$weights)
        score.scalars <- w/(stddevs*sqrt((t(w) %*% corrs %*% w)[1, 1]))
        grid[, SCORE := 0]
        for(i in seq_along(scored.cols)){
            grid[, SCORE := SCORE + score.scalars[i]*(get(scored.cols[i]) - means[i])]
        }
        grid[, VALUE := if(metric == "SCORE") SCORE else if(metric == "POISE") POISE + 40*args$wolf.ring else get(metric)]
        grid[, MASK := h$ARMOR[H] == "Mask of the Father"]
        grid <- grid[POISE + 40*args$wolf.ring >= args$minima[["POISE"]] - 1e-10 & BLEED_RES >= args$minima[["BLEED_RES"]] - 1e-10]

        ## The game's movement check, written out (see R/equip-load.R): the weapons in slot order,
        ## then head, chest, hands, legs, each added in 32-bit, at or below the 32-bit line - the Mask
        ## of the Father's own when it's worn
        f32 <- darksoulsarmor:::float32
        rate <- if(args$havel.ring) f32(1.5) else 1
        load <- f32((args$endurance.level + 40)*rate)
        load.father.mask <- f32((args$endurance.level + 40)*f32(rate*f32(1.05)))
        share <- c(Light = 0.25, Mid = 0.5, Fat = 1, Poop = Inf)[[args$movement]]
        carried <- 0
        for(s in c("left.1", "right.1", "left.2", "right.2")){
            if(s %in% names(args$weapons)){ carried <- f32(carried + f32(weapon.data[WEAPON == args$weapons[[s]], WEIGHT])) }
        }
        total <- f32(f32(f32(f32(carried + f32(h$WEIGHT[grid$H])) + f32(c$WEIGHT[grid$C])) + f32(g$WEIGHT[grid$G])) + f32(l$WEIGHT[grid$L]))
        line <- if(is.infinite(share)) Inf else f32(load*share)
        line.father.mask <- if(is.infinite(share)) Inf else f32(load.father.mask*share)
        grid <- grid[total <= ifelse(MASK, line.father.mask, line)]

        ## A limit is on the armor's listed weight
        expected <- sapply(actual$ARMOR_WEIGHT_LIMIT, function(limit){
            fits <- grid[WEIGHT <= limit + 1e-9]
            if(nrow(fits) == 0) NA_real_ else max(fits$VALUE)
        })
        ## Among the combinations tied on the best value, the best-scoring one is chosen
        expected.score <- sapply(actual$ARMOR_WEIGHT_LIMIT, function(limit){
            fits <- grid[WEIGHT <= limit + 1e-9]
            if(nrow(fits) == 0) NA_real_ else max(fits[VALUE >= max(VALUE) - 1e-6, SCORE])
        })
        info <- paste("trial", trial, metric)
        expect_equal(actual$BEST_VALUE, expected, tolerance = 1e-9, info = info)
        expect_equal(actual$SCORE_RAW, expected.score, tolerance = 1e-6, info = info)

        ## Limits run from 0 to the most armor weight the movement type could allow (its line, with
        ## the Mask of the Father's bonus, less the weapons, on the 0.1 grid) in the requested steps
        heaviest <- max(head.data.unupgraded$WEIGHT) + max(chest.data.unupgraded$WEIGHT) + max(hands.data.unupgraded$WEIGHT) + max(legs.data.unupgraded$WEIGHT)
        allowance <- min(floor(10*(line.father.mask - carried) + 1e-6)/10, heaviest)
        expect_equal(range(actual$ARMOR_WEIGHT_LIMIT), c(0, max(0, allowance)), info = info)
        expect_true(all(diff(actual$ARMOR_WEIGHT_LIMIT) <= step + 1e-9), info = info)

        ## Each reported combination's listed weight is within its limit
        ok <- !is.na(actual$BEST_VALUE)
        listed <- head.data.unupgraded$WEIGHT[match(actual$HEAD, head.data.unupgraded$ARMOR)] + chest.data.unupgraded$WEIGHT[match(actual$CHEST, chest.data.unupgraded$ARMOR)] +
            hands.data.unupgraded$WEIGHT[match(actual$HANDS, hands.data.unupgraded$ARMOR)] + legs.data.unupgraded$WEIGHT[match(actual$LEGS, legs.data.unupgraded$ARMOR)]
        expect_true(all(listed[ok] <= actual$ARMOR_WEIGHT_LIMIT[ok] + 1e-9), info = info)
    }
})

test_that("the SCORE curve's top point is get.optimal.armor.combos' best result", {
    curve <- get.armor.tradeoffs(endurance.level = 40, movement = "Mid", weapons = c(right.1 = "Great Club"))$data
    best <- get.optimal.armor.combos(max.table.size = 1, endurance.level = 40, movement = "Mid", weapons = c(right.1 = "Great Club"))$data
    top <- curve[.N]
    ## The Mask of the Father's line (42) less the Great Club (12)
    expect_equal(top$ARMOR_WEIGHT_LIMIT, 30)
    expect_equal(top$BEST_VALUE, best$SCORE_RAW)
    expect_equal(c(top$HEAD, top$CHEST, top$HANDS, top$LEGS), c(best$HEAD, best$CHEST, best$HANDS, best$LEGS))
    expect_equal(top$SCORE_QUALITY, best$SCORE_QUALITY)
    ## Never worse with more weight allowed
    expect_false(is.unsorted(curve$BEST_VALUE[!is.na(curve$BEST_VALUE)]))
})

test_that("get.armor.tradeoffs validates its own arguments", {
    expect_error(get.armor.tradeoffs(metric = "DURABILITY"), "metric")
    expect_error(get.armor.tradeoffs(metric = c("SCORE", "POISE")), "metric")
    expect_error(get.armor.tradeoffs(weight.step = 0), "weight.step")
    expect_error(get.armor.tradeoffs(weight.step = -1), "weight.step")
    expect_error(get.armor.tradeoffs(max.armor.weight = -5), "max.armor.weight")
    expect_error(get.armor.tradeoffs(max.table.size = 10), "max.table.size")
    expect_error(get.armor.tradeoffs(movement = "Sprint"), "movement")
    ## An explicit max.armor.weight and an arbitrary step
    curve <- get.armor.tradeoffs(metric = "POISE", weight.step = 0.25, max.armor.weight = 3)$data
    expect_equal(curve$ARMOR_WEIGHT_LIMIT, seq(0, 3, by = 0.25))
})

test_that("a curve over part of the weight range matches the full curve there", {
    settings <- list(metric = "POISE", endurance.level = 40, movement = "Mid", weapons = c(right.1 = "Great Club"), wolf.ring = TRUE)
    full <- do.call(get.armor.tradeoffs, c(settings, list(weight.step = 0.1, max.armor.weight = 12)))$data
    part <- do.call(get.armor.tradeoffs, c(settings, list(weight.step = 0.5, min.armor.weight = 3.2, max.armor.weight = 12)))$data
    ## Both ends plus the multiples of the step between them
    expect_equal(part$ARMOR_WEIGHT_LIMIT, c(3.2, seq(3.5, 12, by = 0.5)))
    ## The best combination at each limit is unique (the tie-break is a total order), so the same
    ## rows come back however the range was split
    expect_equal(part, full[match(round(part$ARMOR_WEIGHT_LIMIT, 9), round(full$ARMOR_WEIGHT_LIMIT, 9))])
    ## A single limit
    one <- do.call(get.armor.tradeoffs, c(settings, list(min.armor.weight = 7.3, max.armor.weight = 7.3)))$data
    expect_equal(one, full[round(ARMOR_WEIGHT_LIMIT, 9) == 7.3])

    expect_error(get.armor.tradeoffs(min.armor.weight = -1), "min.armor.weight")
    expect_error(get.armor.tradeoffs(min.armor.weight = c(1, 2)), "min.armor.weight")
    expect_error(get.armor.tradeoffs(min.armor.weight = 5, max.armor.weight = 4), "min.armor.weight.*larger than max.armor.weight")
})
