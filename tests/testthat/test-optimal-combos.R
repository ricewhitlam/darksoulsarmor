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

## SCORE_QUALITY expresses SCORE_RAW's normal-tail probability as "Top/Bottom K in N", using
## whichever tail SCORE_RAW actually sits in - chosen specifically because pnorm(SCORE_RAW)
## rounds to exactly 0 or 1 for the best (or, under tight constraints, least-bad) combinations
## get.optimal.armor.combos returns, which would make a plain percentile unable to distinguish
## them.
expected.score.quality <- function(score.raw){
    better <- score.raw >= 0
    p <- ifelse(better, pnorm(score.raw, lower.tail = FALSE), pnorm(score.raw, lower.tail = TRUE))
    n <- 10^ceiling(1-log10(p))
    k <- round(p*n)
    paste0(ifelse(better, "Top ", "Bottom "), format(k, big.mark = ",", scientific = FALSE, trim = TRUE), " in ", format(n, big.mark = ",", scientific = FALSE, trim = TRUE))
}

test_that("SCORE_QUALITY matches its formula and is correctly positioned as the second column", {
    ## Generous constraints (high endurance, both rings, loose roll) push SCORE_RAW well above
    ## average even for the best feasible combination, exercising the upper-tail branch.
    result <- get.optimal.armor.combos(max.table.size = 20, endurance.level = 99, havel.ring = TRUE, favor.ring = TRUE, roll = "Fat")$data
    expect_equal(names(result)[1:2], c("SCORE_RAW", "SCORE_QUALITY"))
    expect_equal(result$SCORE_QUALITY, expected.score.quality(result$SCORE_RAW))
    expect_true(all(result$SCORE_RAW > 0))
    expect_true(all(grepl("^Top ", result$SCORE_QUALITY)))
})

test_that("SCORE_QUALITY reads 'Bottom ... in ...' when even the best feasible combination scores below average", {
    ## The default constraints (endurance.level = 10, roll = "Fast") are tight enough that even
    ## the best feasible combination scores below the full population average, exercising the
    ## lower-tail branch.
    result <- get.optimal.armor.combos(max.table.size = 20)$data
    expect_true(all(result$SCORE_RAW < 0))
    expect_true(all(grepl("^Bottom ", result$SCORE_QUALITY)))
    expect_equal(result$SCORE_QUALITY, expected.score.quality(result$SCORE_RAW))
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
