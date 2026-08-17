## Brute-force reference test: enumerates every combination directly over a small filtered
## subset and applies the same weight/minima constraints and scoring formula, independent of
## the shell-expansion and priority-queue machinery that get.optimal.armor.combos and
## optimal_armor_combinations use internally. This is the test that would have caught the
## minima-indexing bug fixed alongside this test suite (see git history for
## R/optimal-combos.R): a wrong index into `minima` there can inflate the starting
## shell size and cause the C++ search to silently skip feasible combinations.
test_that("get.optimal.armor.combos matches a brute-force reference over a small filtered subset", {

    set.seed(20260814)
    pool.head <- setdiff(head.data_00$ARMOR, "Mask of the Father")
    head.sel <- sample(pool.head, 5)
    chest.sel <- sample(chest.data_00$ARMOR, 5)
    hands.sel <- sample(hands.data_00$ARMOR, 5)
    legs.sel <- sample(legs.data_00$ARMOR, 5)

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
    h <- head.data_00[ARMOR %in% head.sel]
    c <- chest.data_00[ARMOR %in% chest.sel]
    g <- hands.data_00[ARMOR %in% hands.sel]
    l <- legs.data_00[ARMOR %in% legs.sel]

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
