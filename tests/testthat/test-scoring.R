## The internal statistics in R/sysdata.rda (means/stddevs/corrs, total.combo.count, and
## mean.stddev.corr.list) are derived by create_rda.R from the same armor tables shipped as
## data/*.rda. Every combination counts once in those populations, so each slot's piece is
## independent of the others and the statistics of a combination's summed metrics are just the
## sums of each slot table's own means and covariances - cheap to recompute here from the shipped
## tables, independently of create_rda.R, to catch the two files drifting apart. The remaining
## tests check that the score normalization is algebraically correct for any weights, and that
## get.optimal.armor.combos actually ranks combos in the direction its weights imply.

scored.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
interp <- function(unupgraded, reg, twink){ darksoulsarmor:::get.interp.data(unupgraded, reg, twink) }

## create_rda.R's pooled population for one slot: non-upgradeable pieces once, Regular pieces at
## each of +0-+10, Twinkling pieces at each of +0-+5
pooled.slot <- function(unupgraded){
    out <- unupgraded[UPGRADE_TYPE == "None"]
    for(reg in 0:10){
        out <- rbind(out, interp(unupgraded[UPGRADE_TYPE == "Regular"], reg, 0))
    }
    for(twink in 0:5){
        out <- rbind(out, interp(unupgraded[UPGRADE_TYPE == "Twinkling"], 0, twink))
    }
    out
}

## Population mean/sd/corr of the summed metrics over every head x chest x hands x legs combination
closed.form.stats <- function(head, chest, hands, legs){
    slot.means <- function(d){ colMeans(as.matrix(d[, ..scored.cols])) }
    slot.covar <- function(d){ m <- as.matrix(d[, ..scored.cols]); crossprod(sweep(m, 2, colMeans(m)))/nrow(m) }
    covar <- slot.covar(head) + slot.covar(chest) + slot.covar(hands) + slot.covar(legs)
    stddevs <- sqrt(diag(covar))
    list(means = slot.means(head) + slot.means(chest) + slot.means(hands) + slot.means(legs), stddevs = stddevs, corrs = covar/outer(stddevs, stddevs))
}

pooled.head <- pooled.slot(head.data.unupgraded)
pooled.chest <- pooled.slot(chest.data.unupgraded)
pooled.hands <- pooled.slot(hands.data.unupgraded)
pooled.legs <- pooled.slot(legs.data.unupgraded)

test_that("the shipped pooled statistics match the shipped armor data", {
    expected <- closed.form.stats(pooled.head, pooled.chest, pooled.hands, pooled.legs)
    expect_equal(darksoulsarmor:::means, expected$means, tolerance = 1e-10)
    expect_equal(darksoulsarmor:::stddevs, expected$stddevs, tolerance = 1e-10)
    expect_equal(darksoulsarmor:::corrs, expected$corrs, tolerance = 1e-10)
    expect_identical(darksoulsarmor:::total.combo.count, as.numeric(nrow(pooled.head))*nrow(pooled.chest)*nrow(pooled.hands)*nrow(pooled.legs))
})

test_that("the shipped per-upgrade-level statistics match the shipped armor data", {
    level.list <- darksoulsarmor:::mean.stddev.corr.list
    expect_named(level.list, c(as.vector(t(outer(0:10, 0:5, paste, sep = "_"))), "overall"))
    for(reg in 0:10){
        for(twink in 0:5){
            key <- paste0(reg, "_", twink)
            expected <- closed.form.stats(
                interp(head.data.unupgraded, reg, twink),
                interp(chest.data.unupgraded, reg, twink),
                interp(hands.data.unupgraded, reg, twink),
                interp(legs.data.unupgraded, reg, twink)
            )
            expect_equal(level.list[[key]], expected, tolerance = 1e-10, info = key)
        }
    }
    expect_identical(level.list[["overall"]], list(means = darksoulsarmor:::means, stddevs = darksoulsarmor:::stddevs, corrs = darksoulsarmor:::corrs))
})

## The real-population version of create_rda.R's test.meansd(): with the shipped statistics, the
## score itself has population mean 0 and standard deviation 1 for any weights. The score is a
## sum over slots, so its mean and variance are the sums of each slot's own.
test_that("scores have population mean 0 and standard deviation 1 for arbitrary weights", {
    means <- darksoulsarmor:::means
    stddevs <- darksoulsarmor:::stddevs
    corrs <- darksoulsarmor:::corrs
    set.seed(20261008)
    for(trial in 1:5){
        w <- runif(10)
        w <- w/sum(w)
        score.scalars <- w/(stddevs*sqrt((t(w) %*% corrs %*% w)[1, 1]))
        slot.scores <- lapply(list(pooled.head, pooled.chest, pooled.hands, pooled.legs), function(d){ drop(sweep(as.matrix(d[, ..scored.cols]), 2, 0.25*means) %*% score.scalars) })
        expect_equal(sum(sapply(slot.scores, mean)), 0, tolerance = 1e-10)
        expect_equal(sqrt(sum(sapply(slot.scores, function(s){ mean((s - mean(s))^2) }))), 1, tolerance = 1e-10)
    }
})

test_that("score normalization gives unit variance for arbitrary weights", {
    ## means/stddevs/corrs are internal (R/sysdata.rda), so accessed here via :::.
    stddevs <- darksoulsarmor:::stddevs
    corrs <- darksoulsarmor:::corrs
    Sigma <- diag(stddevs) %*% corrs %*% diag(stddevs)

    set.seed(2026)
    for(trial in 1:5){
        w <- runif(10)
        w <- w / sum(w)
        score.scalars <- w / (stddevs * sqrt((t(w) %*% corrs %*% w)[1, 1]))
        implied.var <- (t(score.scalars) %*% Sigma %*% score.scalars)[1, 1]
        expect_equal(implied.var, 1, tolerance = 1e-8)
    }
})

test_that("get.optimal.armor.combos ranks combos in the direction its weights imply", {
    ## Guardian Helm has much higher PHYS_DEF than Big Hat; Big Hat has much higher MAG_DEF.
    head.choices <- c("Guardian Helm", "Big Hat")

    phys.result <-
        get.optimal.armor.combos(
            max.table.size = 10,
            head.filter = head.choices,
            chest.filter = chest.data.unupgraded$ARMOR[1],
            hands.filter = hands.data.unupgraded$ARMOR[1],
            legs.filter = legs.data.unupgraded$ARMOR[1],
            movement = "Fat",
            weights = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        )$data

    mag.result <-
        get.optimal.armor.combos(
            max.table.size = 10,
            head.filter = head.choices,
            chest.filter = chest.data.unupgraded$ARMOR[1],
            hands.filter = hands.data.unupgraded$ARMOR[1],
            legs.filter = legs.data.unupgraded$ARMOR[1],
            movement = "Fat",
            weights = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
        )$data

    expect_equal(phys.result$HEAD[1], "Guardian Helm")
    expect_equal(mag.result$HEAD[1], "Big Hat")
})

## stat.scale: under "sd" the multipliers are the original formula, bit for bit; under every scale
## the score keeps population mean 0 and standard deviation 1 (checked on the real population,
## as above)
test_that("every stat.scale gives scores with population mean 0 and standard deviation 1", {
    means <- darksoulsarmor:::means
    stddevs <- darksoulsarmor:::stddevs
    corrs <- darksoulsarmor:::corrs
    set.seed(20261103)
    for(trial in 1:3){
        w <- runif(10)
        w <- w/sum(w)
        expect_identical(darksoulsarmor:::score.scalars.for(w, "sd", "individual"), w/(stddevs*sqrt((t(w) %*% corrs %*% w)[1, 1])))
        for(stat.scale in c("sd", "max", "raw")){
            score.scalars <- darksoulsarmor:::score.scalars.for(w, stat.scale, "individual")
            slot.scores <- lapply(list(pooled.head, pooled.chest, pooled.hands, pooled.legs), function(d){ drop(sweep(as.matrix(d[, ..scored.cols]), 2, 0.25*means) %*% score.scalars) })
            expect_equal(sum(sapply(slot.scores, mean)), 0, tolerance = 1e-10, info = stat.scale)
            expect_equal(sqrt(sum(sapply(slot.scores, function(s){ mean((s - mean(s))^2) }))), 1, tolerance = 1e-10, info = stat.scale)
        }
    }
})

## What each scale means: weights w on raw points rank exactly like w times each stat's standard
## deviation on standard deviations, and weights w on best possible totals exactly like w divided
## by each stat's best total on raw points - the same sets, in the same order, with the same scores
test_that("stat.scale weights are weights per standard deviation, per best total, or per point", {
    stddevs <- darksoulsarmor:::stddevs
    stat.maxes <- darksoulsarmor:::stat.maxes
    search <- function(w, stat.scale){
        get.optimal.armor.combos(max.table.size = 50, endurance.level = 40, movement = "Fat", regular.level = "+5", weights = w, stat.scale = stat.scale)
    }
    same <- function(a, b){
        expect_identical(a$data[, .(HEAD, CHEST, HANDS, LEGS)], b$data[, .(HEAD, CHEST, HANDS, LEGS)])
        expect_equal(a$data$SCORE, b$data$SCORE, tolerance = 1e-10)
        expect_identical(a$data$SCORE_QUALITY, b$data$SCORE_QUALITY)
    }
    set.seed(31)
    for(trial in 1:3){
        w <- runif(10)
        same(search(w, "raw"), search(w*stddevs, "sd"))
        same(search(w, "max"), search(w/stat.maxes, "raw"))
    }
    ## The scale is recorded with the other arguments, "sd" by default
    expect_identical(search(w, "max")$args$stat.scale, "max")
    expect_identical(get.optimal.armor.combos(max.table.size = 1, weights = default.weights)$args$stat.scale, "sd")
    ## ...and passed through by get.armor.tradeoffs
    curve <- get.armor.tradeoffs(max.armor.weight = 3, weights = w, stat.scale = "raw")$data
    expect_equal(curve$SCORE, get.armor.tradeoffs(max.armor.weight = 3, weights = w*stddevs, stat.scale = "sd")$data$SCORE, tolerance = 1e-10)
})

## weights.mode = "grouped": each group counts as the average of its stats, in units of that
## average - its standard deviation over every combination (here computed straight from the real
## population, slot by slot), its best possible total, or a point. So group weights W rank exactly
## like individual raw weights of W/(k x unit) on each of a group's k stats.
test_that("grouped weights weight each group's average stat, in units of that average", {
    stat.groups <- darksoulsarmor:::stat.groups
    group.maxes <- darksoulsarmor:::group.maxes
    population.sd <- function(stats){
        slot.vars <- sapply(list(pooled.head, pooled.chest, pooled.hands, pooled.legs), function(d){ a <- rowMeans(as.matrix(d[, stats, with = FALSE])); mean((a - mean(a))^2) })
        sqrt(sum(slot.vars))
    }
    search <- function(w, stat.scale, weights.mode){
        get.optimal.armor.combos(max.table.size = 50, endurance.level = 40, movement = "Fat", regular.level = "+5", weights = w, stat.scale = stat.scale, weights.mode = weights.mode)
    }
    set.seed(47)
    for(stat.scale in c("sd", "max", "raw")){
        W <- setNames(runif(3), names(stat.groups))
        per.point <- setNames(rep(0, length(scored.cols)), scored.cols)
        for(g in names(stat.groups)){
            unit <- switch(stat.scale, sd = population.sd(stat.groups[[g]]), max = group.maxes[[g]], raw = 1)
            per.point[stat.groups[[g]]] <- W[[g]]/(length(stat.groups[[g]])*unit)
        }
        grouped <- search(W, stat.scale, "grouped")
        individual <- search(per.point, "raw", "individual")
        ## Grouping makes exact ties common (e.g. magic 135 + fire 159 = 129 + 165, the same
        ## elemental average), and tied sets fall back to their pieces' order, which the two
        ## derivations can settle differently in the last bit: so compare the sets within each tie,
        ## leaving out a tie the table's last row may cut short
        sets <- function(d){
            d <- d[, .(TIE = round(SCORE, 8), HEAD, CHEST, HANDS, LEGS)]
            d <- d[TIE != TIE[.N]]
            d[order(-TIE, HEAD, CHEST, HANDS, LEGS)]
        }
        expect_identical(sets(grouped$data), sets(individual$data), info = stat.scale)
        expect_equal(grouped$data$SCORE, individual$data$SCORE, tolerance = 1e-8, info = stat.scale)
        expect_identical(grouped$data$SCORE_QUALITY, individual$data$SCORE_QUALITY, info = stat.scale)
        expect_identical(grouped$args$weights.mode, "grouped")
        expect_equal(grouped$args$weights, unname(W/sum(W)))
    }
    ## Named in any order, or unnamed in the groups' order; individual by default
    expect_identical(search(c(RESISTANCES = 1, PHYSICAL_DEFENSES = 2), "sd", "grouped")$data, search(c(2, 0, 1), "sd", "grouped")$data)
    expect_identical(get.optimal.armor.combos(max.table.size = 1, weights = default.weights)$args$weights.mode, "individual")
    ## ...and passed through by get.armor.tradeoffs
    curve <- get.armor.tradeoffs(max.armor.weight = 3, weights = c(1, 2, 3), weights.mode = "grouped")$data
    expect_equal(curve$BEST_VALUE, curve$SCORE)
    expect_false(isTRUE(all.equal(curve$SCORE, get.armor.tradeoffs(max.armor.weight = 3, weights = default.weights)$data$SCORE)))
})

## Grouped scores keep population mean 0 and standard deviation 1 under every scale
test_that("grouped weights give scores with population mean 0 and standard deviation 1", {
    means <- darksoulsarmor:::means
    set.seed(53)
    for(stat.scale in c("sd", "max", "raw")){
        W <- runif(3)
        score.scalars <- darksoulsarmor:::score.scalars.for(W/sum(W), stat.scale, "grouped")
        slot.scores <- lapply(list(pooled.head, pooled.chest, pooled.hands, pooled.legs), function(d){ drop(sweep(as.matrix(d[, ..scored.cols]), 2, 0.25*means) %*% score.scalars) })
        expect_equal(sum(sapply(slot.scores, mean)), 0, tolerance = 1e-10, info = stat.scale)
        expect_equal(sqrt(sum(sapply(slot.scores, function(s){ mean((s - mean(s))^2) }))), 1, tolerance = 1e-10, info = stat.scale)
    }
})
