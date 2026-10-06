## The internal statistics in R/sysdata.rda (means/stddevs/corrs, total.combo.count, and
## mean.stddev.corr.list) are derived by create_rda.R from the same armor tables shipped as
## data/*.rda. Every combination counts once in those populations, so each slot's piece is
## independent of the others and the statistics of a combination's summed metrics are just the
## sums of each slot table's own means and covariances - cheap to recompute here from the shipped
## tables, independently of create_rda.R, to catch the two files drifting apart. The remaining
## tests check that the score normalization is algebraically correct for any weights, and that
## get.optimal.armor.combos actually ranks combos in the direction its weights imply.

scored.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
interp <- function(unupgraded, fullupgrade, reg, twink){ darksoulsarmor:::get.interp.data(unupgraded, fullupgrade, reg, twink) }

## create_rda.R's pooled population for one slot: non-upgradeable pieces once, Regular pieces at
## each of +0-+10, Twinkling pieces at each of +0-+5
pooled.slot <- function(unupgraded, fullupgrade){
    out <- unupgraded[UPGRADE_TYPE == "None"]
    for(reg in 0:10){
        out <- rbind(out, interp(unupgraded[UPGRADE_TYPE == "Regular"], fullupgrade[UPGRADE_TYPE == "Regular"], reg, 0))
    }
    for(twink in 0:5){
        out <- rbind(out, interp(unupgraded[UPGRADE_TYPE == "Twinkling"], fullupgrade[UPGRADE_TYPE == "Twinkling"], 0, twink))
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

pooled.head <- pooled.slot(head.data.unupgraded, head.data.fullupgrade)
pooled.chest <- pooled.slot(chest.data.unupgraded, chest.data.fullupgrade)
pooled.hands <- pooled.slot(hands.data.unupgraded, hands.data.fullupgrade)
pooled.legs <- pooled.slot(legs.data.unupgraded, legs.data.fullupgrade)

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
                interp(head.data.unupgraded, head.data.fullupgrade, reg, twink),
                interp(chest.data.unupgraded, chest.data.fullupgrade, reg, twink),
                interp(hands.data.unupgraded, hands.data.fullupgrade, reg, twink),
                interp(legs.data.unupgraded, legs.data.fullupgrade, reg, twink)
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
            roll = "Fat",
            weights = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        )$data

    mag.result <-
        get.optimal.armor.combos(
            max.table.size = 10,
            head.filter = head.choices,
            chest.filter = chest.data.unupgraded$ARMOR[1],
            hands.filter = hands.data.unupgraded$ARMOR[1],
            legs.filter = legs.data.unupgraded$ARMOR[1],
            roll = "Fat",
            weights = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
        )$data

    expect_equal(phys.result$HEAD[1], "Guardian Helm")
    expect_equal(mag.result$HEAD[1], "Big Hat")
})
