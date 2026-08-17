## The shipped means/stddevs/corrs were fit by create_rda.R over the full cross-upgrade-level
## armor population, which isn't reconstructible at package runtime (it isn't shipped, and
## regenerating it here would mean materializing a table far larger than the one
## get.all.armor.combos already warns is ~2GB for a single upgrade level). So rather than
## reproducing create_rda.R's population-level test.meansd() check, these tests cover what is
## checkable from the shipped data: that the score normalization is algebraically correct for
## any weights, and that get.optimal.armor.combos actually ranks combos in the direction its
## weights imply.

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
            chest.filter = chest.data_00$ARMOR[1],
            hands.filter = hands.data_00$ARMOR[1],
            legs.filter = legs.data_00$ARMOR[1],
            roll = "Fat",
            weights = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        )$data

    mag.result <-
        get.optimal.armor.combos(
            max.table.size = 10,
            head.filter = head.choices,
            chest.filter = chest.data_00$ARMOR[1],
            hands.filter = hands.data_00$ARMOR[1],
            legs.filter = legs.data_00$ARMOR[1],
            roll = "Fat",
            weights = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
        )$data

    expect_equal(phys.result$HEAD[1], "Guardian Helm")
    expect_equal(mag.result$HEAD[1], "Big Hat")
})
