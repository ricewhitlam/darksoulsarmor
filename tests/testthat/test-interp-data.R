## get.interp.data computes a slot table at any upgrade level from +0, as the game does: each
## defense and resistance times that level's rate, in 32-bit floating point. (test-data.R checks it
## against the displayed values at max upgrade; these tests cover the levels in between.)
slot.tables <- list(head = head.data.unupgraded, chest = chest.data.unupgraded, hands = hands.data.unupgraded, legs = legs.data.unupgraded)
stat.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")

test_that("level 0 is the +0 table itself", {
    for(slot in names(slot.tables)){
        u <- slot.tables[[slot]]
        expect_equal(darksoulsarmor:::get.interp.data(u, 0, 0), u[order(ARMOR)], tolerance = 0, info = slot)
    }
})

test_that("upgrades change only upgradeable pieces' defenses and resistances, never downward", {
    float32 <- darksoulsarmor:::float32
    for(slot in names(slot.tables)){
        u <- slot.tables[[slot]][order(ARMOR)]
        none <- u$UPGRADE_TYPE == "None"
        previous <- u
        for(level in 1:10){
            d <- darksoulsarmor:::get.interp.data(u, level, min(level, 5))
            info <- paste(slot, level)
            ## Everything else, and pieces that can't be upgraded, stay as they are at +0
            expect_equal(d[, setdiff(names(d), stat.cols), with = FALSE], u[, setdiff(names(u), stat.cols), with = FALSE], tolerance = 0, info = info)
            expect_equal(d[none], u[none], tolerance = 0, info = info)
            for(col in stat.cols){
                expect_true(all(float32(d[[col]]) == d[[col]]), info = paste(info, col))
                expect_true(all(d[[col]] >= previous[[col]]), info = paste(info, col))
            }
            previous <- d
        }
    }
})

## Values read in-game, within half a display step (plus 32-bit noise) like test-data.R's check.
## Strike at +1 depends on the exact +0 value: Black Sorcerer Hat's strike is 5 x 103% = 5.1499996,
## shown as 5.1, and 5.1499996 x 1.1 shows as 5.7 where 5.1 x 1.1 would show 5.6. Knight Armor's
## (37 x 95% = 35.1499977) likewise shows 38.7, not 38.6.
test_that("upgraded values match values read in-game", {
    at <- function(table, armor, col, level){ darksoulsarmor:::get.interp.data(table, level, 0)[ARMOR == armor][[col]] }
    hat.physical <- c(5.0, 5.5, 6.0, 6.5, 7.1, 7.8, 8.5, 9.2, 10.1, 10.9, 12.1)
    for(level in 0:10){
        expect_lte(abs(at(head.data.unupgraded, "Black Sorcerer Hat", "PHYS_DEF", level) - hat.physical[level+1]), 0.05 + 1e-5)
    }
    expect_lte(abs(at(head.data.unupgraded, "Black Sorcerer Hat", "STRIKE_DEF", 1) - 5.7), 0.05 + 1e-5)
    expect_lte(abs(at(chest.data.unupgraded, "Knight Armor", "STRIKE_DEF", 1) - 38.7), 0.05 + 1e-5)
})

## Paladin Armor (twinkling) at +0 through +5, as the wiki lists it. Its stats are large enough that
## a rate off by 0.01 shows (Black Sorcerer Hat's physical 5 can't: 5 x 1.43 and 5 x 1.42 both show
## 7.1). Two wiki values are left out as typos: thrust +4 is listed as 83.0, but thrust equals
## physical at every other level (82.0 here), and curse +3 as 64.5, where 57 x 1.14 = 64.98.
test_that("upgraded values match Paladin Armor's listed values at every twinkling level", {
    cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
    listed <- rbind(
        c(59.0, 56.1, 67.9, 59.0, 31.0, 37.0, 19.0, 34.0, 24.0, 57.0),
        c(63.7, 60.5, 73.3, 63.7, 33.5, 40.0, 20.5, 35.7, 25.2, 59.8),
        c(70.2, 66.7, 80.7, 70.2, 36.9, 44.0, 22.6, 37.1, 26.2, 62.1),
        c(76.1, 72.3, 87.5, 76.1, 40.0, 47.7, 24.5, 38.8, 27.4, NA),
        c(82.0, 77.9, 94.3, NA, 43.1, 51.4, 26.4, 40.1, 28.3, 67.3),
        c(91.5, 86.9, 105.2, 91.5, 48.1, 57.4, 29.5, 43.2, 30.5, 72.4)
    )
    for(level in 0:5){
        exact <- unlist(darksoulsarmor:::get.interp.data(chest.data.unupgraded, 0, level)[ARMOR == "Paladin Armor", ..cols])
        far <- !is.na(listed[level+1, ]) & abs(exact - listed[level+1, ]) > 0.05 + 1e-5
        expect_false(any(far), info = paste0("+", level, " ", paste(cols[far], collapse = ", ")))
    }
})

## The game's own rates (its ReinforceParamProtector table), pinned so an accidental edit can't
## pass unnoticed: small bases hide a slightly wrong rate at the regular levels between +0 and +10
test_that("the upgrade rates are the game's", {
    expect_identical(darksoulsarmor:::REGULAR.DEF.RATES, c(1, 1.1, 1.2, 1.3, 1.43, 1.56, 1.69, 1.85, 2.01, 2.17, 2.42))
    expect_identical(darksoulsarmor:::REGULAR.RES.RATES, c(1, 1, 1, 1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.4))
    expect_identical(darksoulsarmor:::TWINKLING.DEF.RATES, c(1, 1.08, 1.19, 1.29, 1.39, 1.55))
    expect_identical(darksoulsarmor:::TWINKLING.RES.RATES, c(1, 1.05, 1.09, 1.14, 1.18, 1.27))
})
