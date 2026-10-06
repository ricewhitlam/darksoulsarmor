## Validates the shipped armor tables (data/*.rda, built by create_rda/create_rda.R from the
## create_rda/*.csv files). Mistakes in those CSVs mostly fail silently - e.g. a mistyped area name
## never matches a completed area, so the piece quietly becomes unobtainable - so these checks
## guard every later edit to the data.

slots <- list(
    head = list(unupgraded = head.data.unupgraded, fullupgrade = head.data.fullupgrade, none = "No Head"),
    chest = list(unupgraded = chest.data.unupgraded, fullupgrade = chest.data.fullupgrade, none = "No Chest"),
    hands = list(unupgraded = hands.data.unupgraded, fullupgrade = hands.data.fullupgrade, none = "No Hands"),
    legs = list(unupgraded = legs.data.unupgraded, fullupgrade = legs.data.fullupgrade, none = "No Legs")
)
def.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF")
res.cols <- c("BLEED_RES", "POIS_RES", "CURSE_RES")
meta.cols <- c("ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_MATCH_TYPE", "AREA_LIST", "LINK")

test_that("each slot's +0 and max tables list the same unique pieces with the same metadata", {
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        f <- slots[[slot]]$fullupgrade
        expect_false(anyDuplicated(u$ARMOR) > 0, info = slot)
        expect_identical(u[, ..meta.cols], f[, ..meta.cols], info = slot)
        expect_identical(names(u), names(f), info = slot)
        expect_false(anyNA(u), info = slot)
        expect_false(anyNA(f), info = slot)
    }
})

test_that("categorical columns only hold known values, and every area name matches `areas`", {
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        expect_true(all(u$UPGRADE_TYPE %in% c("None", "Regular", "Twinkling")), info = slot)
        expect_true(all(u$STARTING_CLASS %in% c(classes, "N/A")), info = slot)
        expect_true(all(grepl("^https?://", u$LINK) | u$LINK == "N/A"), info = slot)

        types <- strsplit(u$AREA_MATCH_TYPE, "|", fixed = TRUE)
        clauses <- lapply(u$AREA_LIST, function(x){ if(x == "") "" else strsplit(x, "|", fixed = TRUE)[[1]] })
        expect_true(all(unlist(types) %in% c("ALWAYS", "ANY", "ALL")), info = slot)
        expect_equal(lengths(types), lengths(clauses), info = slot)
        ## ALWAYS stands alone, with no areas listed
        always <- u$AREA_MATCH_TYPE == "ALWAYS"
        expect_true(all(u$AREA_LIST[always] == ""), info = slot)
        expect_false(any(grepl("ALWAYS", u$AREA_MATCH_TYPE[!always])), info = slot)
        ## A mistyped area name here would never match a completed area
        listed <- unlist(strsplit(unlist(clauses[!always]), ";", fixed = TRUE))
        expect_true(all(listed %in% areas), info = paste(slot, paste(setdiff(listed, areas), collapse = ", ")))
    }
    ## ...and a mistyped entry in `areas` would never match any piece
    all.listed <- unlist(lapply(slots, function(s){ strsplit(unlist(strsplit(s$unupgraded$AREA_LIST, "|", fixed = TRUE)), ";", fixed = TRUE) }))
    expect_true(all(areas %in% all.listed), info = paste(setdiff(areas, all.listed), collapse = ", "))
})

test_that("stats are consistent between +0 and max", {
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        f <- slots[[slot]]$fullupgrade
        ## STAM_MOD is legitimately negative for heavy armor
        for(col in c(def.cols, res.cols, "POISE", "DURABILITY", "WEIGHT")){
            expect_true(all(u[[col]] >= 0) && all(f[[col]] >= 0), info = paste(slot, col))
        }
        ## The upgrade interpolation (get.interp.data) assumes these never change with upgrade level
        for(col in c("POISE", "DURABILITY", "WEIGHT", "STAM_MOD", "SOUND_MOD")){
            expect_equal(f[[col]], u[[col]], info = paste(slot, col))
        }
        none <- u$UPGRADE_TYPE == "None"
        for(col in c(def.cols, res.cols)){
            expect_equal(f[[col]][none], u[[col]][none], info = paste(slot, col))
            expect_true(all(f[[col]][!none] >= u[[col]][!none]), info = paste(slot, col))
        }
        ## The developers' entered values are whole numbers at +0 - strike/slash/thrust aren't
        for(col in c("PHYS_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", res.cols)){
            expect_true(all(u[[col]] == round(u[[col]])), info = paste(slot, col))
        }
    }
})

## Every upgradeable piece's max value is its +0 value times the standard full-upgrade multiplier
## (the last entry of each pattern in get.interp.data), up to rounding. The game itself rounds
## exact .x5 ties inconsistently (e.g. 7 x 1.55 = 10.85 displays as 10.8, but 31 x 1.55 = 48.05
## as 48.1), and strike/slash/thrust aren't whole numbers at +0, so their displayed +0 is itself
## rounded - real values can sit a little over 0.15 from the pattern. The 1.0 tolerance is for
## catching data-entry typos (a wrong digit), not rounding.
test_that("every upgradeable piece's max value follows the standard upgrade multiplier", {
    full.multiplier <- list(Regular = c(def = 2.42, res = 1.40), Twinkling = c(def = 1.55, res = 1.27))
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        f <- slots[[slot]]$fullupgrade
        for(type in c("Regular", "Twinkling")){
            rows <- u$UPGRADE_TYPE == type
            for(col in c(def.cols, res.cols)){
                m <- full.multiplier[[type]][[if(col %in% def.cols) "def" else "res"]]
                off <- abs(f[[col]][rows] - u[[col]][rows]*m) > 1.0
                expect_false(any(off), info = paste(slot, type, col, paste(u$ARMOR[rows][off], collapse = ", ")))
            }
        }
    }
})

test_that("each slot has exactly one always-available, all-zero 'No <slot>' row", {
    for(slot in names(slots)){
        for(tbl in slots[[slot]][c("unupgraded", "fullupgrade")]){
            none <- tbl[ARMOR == slots[[slot]]$none]
            expect_equal(nrow(none), 1, info = slot)
            expect_true(all(unlist(none[, c(def.cols, res.cols, "POISE", "WEIGHT", "STAM_MOD"), with = FALSE]) == 0), info = slot)
            expect_equal(none$DURABILITY, 999, info = slot)
            expect_equal(none$SOUND_MOD, 1, info = slot)
            expect_equal(none$AREA_MATCH_TYPE, "ALWAYS", info = slot)
            expect_equal(none$UPGRADE_TYPE, "None", info = slot)
        }
    }
})
