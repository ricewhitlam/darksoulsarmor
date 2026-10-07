## Validates the shipped armor tables (data/*.rda, built by create_rda/create_rda.R from the
## create_rda/*.csv files). Mistakes in those CSVs mostly fail silently - e.g. a mistyped area name
## never matches a completed area, so the piece quietly becomes unobtainable - so these checks
## guard every later edit to the data.

slots <- list(
    head = list(unupgraded = head.data.unupgraded, none = "No Head"),
    chest = list(unupgraded = chest.data.unupgraded, none = "No Chest"),
    hands = list(unupgraded = hands.data.unupgraded, none = "No Hands"),
    legs = list(unupgraded = legs.data.unupgraded, none = "No Legs")
)
def.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF")
res.cols <- c("BLEED_RES", "POIS_RES", "CURSE_RES")
fixed.cols <- c("POISE", "DURABILITY", "WEIGHT", "STAM_MOD", "SOUND_MOD")

## Every stat as displayed, recorded by hand: each piece at +0 (from the wiki) and at max upgrade
## (read in-game). Displays round to one decimal, so these are observations to check the exact
## values against, not data the package uses.
displayed.00 <- data.table::fread(test_path("fixtures", "displayed_00.csv"))
displayed.10 <- data.table::fread(test_path("fixtures", "displayed_10.csv"))

test_that("each slot lists unique pieces with no missing values", {
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        expect_false(anyDuplicated(u$ARMOR) > 0, info = slot)
        expect_false(anyNA(u), info = slot)
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

test_that("stats are valid at +0", {
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        ## STAM_MOD is legitimately negative for heavy armor
        for(col in c(def.cols, res.cols, "POISE", "DURABILITY", "WEIGHT")){
            expect_true(all(u[[col]] >= 0), info = paste(slot, col))
        }
        ## The developers' entered values are whole numbers - strike/slash/thrust aren't (below)
        for(col in c("PHYS_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", res.cols)){
            expect_true(all(u[[col]] == round(u[[col]])), info = paste(slot, col))
        }
    }
})

## The game stores strike, slash and thrust defense as a whole-number percentage of physical
## defense (e.g. Black Sorcerer Hat: strike 103%) and computes them in 32-bit floating point, so
## each must be exactly float32(physical x percentage). A typo in one of these 9-digit values
## would break that.
test_that("strike, slash and thrust are physical defense times a whole-number percentage, in 32-bit", {
    float32 <- darksoulsarmor:::float32
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        for(col in c("STRIKE_DEF", "SLASH_DEF", "THRUST_DEF")){
            expect_true(all(float32(u[[col]]) == u[[col]]), info = paste(slot, col))
            pct <- ifelse(u$PHYS_DEF > 0, round(100*u[[col]]/u$PHYS_DEF), 100)
            rebuilt <- float32(u$PHYS_DEF*float32(pct/100))
            expect_true(all(rebuilt == u[[col]]), info = paste(slot, col, paste(u$ARMOR[rebuilt != u[[col]]], collapse = ", ")))
        }
    }
})

test_that("the displayed values cover exactly the package's pieces", {
    pieces <- sort(unlist(lapply(slots, function(s){ s$unupgraded$ARMOR }), use.names = FALSE))
    expect_identical(sort(displayed.00$ARMOR), pieces)
    expect_identical(sort(displayed.10$ARMOR), pieces)
})

## At +0, and at max upgrade (+10 regular, +5 twinkling; pieces that can't be upgraded stay at +0),
## every exact value - stored, or computed from it by get.interp.data - must display as the
## recorded value: within half a display step, plus 32-bit noise. How the game rounds an exact tie
## (e.g. 9.25) isn't consistent, so a tie may display either way. Stats that never change with
## upgrade level must match exactly.
test_that("the exact values are within half a display step of the displayed values, at +0 and max", {
    for(slot in names(slots)){
        u <- slots[[slot]]$unupgraded
        at.max <- darksoulsarmor:::get.interp.data(u, 10, 5)
        for(level in list(list(name = "+0", exact = u, shown = displayed.00), list(name = "max", exact = at.max, shown = displayed.10))){
            shown <- level$shown[match(level$exact$ARMOR, ARMOR)]
            for(col in c(def.cols, res.cols)){
                far <- abs(level$exact[[col]] - shown[[col]]) > 0.05 + 1e-5
                expect_false(any(far), info = paste(slot, level$name, col, paste(level$exact$ARMOR[far], collapse = ", ")))
            }
            for(col in fixed.cols){
                expect_equal(level$exact[[col]], shown[[col]], info = paste(slot, level$name, col))
            }
        }
    }
})

test_that("each slot has exactly one always-available, all-zero 'No <slot>' row", {
    for(slot in names(slots)){
        none <- slots[[slot]]$unupgraded[ARMOR == slots[[slot]]$none]
        expect_equal(nrow(none), 1, info = slot)
        expect_true(all(unlist(none[, c(def.cols, res.cols, "POISE", "WEIGHT", "STAM_MOD"), with = FALSE]) == 0), info = slot)
        expect_equal(none$DURABILITY, 999, info = slot)
        expect_equal(none$SOUND_MOD, 1, info = slot)
        expect_equal(none$AREA_MATCH_TYPE, "ALWAYS", info = slot)
        expect_equal(none$UPGRADE_TYPE, "None", info = slot)
    }
})

## The weapon.data table: one row per weapon (weight is all the package uses), weights as the game has
## them - including the items used in the in-game equip-load checks
test_that("the weapon.data table lists unique weapons with valid weights", {
    expect_false(anyDuplicated(weapon.data$WEAPON) > 0)
    expect_false(anyNA(weapon.data))
    expect_true(all(weapon.data$WEIGHT >= 0))
    expect_true(all(abs(weapon.data$WEIGHT*10 - round(weapon.data$WEIGHT*10)) < 1e-9))
    weight.of <- function(name){ weapon.data[WEAPON == name, WEIGHT] }
    expect_equal(weight.of("Longsword"), 3)
    expect_equal(weight.of("Claymore"), 6)
    expect_equal(weight.of("Talisman"), 0.3)
    expect_equal(weight.of("Pyromancy Flame"), 0)
})
