## get.optimal.armor.combos validates every argument before doing any work; these tests cover
## one invalid shape per argument to guard against a check silently being dropped or loosened.
test_that("get.optimal.armor.combos rejects invalid argument shapes", {
    expect_error(get.optimal.armor.combos(weights = default.weights, max.table.size = "1000"), "max.table.size")
    expect_error(get.optimal.armor.combos(weights = default.weights, max.table.size = NA), "max.table.size")
    expect_error(get.optimal.armor.combos(weights = default.weights, starting.class = "Not A Class"), "starting.class")
    expect_error(get.optimal.armor.combos(weights = default.weights, areas.completed = "Not An Area"), "areas.completed")
    expect_error(get.optimal.armor.combos(weights = default.weights, upgrade.types = "Not A Type"), "upgrade.types")
    expect_error(get.optimal.armor.combos(weights = default.weights, head.filter = "Not An Armor Piece"), "head.filter")
    expect_error(get.optimal.armor.combos(weights = default.weights, chest.filter = "Not An Armor Piece"), "chest.filter")
    expect_error(get.optimal.armor.combos(weights = default.weights, hands.filter = "Not An Armor Piece"), "hands.filter")
    expect_error(get.optimal.armor.combos(weights = default.weights, legs.filter = "Not An Armor Piece"), "legs.filter")
    expect_error(get.optimal.armor.combos(weights = default.weights, regular.level = "+11"), "regular.level")
    expect_error(get.optimal.armor.combos(weights = default.weights, twinkling.level = "+6"), "twinkling.level")
    expect_error(get.optimal.armor.combos(weights = default.weights, movement = "Sprint"), "movement")
    ## The names before movement types were renamed (roll Fast/Mid/Fat/None) are gone
    expect_error(get.optimal.armor.combos(weights = default.weights, movement = "Fast"), "movement")
    expect_error(get.optimal.armor.combos(weights = default.weights, movement = "None"), "movement")
    expect_error(get.optimal.armor.combos(weights = default.weights, roll = "Mid"), "unused argument")
    expect_error(get.armor.tradeoffs(weights = default.weights, roll = "Mid"), "unused argument")
    ## Weapons are given per slot, by name; a single weight is no longer accepted
    expect_error(get.optimal.armor.combos(weights = default.weights, unarmored.weight = 10), "unused argument")
    expect_error(get.optimal.armor.combos(weights = default.weights, weapons = 10), "weapons")
    expect_error(get.optimal.armor.combos(weights = default.weights, weapons = c("Longsword", "Talisman")), "weapons.*all four")
    expect_error(get.optimal.armor.combos(weights = default.weights, weapons = c(left.3 = "Longsword")), "weapons.*unknown slots: left.3")
    expect_error(get.optimal.armor.combos(weights = default.weights, weapons = c(left.1 = "Longsword", left.1 = "Club")), "weapons.*duplicated")
    expect_error(get.optimal.armor.combos(weights = default.weights, weapons = c(left.1 = "Excalibur")), "weapons.*unknown weapons: Excalibur")
    expect_error(get.optimal.armor.combos(weights = default.weights, weapons = c(left.1 = NA_character_)), "weapons")
    expect_error(get.optimal.armor.combos(weights = default.weights, endurance.level = NA), "endurance.level")
    expect_error(get.optimal.armor.combos(weights = default.weights, havel.ring = "yes"), "havel.ring")
    expect_error(get.optimal.armor.combos(weights = default.weights, favor.ring = c(TRUE, FALSE)), "favor.ring")
    expect_error(get.optimal.armor.combos(weights = default.weights, wolf.ring = NA), "wolf.ring")
    expect_error(get.optimal.armor.combos(weights = default.weights, minima = rep(0, 11)), "minima")
    expect_error(get.optimal.armor.combos(weights = default.weights, minima = c(rep(0, 11), NA)), "minima")
    expect_error(get.optimal.armor.combos(weights = default.weights, stat.scale = "SD"), "stat.scale.*sd, max, raw")
    expect_error(get.optimal.armor.combos(weights = default.weights, stat.scale = c("sd", "max")), "stat.scale")
    expect_error(get.optimal.armor.combos(weights = default.weights, stat.scale = NA_character_), "stat.scale")
    expect_error(get.optimal.armor.combos(weights = default.weights, stat.scale = 1), "stat.scale")
    expect_error(get.armor.tradeoffs(weights = default.weights, stat.scale = "points"), "stat.scale")
    expect_error(get.optimal.armor.combos(weights = rep(0.1, 9)), "weights")
    expect_error(get.optimal.armor.combos(weights = rep(-1, 10)), "weights")
    expect_error(get.optimal.armor.combos(weights = rep(0, 10)), "weights")
})

## NULL and character(0) are documented to mean the same thing ("no armor in this slot"), and
## must produce actual non-empty results (all(x == "No Head") on a zero-row result is vacuously
## TRUE, so nrow() is checked explicitly to guard against silently getting zero rows back).
test_that("get.optimal.armor.combos treats NULL and character(0) slot filters identically", {
    r.head.null <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, head.filter = NULL)
    r.head.empty <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, head.filter = character(0))
    expect_gt(nrow(r.head.empty$data), 0)
    expect_true(all(r.head.empty$data$HEAD == "No Head"))
    expect_equal(r.head.null$data, r.head.empty$data)

    r.chest.null <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, chest.filter = NULL)
    r.chest.empty <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, chest.filter = character(0))
    expect_gt(nrow(r.chest.empty$data), 0)
    expect_true(all(r.chest.empty$data$CHEST == "No Chest"))
    expect_equal(r.chest.null$data, r.chest.empty$data)

    r.hands.null <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, hands.filter = NULL)
    r.hands.empty <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, hands.filter = character(0))
    expect_gt(nrow(r.hands.empty$data), 0)
    expect_true(all(r.hands.empty$data$HANDS == "No Hands"))
    expect_equal(r.hands.null$data, r.hands.empty$data)

    r.legs.null <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, legs.filter = NULL)
    r.legs.empty <- get.optimal.armor.combos(weights = default.weights, max.table.size = 10, legs.filter = character(0))
    expect_gt(nrow(r.legs.empty$data), 0)
    expect_true(all(r.legs.empty$data$LEGS == "No Legs"))
    expect_equal(r.legs.null$data, r.legs.empty$data)
})

## minima and weights also accept named vectors; malformed names get a specific error
test_that("named minima/weights reject unknown, duplicated, and partial names", {
    expect_error(get.optimal.armor.combos(weights = c(PHYS = 1)), "weights.*unknown names: PHYS. Valid names are: PHYS_DEF")
    expect_error(get.optimal.armor.combos(weights = default.weights, minima = c(POISE = 30, Poise = 5)), "minima.*unknown names: Poise")
    expect_error(get.optimal.armor.combos(weights = c(PHYS_DEF = 1, PHYS_DEF = 2)), "weights.*duplicated names: PHYS_DEF")
    expect_error(get.optimal.armor.combos(weights = default.weights, minima = setNames(c(30, 5), c("POISE", ""))), "minima.*name every entry or none")
    ## POISE and DURABILITY are minima-only metrics, not scored
    expect_error(get.optimal.armor.combos(weights = c(POISE = 1)), "weights.*unknown names: POISE")
    ## Value checks still apply to named vectors
    expect_error(get.optimal.armor.combos(weights = c(PHYS_DEF = 0)), "weights")
    expect_error(get.optimal.armor.combos(weights = c(PHYS_DEF = -1, MAG_DEF = 2)), "weights")
    expect_error(get.optimal.armor.combos(weights = default.weights, minima = c(POISE = NA)), "minima")
})

test_that("duplicated filter, area, and upgrade-type entries are removed from args without changing results", {
    heads <- head.data.unupgraded$ARMOR[1:5]
    plain <- get.optimal.armor.combos(weights = default.weights, max.table.size = 50, head.filter = heads, areas.completed = areas[1:6], upgrade.types = c("Regular", "None"))
    duplicated <- get.optimal.armor.combos(weights = default.weights, max.table.size = 50, head.filter = c(heads, heads[2:3]), areas.completed = c(areas[1:6], areas[1]), upgrade.types = c("Regular", "None", "Regular"))
    expect_identical(duplicated$data, plain$data)
    expect_identical(duplicated$args$head.filter, heads)
    expect_identical(duplicated$args$areas.completed, areas[1:6])
    expect_identical(duplicated$args$upgrade.types, c("Regular", "None"))
})

## weights has no default: every search, the trade-off curves included (even of a single stat,
## where the score breaks ties), says what "better" means
test_that("weights is required", {
    expect_error(get.optimal.armor.combos(), "weights is required")
    expect_error(get.optimal.armor.combos(max.table.size = 5, movement = "Mid"), "weights is required")
    expect_error(get.armor.tradeoffs(), "weights is required")
    expect_error(get.armor.tradeoffs(metric = "POISE", max.armor.weight = 3), "weights is required")
})

## weights.mode decides what weights must look like, and names from the other mode say so
test_that("weights.mode is checked, and weights against it", {
    expect_error(get.optimal.armor.combos(weights = default.weights, weights.mode = "groups"), "weights.mode.*individual, grouped")
    expect_error(get.optimal.armor.combos(weights = default.weights, weights.mode = c("individual", "grouped")), "weights.mode")
    expect_error(get.optimal.armor.combos(weights = default.weights, weights.mode = NA_character_), "weights.mode")
    expect_error(get.optimal.armor.combos(weights = default.weights, weights.mode = TRUE), "weights.mode")
    expect_error(get.optimal.armor.combos(weights = c(PHYS_DEF = 1), weights.mode = "grouped"),
                 "PHYS_DEF is a stat name, but weights.mode is \"grouped\". Use PHYSICAL_DEFENSES, ELEMENTAL_DEFENSES, RESISTANCES, or set weights.mode = \"individual\".", fixed = TRUE)
    expect_error(get.optimal.armor.combos(weights = c(PHYSICAL_DEFENSES = 1, RESISTANCES = 2)),
                 "PHYSICAL_DEFENSES, RESISTANCES are group names, but weights.mode is \"individual\".", fixed = TRUE)
    expect_error(get.optimal.armor.combos(weights = c(PHYSICAL_DEFENSES = 1, MAG_DEF = 2), weights.mode = "grouped"), "MAG_DEF is a stat name")
    expect_error(get.optimal.armor.combos(weights = c(PHYSICAL = 1), weights.mode = "grouped"), "weights.*unknown names: PHYSICAL")
    expect_error(get.optimal.armor.combos(weights = c(1, 2), weights.mode = "grouped"), "weights")
    expect_error(get.optimal.armor.combos(weights = default.weights, weights.mode = "grouped"), "weights")
    expect_error(get.optimal.armor.combos(weights = c(1, 2, 3)), "weights")
    expect_error(get.optimal.armor.combos(weights = c(0, 0, 0), weights.mode = "grouped"), "weights")
    expect_error(get.armor.tradeoffs(weights = c(1, 2, 3), weights.mode = "grouping"), "weights.mode")
})
