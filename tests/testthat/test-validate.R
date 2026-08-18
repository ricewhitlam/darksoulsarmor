## get.optimal.armor.combos validates every argument before doing any work; these tests cover
## one invalid shape per argument to guard against a check silently being dropped or loosened.
test_that("get.optimal.armor.combos rejects invalid argument shapes", {
    expect_error(get.optimal.armor.combos(max.table.size = "1000"), "max.table.size")
    expect_error(get.optimal.armor.combos(max.table.size = NA), "max.table.size")
    expect_error(get.optimal.armor.combos(starting.class = "Not A Class"), "starting.class")
    expect_error(get.optimal.armor.combos(areas.completed = "Not An Area"), "areas.completed")
    expect_error(get.optimal.armor.combos(upgrade.types = "Not A Type"), "upgrade.types")
    expect_error(get.optimal.armor.combos(head.filter = "Not An Armor Piece"), "head.filter")
    expect_error(get.optimal.armor.combos(chest.filter = "Not An Armor Piece"), "chest.filter")
    expect_error(get.optimal.armor.combos(hands.filter = "Not An Armor Piece"), "hands.filter")
    expect_error(get.optimal.armor.combos(legs.filter = "Not An Armor Piece"), "legs.filter")
    expect_error(get.optimal.armor.combos(regular.level = "+11"), "regular.level")
    expect_error(get.optimal.armor.combos(twinkling.level = "+6"), "twinkling.level")
    expect_error(get.optimal.armor.combos(roll = "Sprint"), "roll")
    expect_error(get.optimal.armor.combos(unarmored.weight = "ten"), "unarmored.weight")
    expect_error(get.optimal.armor.combos(endurance.level = NA), "endurance.level")
    expect_error(get.optimal.armor.combos(havel.ring = "yes"), "havel.ring")
    expect_error(get.optimal.armor.combos(favor.ring = c(TRUE, FALSE)), "favor.ring")
    expect_error(get.optimal.armor.combos(wolf.ring = NA), "wolf.ring")
    expect_error(get.optimal.armor.combos(minima = rep(0, 11)), "minima")
    expect_error(get.optimal.armor.combos(minima = c(rep(0, 11), NA)), "minima")
    expect_error(get.optimal.armor.combos(weights = rep(0.1, 9)), "weights")
    expect_error(get.optimal.armor.combos(weights = rep(-1, 10)), "weights")
    expect_error(get.optimal.armor.combos(weights = rep(0, 10)), "weights")
})

## NULL and character(0) are documented to mean the same thing ("no armor in this slot"), and
## must produce actual non-empty results (all(x == "No Head") on a zero-row result is vacuously
## TRUE, so nrow() is checked explicitly to guard against silently getting zero rows back).
test_that("get.optimal.armor.combos treats NULL and character(0) slot filters identically", {
    r.head.null <- get.optimal.armor.combos(max.table.size = 10, head.filter = NULL)
    r.head.empty <- get.optimal.armor.combos(max.table.size = 10, head.filter = character(0))
    expect_gt(nrow(r.head.empty$data), 0)
    expect_true(all(r.head.empty$data$HEAD == "No Head"))
    expect_equal(r.head.null$data, r.head.empty$data)

    r.chest.null <- get.optimal.armor.combos(max.table.size = 10, chest.filter = NULL)
    r.chest.empty <- get.optimal.armor.combos(max.table.size = 10, chest.filter = character(0))
    expect_gt(nrow(r.chest.empty$data), 0)
    expect_true(all(r.chest.empty$data$CHEST == "No Chest"))
    expect_equal(r.chest.null$data, r.chest.empty$data)

    r.hands.null <- get.optimal.armor.combos(max.table.size = 10, hands.filter = NULL)
    r.hands.empty <- get.optimal.armor.combos(max.table.size = 10, hands.filter = character(0))
    expect_gt(nrow(r.hands.empty$data), 0)
    expect_true(all(r.hands.empty$data$HANDS == "No Hands"))
    expect_equal(r.hands.null$data, r.hands.empty$data)

    r.legs.null <- get.optimal.armor.combos(max.table.size = 10, legs.filter = NULL)
    r.legs.empty <- get.optimal.armor.combos(max.table.size = 10, legs.filter = character(0))
    expect_gt(nrow(r.legs.empty$data), 0)
    expect_true(all(r.legs.empty$data$LEGS == "No Legs"))
    expect_equal(r.legs.null$data, r.legs.empty$data)
})
