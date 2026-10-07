## The equip-load rule (R/equip-load.R) was established in-game: sets weighing exactly 25% of the
## equip load on paper, chosen so that 32-bit arithmetic tips some of them a hair over the Light
## line, then rolled. Every reading is recorded here, and both the rule and an actual search must
## reproduce it: a Light search finds the set exactly when it rolled Light.
## - 99 Endurance with the Ring of Favor (load 166.8, Light line 41.7): Control and A established
##   32-bit weights against a 32-bit line; Decider and Confirmation the armor's order (head, chest,
##   hands, legs - also when put on in another order, which is why A and Decider stand for those
##   checks too); W1-W3 that weapons are added before the armor; P1-P3 the weapon slots' order
##   (left 1, right 1, left 2, right 2), with two talismans.
## - 60 Endurance (T1) and 40 Endurance (T3, T4): that the effects' rates are multiplied together
##   first, then applied to the load.
in.game <- list(
    list(name = "Control", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(), armor = c("Balder Helm", "Havel's Armor", "Giant Gauntlets", "Steel Leggings"), rolled = "Light"),
    list(name = "A", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(), armor = c("Giant Helm", "Smough's Armor", "Guardian Gauntlets", "Cleric Leggings"), rolled = "Mid"),
    list(name = "Decider", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(), armor = c("Fang Boar Helm", "Smough's Armor", "Black Iron Gauntlets", "Catarina Leggings"), rolled = "Light"),
    list(name = "Confirmation", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(), armor = c("Giant Helm", "Stone Armor", "Guardian Gauntlets", "Cleric Leggings"), rolled = "Mid"),
    list(name = "W1", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(right.1 = "Longsword"), armor = c("Giant Helm", "Smough's Armor", "Cleric Gauntlets", "Cleric Leggings"), rolled = "Mid"),
    list(name = "W2", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(right.1 = "Longsword"), armor = c("Dark Mask", "Smough's Armor", "Guardian Gauntlets", "Catarina Leggings"), rolled = "Mid"),
    list(name = "W3", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(right.1 = "Longsword"), armor = c("Balder Helm", "Havel's Armor", "Paladin Gauntlets", "Cleric Leggings"), rolled = "Light"),
    list(name = "P1", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(left.1 = "Talisman", left.2 = "Talisman", right.1 = "Claymore"), armor = c("Balder Helm", "Smough's Armor", "Catarina Gauntlets", "Balder Leggings"), rolled = "Mid"),
    list(name = "P2", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(right.1 = "Talisman", right.2 = "Talisman", left.1 = "Claymore"), armor = c("Balder Helm", "Smough's Armor", "Catarina Gauntlets", "Balder Leggings"), rolled = "Mid"),
    list(name = "P3", endurance = 99, havel = FALSE, favor = TRUE, weapons = c(left.1 = "Talisman", right.1 = "Talisman", left.2 = "Claymore"), armor = c("Balder Helm", "Smough's Armor", "Catarina Gauntlets", "Balder Leggings"), rolled = "Light"),
    list(name = "T1", endurance = 60, havel = FALSE, favor = TRUE, weapons = c(), armor = c("Mask of the Father", "Armor of Artorias", "Havel's Gauntlets", "Steel Leggings"), rolled = "Light"),
    list(name = "T3", endurance = 40, havel = FALSE, favor = TRUE, weapons = c(), armor = c("Mask of the Father", "Cleric Armor", "Havel's Gauntlets", "No Legs"), rolled = "Light"),
    list(name = "T4", endurance = 40, havel = TRUE, favor = FALSE, weapons = c(), armor = c("Mask of the Father", "Armor of Artorias", "Havel's Gauntlets", "Steel Leggings"), rolled = "Mid")
)

test_that("the equip-load rule reproduces every in-game reading", {
    weight.of <- function(table, piece){ if(grepl("^No ", piece)) 0 else table[ARMOR == piece, WEIGHT] }
    for(reading in in.game){
        weapons <- c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None")
        weapons[names(reading$weapons)] <- reading$weapons
        total <- darksoulsarmor:::total.weight(
            darksoulsarmor:::weapons.weight(weapons),
            weight.of(head.data.unupgraded, reading$armor[1]), weight.of(chest.data.unupgraded, reading$armor[2]),
            weight.of(hands.data.unupgraded, reading$armor[3]), weight.of(legs.data.unupgraded, reading$armor[4])
        )
        load <- darksoulsarmor:::equip.load(reading$endurance, reading$havel, reading$favor, father.mask = reading$armor[1] == "Mask of the Father")
        expect_equal(darksoulsarmor:::movement.type(total, load), reading$rolled, info = reading$name)
    }
})

test_that("a Light search finds exactly the in-game sets that rolled Light", {
    only <- function(piece){ if(grepl("^No ", piece)) character(0) else piece }
    for(reading in in.game){
        found <- get.optimal.armor.combos(
            max.table.size = 5, movement = "Light", endurance.level = reading$endurance, havel.ring = reading$havel, favor.ring = reading$favor,
            weapons = if(length(reading$weapons)) reading$weapons else c(left.1 = "None"),
            head.filter = only(reading$armor[1]), chest.filter = only(reading$armor[2]), hands.filter = only(reading$armor[3]), legs.filter = only(reading$armor[4])
        )$data
        expect_equal(nrow(found), if(reading$rolled == "Light") 1 else 0, info = reading$name)
    }
})

## The rates combine the same way in any order: three 32-bit rates multiply to the same 32-bit
## product whichever two are multiplied first
test_that("the equip load doesn't depend on the order the effects are combined in", {
    f32 <- darksoulsarmor:::float32
    rates <- f32(c(1.5, 1.2, 1.05))
    orders <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
    products <- sapply(orders, function(o){ x <- 1; for(i in o) x <- f32(x*rates[i]); x })
    expect_equal(length(unique(products)), 1)
    expect_equal(darksoulsarmor:::equip.load(99, TRUE, TRUE, TRUE), f32(139*products[1]), tolerance = 0)
})

test_that("weapons named by slot or given in slot order mean the same", {
    named <- get.optimal.armor.combos(max.table.size = 1, weapons = c(right.2 = "Talisman", left.1 = "Claymore"))$args$weapons
    ordered <- get.optimal.armor.combos(max.table.size = 1, weapons = c("Claymore", "None", "None", "Talisman"))$args$weapons
    expect_identical(named, c(left.1 = "Claymore", right.1 = "None", left.2 = "None", right.2 = "Talisman"))
    expect_identical(ordered, named)
    expect_identical(get.optimal.armor.combos(max.table.size = 1)$args$weapons, c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None"))
})

test_that("an empty weapons vector means no weapons", {
    expect_identical(get.optimal.armor.combos(max.table.size = 1, weapons = character(0))$args$weapons, c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None"))
    expect_identical(get.optimal.armor.combos(max.table.size = 1, weapons = stats::setNames(character(0), character(0)))$args$weapons, c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None"))
})
