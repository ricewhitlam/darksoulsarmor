## Which pieces the completed areas and the starting class make available. A piece's AREA_MATCH_TYPE
## and AREA_LIST hold one or more "|"-separated clauses, combined with OR: ALWAYS (no areas needed),
## ANY (one of the clause's ";"-separated areas) or ALL (every one of them). Its starting class's
## pieces are available regardless.

test_that("area requirements follow ALWAYS, ANY, ALL and |-separated OR clauses", {
    met <- darksoulsarmor:::area.requirement.met
    expect_true(met("ALWAYS", "", character(0)))
    ## ANY: one of the clause's areas is enough
    expect_true(met("ANY", "Undead Parish;Sens Fortress", "Sens Fortress"))
    expect_false(met("ANY", "Undead Parish;Sens Fortress", "Anor Londo"))
    ## ALL: every one of them
    expect_true(met("ALL", "New Londo Ruins;Valley of Drakes", c("Valley of Drakes", "Depths", "New Londo Ruins")))
    expect_false(met("ALL", "New Londo Ruins;Valley of Drakes", "New Londo Ruins"))
    ## Clauses are alternatives: Depths alone, or Undead Parish and Blighttown together
    expect_true(met("ANY|ALL", "Depths|Undead Parish;Blighttown", "Depths"))
    expect_true(met("ANY|ALL", "Depths|Undead Parish;Blighttown", c("Blighttown", "Undead Parish")))
    expect_false(met("ANY|ALL", "Depths|Undead Parish;Blighttown", "Undead Parish"))
    expect_false(met("ANY|ALL", "Depths|Undead Parish;Blighttown", character(0)))
})

## The pieces a search can use in each slot, given the completed areas and the starting class: every
## combination of the named pieces (each slot also allowing its empty "No <slot>" piece), with no
## other limit, so each available piece appears in some result
available.pieces <- function(pieces, areas.completed, starting.class = "Deprived"){
    result <- get.optimal.armor.combos(
        max.table.size = 1e5, movement = "Poop", areas.completed = areas.completed, starting.class = starting.class,
        head.filter = c("No Head", pieces[["head"]]), chest.filter = c("No Chest", pieces[["chest"]]),
        hands.filter = c("No Hands", pieces[["hands"]]), legs.filter = c("No Legs", pieces[["legs"]]),
        weights = default.weights
    )$data
    list(head = setdiff(result$HEAD, "No Head"), chest = setdiff(result$CHEST, "No Chest"), hands = setdiff(result$HANDS, "No Hands"), legs = setdiff(result$LEGS, "No Legs"))
}
none <- list(head = character(0), chest = character(0), hands = character(0), legs = character(0))

## Each slot filters its own table, so each rule is checked in all four, with a piece of each slot
## that has it
test_that("a search offers exactly the pieces the completed areas make available, in every slot", {
    ## Depths, or Undead Parish and Blighttown together
    either <- list(head = "Helm of the Wise", chest = "Armor of the Glorious", hands = "Gauntlets of the Vanquisher", legs = "Boots of the Explorer")
    expect_identical(available.pieces(either, character(0)), none)
    expect_identical(available.pieces(either, "Depths"), either)
    expect_identical(available.pieces(either, "Undead Parish"), none)
    expect_identical(available.pieces(either, c("Undead Parish", "Blighttown")), either)

    ## Both New Londo Ruins and Valley of Drakes
    both <- list(head = "Witch Hat", chest = "Witch Cloak", hands = "Witch Gloves", legs = "Witch Skirt")
    expect_identical(available.pieces(both, "New Londo Ruins"), none)
    expect_identical(available.pieces(both, "Valley of Drakes"), none)
    expect_identical(available.pieces(both, c("Valley of Drakes", "New Londo Ruins")), both)

    ## Either of Undead Parish and Sens Fortress
    any.of <- list(head = "Balder Helm", chest = "Balder Armor", hands = "Balder Gauntlets", legs = "Balder Leggings")
    expect_identical(available.pieces(any.of, "Sens Fortress"), any.of)
    expect_identical(available.pieces(any.of, "Undead Parish"), any.of)
    expect_identical(available.pieces(any.of, "Anor Londo"), none)
})

## A starting class's pieces are available without their areas; another class's aren't
test_that("a search offers the starting class's pieces regardless of areas, in every slot", {
    knight <- list(
        head = head.data.unupgraded[STARTING_CLASS == "Knight", ARMOR], chest = chest.data.unupgraded[STARTING_CLASS == "Knight", ARMOR],
        hands = hands.data.unupgraded[STARTING_CLASS == "Knight", ARMOR], legs = legs.data.unupgraded[STARTING_CLASS == "Knight", ARMOR]
    )
    expect_true(all(lengths(knight) == 1))
    expect_identical(available.pieces(knight, character(0), starting.class = "Knight"), knight)
    expect_identical(available.pieces(knight, character(0), starting.class = "Warrior"), none)
})
