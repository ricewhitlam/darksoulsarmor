## The Equip Load page on its own (inst/shiny/R/equipload.R): a setup's equip load, weight and movement
## type exactly as the game computes them (R/equip-load.R) and on paper, the weapon arrangements
## under which the game agrees with paper, the random examples exactly on a line, and the page's
## Calculate, message and example buttons
source(system.file("shiny", "R", "pieces.R", package = "darksoulsarmor"), local = TRUE)
source(system.file("shiny", "R", "equipload.R", package = "darksoulsarmor"), local = TRUE)

## A setup as the page holds it: weapons by slot (empty unless given), armor by name
setup.of <- function(endurance, havel = FALSE, favor = FALSE, weapons = c(), armor){
    all.weapons <- c(right.1 = "None", right.2 = "None", left.1 = "None", left.2 = "None")
    all.weapons[names(weapons)] <- weapons
    list(endurance = endurance, havel.ring = havel, favor.ring = favor, weapons = all.weapons, head = armor[1], chest = armor[2], hands = armor[3], legs = armor[4])
}

## Some of the in-game readings test-equip-load.R records: each must roll as it did in the game
in.game <- list(
    list(name = "Control", setup = setup.of(99, favor = TRUE, armor = c("Balder Helm", "Havel's Armor", "Giant Gauntlets", "Steel Leggings")), rolled = "Light"),
    list(name = "A", setup = setup.of(99, favor = TRUE, armor = c("Giant Helm", "Smough's Armor", "Guardian Gauntlets", "Cleric Leggings")), rolled = "Mid"),
    list(name = "W1", setup = setup.of(99, favor = TRUE, weapons = c(right.1 = "Longsword"), armor = c("Giant Helm", "Smough's Armor", "Cleric Gauntlets", "Cleric Leggings")), rolled = "Mid"),
    list(name = "P1", setup = setup.of(99, favor = TRUE, weapons = c(left.1 = "Talisman", left.2 = "Talisman", right.1 = "Claymore"), armor = c("Balder Helm", "Smough's Armor", "Catarina Gauntlets", "Balder Leggings")), rolled = "Mid"),
    list(name = "P3", setup = setup.of(99, favor = TRUE, weapons = c(left.1 = "Talisman", right.1 = "Talisman", left.2 = "Claymore"), armor = c("Balder Helm", "Smough's Armor", "Catarina Gauntlets", "Balder Leggings")), rolled = "Light"),
    list(name = "T1", setup = setup.of(60, favor = TRUE, armor = c("Mask of the Father", "Armor of Artorias", "Havel's Gauntlets", "Steel Leggings")), rolled = "Light"),
    list(name = "T3", setup = setup.of(40, favor = TRUE, armor = c("Mask of the Father", "Cleric Armor", "Havel's Gauntlets", "No Legs")), rolled = "Light"),
    list(name = "T4", setup = setup.of(40, havel = TRUE, armor = c("Mask of the Father", "Armor of Artorias", "Havel's Gauntlets", "Steel Leggings")), rolled = "Mid")
)

test_that("the breakdown rolls as the game did, by the package's own equip-load rule", {
    for(reading in in.game){
        b <- equip.load.breakdown(reading$setup)
        expect_equal(b$movement.game, reading$rolled, info = reading$name)
        ## Its running total ends on the package's total, and its load and lines are the package's
        s <- reading$setup
        weight.of <- function(pieces, piece){ pieces[ARMOR == piece, WEIGHT] }
        expect_identical(b$total.game, darksoulsarmor:::total.weight(darksoulsarmor:::weapons.weight(s$weapons),
            weight.of(HEAD.PIECES, s$head), weight.of(CHEST.PIECES, s$chest), weight.of(HANDS.PIECES, s$hands), weight.of(LEGS.PIECES, s$legs)), info = reading$name)
        expect_identical(b$load.game, darksoulsarmor:::equip.load(s$endurance, s$havel.ring, s$favor.ring, father.mask = s$head == "Mask of the Father"), info = reading$name)
        expect_identical(unname(b$lines$GAME_LINE), unname(vapply(c("Light", "Mid", "Fat"), function(m){ darksoulsarmor:::movement.line(b$load.game, m) }, numeric(1))), info = reading$name)
        expect_equal(nrow(b$steps), 8)
    }
    ## Every one of these sets is exactly on the Light line on paper (the readings were chosen so)
    expect_true(all(vapply(in.game, function(r){ identical(equip.load.breakdown(r$setup)$on.line, "Light") }, logical(1))))
})

test_that("the breakdown sets the game against paper", {
    ## A: exactly on the Light line on paper (166.8 x 25% = 41.7), a hair over it in the game
    b <- equip.load.breakdown(in.game[[2]]$setup)
    expect_equal(c(b$load.paper, b$total.paper, b$lines$PAPER_LINE[1]), c(166.8, 41.7, 41.7))
    expect_equal(c(b$movement.paper, b$movement.game), c("Light", "Mid"))
    expect_gt(b$total.game, b$lines$GAME_LINE[1])
    ## The Ring of Favor's 1.2 puts the game's line a hair above paper's; the Mask's 1.05 a hair below
    favor <- equip.load.breakdown(setup.of(1, favor = TRUE, armor = c("No Head", "No Chest", "No Hands", "No Legs")))
    expect_equal(favor$lines$PAPER_LINE[1], 12.3)
    expect_gt(favor$lines$GAME_LINE[1], 12.3)
    mask <- equip.load.breakdown(setup.of(0, havel = TRUE, armor = c("Mask of the Father", "No Chest", "No Hands", "No Legs")))
    expect_true(mask$mask)
    expect_equal(mask$lines$PAPER_LINE[2], 31.5)
    expect_lt(mask$lines$GAME_LINE[2], 31.5)
    ## Empty slots weigh nothing and change nothing
    expect_equal(favor$steps$GAME_TOTAL, rep(0, 8))
    ## The text shown: paper with at least one decimal, the game to 7
    expect_identical(c(paper.text(84), paper.text(88.2), paper.text(22.05)), c("84.0", "88.2", "22.05"))
    expect_identical(game.text(float32(21.0000019)), "21.0000019")
})

test_that("only the order tips P1 over: its arrangements that agree with paper include P3's", {
    p1 <- equip.load.breakdown(in.game[[4]]$setup)
    expect_equal(c(p1$movement.paper, p1$movement.game), c("Light", "Mid"))
    arrangements <- weapon.arrangements(p1)
    expect_identical(names(arrangements), unname(EQUIP.WEAPON.LABELS))
    as.rows <- function(d){ apply(as.matrix(d), 1, paste, collapse = " | ") }
    p3 <- in.game[[5]]$setup$weapons[names(EQUIP.WEAPON.LABELS)]
    expect_true(paste(p3, collapse = " | ") %in% as.rows(arrangements))
    expect_false(paste(in.game[[4]]$setup$weapons[names(EQUIP.WEAPON.LABELS)], collapse = " | ") %in% as.rows(arrangements))
    ## Each listed arrangement really is Light in the game, and each distinct one is listed once
    for(row in seq_len(nrow(arrangements))){
        weapons <- stats::setNames(unlist(arrangements[row]), names(EQUIP.WEAPON.LABELS))
        s <- in.game[[4]]$setup
        s$weapons <- weapons
        expect_equal(equip.load.breakdown(s)$movement.game, "Light")
    }
    expect_equal(anyDuplicated(as.rows(arrangements)), 0)
})

test_that("the random examples are exactly on a line on paper, and over it or within it in the game", {
    set.seed(1)
    for(i in 1:8){
        over <- equip.load.breakdown(equip.load.example("over"))
        expect_false(is.na(over$on.line))
        expect_equal(over$movement.paper, over$on.line)
        expect_false(over$movement.game == over$movement.paper)
        expect_gt(over$total.game, over$lines[MOVEMENT == over$on.line]$GAME_LINE)

        within <- equip.load.breakdown(equip.load.example("within"))
        expect_false(is.na(within$on.line))
        expect_equal(within$movement.game, within$movement.paper)
        expect_lte(within$total.game, within$lines[MOVEMENT == within$on.line]$GAME_LINE)
    }
    ## Their items are real choices from the page's lists
    s <- equip.load.example("over")
    expect_true(all(s$weapons %in% c("None", weapon.data$WEAPON)))
    expect_identical(names(s$weapons), names(EQUIP.WEAPON.LABELS))
    expect_true(s$head %in% HEAD.PIECES$ARMOR && s$chest %in% CHEST.PIECES$ARMOR && s$hands %in% HANDS.PIECES$ARMOR && s$legs %in% LEGS.PIECES$ARMOR)
})

test_that("the page calculates on Calculate, flags changed inputs, and fills in examples", {
    updates <- list()
    local_mocked_bindings(updatePickerInput = function(session = NULL, inputId, selected = NULL, ...){ updates[[inputId]] <<- selected }, .package = "shinyWidgets")
    shiny::testServer(equip.load.server, {
        ## A Ring of Favor setup, as the browser sends it
        inputs <- list(endurance = 99, havel.ring = FALSE, favor.ring = TRUE,
                       weapon.right.1 = "None", weapon.right.2 = "None", weapon.left.1 = "None", weapon.left.2 = "None",
                       head = "Giant Helm", chest = "Smough's Armor", hands = "Guardian Gauntlets", legs = "Cleric Leggings")
        do.call(session$setInputs, inputs)
        expect_match(output$message, "click 'Calculate'")
        expect_null(calculated())

        session$setInputs(calculate = 1)
        expect_equal(output$message, "")
        expect_equal(session$returned$breakdown()$movement.game, "Mid")
        expect_match(output$readout$html, "The game disagrees with paper", fixed = TRUE)
        expect_match(output$steps, "Smough&#39;s Armor|Smough's Armor")
        expect_match(output$lines, "41.7")

        ## A changed input is flagged until the next Calculate
        session$setInputs(hands = "Cleric Gauntlets")
        expect_match(output$message, "Inputs have changed")
        session$setInputs(calculate = 2)
        expect_identical(calculated()$hands, "Cleric Gauntlets")
        expect_equal(output$message, "")
        ## Endurance is kept to 0-99, whatever a client sends: 150 reads as the 99 already calculated
        session$setInputs(endurance = 150)
        expect_equal(current.setup()$endurance, 99)
        expect_equal(output$message, "")
        session$setInputs(endurance = -3)
        expect_equal(current.setup()$endurance, 0)

        ## A choice that isn't in the lists is read as an empty slot
        session$setInputs(head = "Crown of Everything")
        expect_identical(current.setup()$head, "No Head")

        ## An example is calculated straight away, and the inputs are set to it
        set.seed(2)
        session$setInputs(random_within = 1)
        example <- calculated()
        expect_false(is.na(session$returned$breakdown()$on.line))
        expect_identical(updates$head, example$head)
        expect_identical(updates$weapon.right.1, example$weapons[["right.1"]])
        expect_match(output$readout$html, "Exactly on the", fixed = TRUE)
        ## Its total is exactly on the line in the game too, shown unsigned; the other lines signed
        b <- session$returned$breakdown()
        cells <- trimws(gsub("</?td>", "", regmatches(output$lines, gregexpr("<td>[^<]*</td>", output$lines))[[1]]))
        table <- matrix(cells, ncol = 5, byrow = TRUE)
        expect_identical(table[, 1], c("Light", "Mid", "Fat"))
        distance <- table[, 4]
        gap <- b$total.game - b$lines$GAME_LINE
        expect_identical(distance[gap == 0], rep("0.0000000", sum(gap == 0)))
        expect_true(all(grepl("^[+-][0-9]", distance[gap != 0])))
        ## Once the browser has set the inputs, nothing is flagged
        do.call(session$setInputs, list(endurance = example$endurance, havel.ring = example$havel.ring, favor.ring = example$favor.ring,
            weapon.right.1 = example$weapons[["right.1"]], weapon.right.2 = example$weapons[["right.2"]], weapon.left.1 = example$weapons[["left.1"]], weapon.left.2 = example$weapons[["left.2"]],
            head = example$head, chest = example$chest, hands = example$hands, legs = example$legs))
        expect_equal(output$message, "")
    })
})
