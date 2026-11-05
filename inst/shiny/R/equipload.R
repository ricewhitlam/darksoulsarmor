
## The Equip Load page, as a Shiny module: a calculator for one setup - Endurance, rings, weapons and
## armor - showing its equip load, weight, equip percentage and movement type exactly as the game
## computes them (R/equip-load.R: 32-bit floating point, the weights added one at a time in the
## game's slot order), beside the same arithmetic on paper. Its inputs are its own: nothing else in
## the app reads or changes them, and it doesn't follow 'Refresh Armor Data'. Two buttons fill in a
## random setup exactly on a line on paper, that the game puts over it or keeps within it.

float32 <- darksoulsarmor:::float32

## The weapon slots as the page lists them (right hand first, as the game's menu does), and the
## game's own order for adding them up (left 1, right 1, left 2, right 2), then the armor's
EQUIP.WEAPON.LABELS <- c(right.1 = "Right Hand Weapon 1", right.2 = "Right Hand Weapon 2", left.1 = "Left Hand Weapon 1", left.2 = "Left Hand Weapon 2")
EQUIP.SLOT.LABELS <- c(left.1 = "Left Hand 1", right.1 = "Right Hand 1", left.2 = "Left Hand 2", right.2 = "Right Hand 2", head = "Head", chest = "Chest", hands = "Hands", legs = "Legs")
EQUIP.MOVEMENTS <- c("Light", "Mid", "Fat")

## A value on paper, rid of double-precision noise: every paper value here (weights in tenths, the
## rates 1.5, 1.2 and 1.05) has at most 6 decimals
paper.value <- function(x){ round(x, 6) }
## As shown: a paper value with at least one decimal (84.0, 88.2, 22.05); a game value to 7 decimals,
## where 32-bit drift shows (21.0000019)
paper.text <- function(x){ format(paper.value(x), nsmall = 1, digits = 12, scientific = FALSE) }
game.text <- function(x){ sprintf("%.7f", x) }

## Each item's listed weight: a weapon's ("None" weighs nothing), a piece's ("No Head" etc. weigh 0)
weapon.weight.of <- function(weapon){ if(weapon == "None") 0 else weapon.data$WEIGHT[match(weapon, weapon.data$WEAPON)] }
piece.weight.of <- function(pieces, piece){ pieces$WEIGHT[match(piece, pieces$ARMOR)] }

## A setup's equip load, weight and movement type, in the game and on paper. setup: endurance (0-99),
## havel.ring and favor.ring (TRUE/FALSE), weapons (named left.1, right.1, left.2, right.2), head,
## chest, hands and legs (piece names); the Mask of the Father's bonus applies when it's the head.
equip.load.breakdown <- function(setup){
    mask <- setup$head == "Mask of the Father"
    rate <- prod(c(if(setup$havel.ring) 1.5, if(setup$favor.ring) 1.2, if(mask) 1.05))
    load.paper <- paper.value((40 + setup$endurance)*rate)
    load.game <- darksoulsarmor:::equip.load(setup$endurance, setup$havel.ring, setup$favor.ring, father.mask = mask)

    ## The weights in the game's order, and the running total after each: the game's rounds each
    ## sum to 32 bits (an empty weapon slot adds nothing), paper's doesn't
    items <- c(unname(setup$weapons[darksoulsarmor:::WEAPON.SLOTS]), setup$head, setup$chest, setup$hands, setup$legs)
    weights <- c(
        vapply(items[1:4], weapon.weight.of, numeric(1)),
        piece.weight.of(HEAD.PIECES, setup$head), piece.weight.of(CHEST.PIECES, setup$chest),
        piece.weight.of(HANDS.PIECES, setup$hands), piece.weight.of(LEGS.PIECES, setup$legs)
    )
    game <- numeric(length(weights))
    total <- 0
    for(i in seq_along(weights)){
        if(i > 4 || items[i] != "None"){
            total <- float32(total + float32(weights[i]))
        }
        game[i] <- total
    }
    steps <- data.table::data.table(SLOT = unname(EQUIP.SLOT.LABELS), ITEM = items, WEIGHT = weights, PAPER_TOTAL = paper.value(cumsum(weights)), GAME_TOTAL = game)

    total.paper <- steps$PAPER_TOTAL[8]
    total.game <- steps$GAME_TOTAL[8]
    lines <- data.table::data.table(
        MOVEMENT = EQUIP.MOVEMENTS,
        PAPER_LINE = paper.value(load.paper*darksoulsarmor:::MOVEMENT.SHARES[EQUIP.MOVEMENTS]),
        GAME_LINE = vapply(EQUIP.MOVEMENTS, function(movement){ darksoulsarmor:::movement.line(load.game, movement) }, numeric(1))
    )
    within.paper <- total.paper <= lines$PAPER_LINE
    list(
        setup = setup, mask = mask,
        load.paper = load.paper, load.game = load.game,
        total.paper = total.paper, total.game = total.game, steps = steps, lines = lines,
        movement.paper = if(any(within.paper)) EQUIP.MOVEMENTS[which(within.paper)[1]] else "Poop",
        movement.game = darksoulsarmor:::movement.type(total.game, load.game),
        ## The line the total is exactly on, on paper (if any)
        on.line = if(any(total.paper == lines$PAPER_LINE)) EQUIP.MOVEMENTS[which(total.paper == lines$PAPER_LINE)[1]] else NA_character_
    )
}

## When the game gives a heavier movement type than paper: the arrangements of the same weapons
## across the four slots (each distinct one once) under which the game agrees with paper, as rows of
## weapons by slot in the page's order. Only the order the weapons are added in changes.
weapon.arrangements <- function(breakdown){
    weapons <- breakdown$setup$weapons[darksoulsarmor:::WEAPON.SLOTS]
    orders <- expand.grid(a = 1:4, b = 1:4, c = 1:4, d = 1:4)
    orders <- orders[apply(orders, 1, function(o){ length(unique(o)) == 4 }), ]
    arranged <- unique(t(apply(orders, 1, function(o){ unname(weapons[o]) })))
    colnames(arranged) <- darksoulsarmor:::WEAPON.SLOTS
    armor <- breakdown$steps$WEIGHT[5:8]
    keeps <- apply(arranged, 1, function(w){
        total <- darksoulsarmor:::total.weight(darksoulsarmor:::weapons.weight(w), armor[1], armor[2], armor[3], armor[4])
        darksoulsarmor:::movement.type(total, breakdown$load.game) == breakdown$movement.paper
    })
    out <- data.table::as.data.table(arranged[keeps, names(EQUIP.WEAPON.LABELS), drop = FALSE])
    data.table::setnames(out, unname(EQUIP.WEAPON.LABELS))
    out
}

## A random setup exactly on a line on paper (Light, Mid or Fat) that the game puts over it (outcome
## "over") or keeps within it ("within"). Settings are drawn at random (Endurance, rings, the Mask, the
## line) until the line is on a tenth on paper - every weight is in tenths, so only then can a
## total sit exactly on it - and such setups exist. The setups are found by following the game's
## running total slot by slot as (exact tenths, 32-bit value) states, counting how many weapon and
## armor choices reach each; one is then drawn, every actual combination equally likely, by walking
## back through the states.
equip.load.example <- function(outcome, tries = 200){
    tenths.of <- function(x){ as.integer(round(10*x)) }
    counted <- function(tenths){ data.table::data.table(t = tenths)[, .(n = .N), by = t] }
    for(try in seq_len(tries)){
        setup <- list(
            endurance = as.numeric(sample(0:99, 1)), havel.ring = sample(c(FALSE, TRUE), 1), favor.ring = sample(c(FALSE, TRUE), 1)
        )
        mask <- sample(c(FALSE, TRUE), 1)
        movement <- sample(EQUIP.MOVEMENTS, 1)
        rate <- prod(c(if(setup$havel.ring) 1.5, if(setup$favor.ring) 1.2, if(mask) 1.05))
        line.paper <- paper.value((40 + setup$endurance)*rate*darksoulsarmor:::MOVEMENT.SHARES[[movement]])
        line.tenths <- tenths.of(line.paper)
        if(abs(10*line.paper - line.tenths) > 1e-6){
            next
        }
        line.game <- darksoulsarmor:::movement.line(darksoulsarmor:::equip.load(setup$endurance, setup$havel.ring, setup$favor.ring, father.mask = mask), movement)

        ## Each slot's choices by weight, in the game's order: a weapon or nothing; then the armor,
        ## the head being the Mask exactly when its bonus is on
        heads <- if(mask) HEAD.PIECES[ARMOR == "Mask of the Father"] else HEAD.PIECES[ARMOR != "Mask of the Father"]
        weapon.choices <- rbind(data.table::data.table(t = 0L, n = 1L), counted(tenths.of(weapon.data$WEIGHT)))
        choices <- list(weapon.choices, weapon.choices, weapon.choices, weapon.choices,
                        counted(tenths.of(heads$WEIGHT)), counted(tenths.of(CHEST.PIECES$WEIGHT)), counted(tenths.of(HANDS.PIECES$WEIGHT)), counted(tenths.of(LEGS.PIECES$WEIGHT)))
        ## Every (state, choice) pair at one slot, and the state it leads to
        step <- function(states, choice){
            i <- rep(seq_len(nrow(states)), each = nrow(choice))
            j <- rep(seq_len(nrow(choice)), times = nrow(states))
            data.table::data.table(
                from = i, choice = j,
                t = states$t[i] + choice$t[j], f = float32(states$f[i] + float32(choice$t[j]/10)), n = states$n[i]*choice$n[j]
            )
        }
        stages <- list(data.table::data.table(t = 0L, f = 0, n = 1))
        for(k in 1:8){
            stages[[k + 1]] <- step(stages[[k]], choices[[k]])[t <= line.tenths, .(n = sum(n)), by = .(t, f)]
        }
        final <- stages[[9]][t == line.tenths]
        final <- if(outcome == "over") final[f > line.game] else final[f <= line.game]
        if(nrow(final) == 0){
            next
        }

        ## Walk back from a final state, drawn by how many combinations reach it
        state <- final[sample.int(nrow(final), 1, prob = final$n)]
        chosen <- integer(8)
        for(k in 8:1){
            ways <- step(stages[[k]], choices[[k]])[t == state$t & f == state$f]
            way <- ways[sample.int(nrow(ways), 1, prob = ways$n)]
            chosen[k] <- choices[[k]]$t[way$choice]
            state <- stages[[k]][way$from]
        }

        ## Real items of the chosen weights, each equally likely
        pick <- function(names, tenths, target){ names <- names[tenths == target]; names[sample.int(length(names), 1)] }
        weapons <- vapply(1:4, function(k){
            if(chosen[k] == 0) "None" else pick(weapon.data$WEAPON, tenths.of(weapon.data$WEIGHT), chosen[k])
        }, character(1))
        setup$weapons <- stats::setNames(weapons, darksoulsarmor:::WEAPON.SLOTS)[names(EQUIP.WEAPON.LABELS)]
        setup$head <- pick(heads$ARMOR, tenths.of(heads$WEIGHT), chosen[5])
        setup$chest <- pick(CHEST.PIECES$ARMOR, tenths.of(CHEST.PIECES$WEIGHT), chosen[6])
        setup$hands <- pick(HANDS.PIECES$ARMOR, tenths.of(HANDS.PIECES$WEIGHT), chosen[7])
        setup$legs <- pick(LEGS.PIECES$ARMOR, tenths.of(LEGS.PIECES$WEIGHT), chosen[8])
        return(setup)
    }
    stop("No example found")
}


## The page: its sidebar (the setup and Calculate) and its readout
equip.load.page <- function(id){
    ns <- shiny::NS(id)
    weapon.input <- function(slot){
        shinyWidgets::pickerInput(
            inputId = ns(paste0("weapon.", slot)),
            label = EQUIP.WEAPON.LABELS[[slot]],
            choices = c("None", weapon.data$WEAPON),
            selected = "None",
            multiple = FALSE,
            options = list(`live-search` = TRUE, size = 10, `show-subtext` = TRUE),
            choicesOpt = list(subtext = c("", sprintf("%.1f", weapon.data$WEIGHT))),
            width = NULL,
            inline = FALSE,
            stateInput = TRUE,
            autocomplete = FALSE
        )
    }
    piece.input <- function(id, label, pieces, none){
        shinyWidgets::pickerInput(
            inputId = ns(id),
            label = label,
            choices = pieces$ARMOR,
            selected = none,
            multiple = FALSE,
            options = list(`live-search` = TRUE, size = 10, `show-subtext` = TRUE),
            choicesOpt = list(subtext = ifelse(pieces$ARMOR == none, "", sprintf("%.1f", pieces$WEIGHT))),
            width = NULL,
            inline = FALSE,
            stateInput = TRUE,
            autocomplete = FALSE
        )
    }
    bslib::layout_sidebar(
        ## The page scrolls rather than squeezing its cards into the window
        fillable = FALSE,
        sidebar = bslib::sidebar(
            width = 360,
            shinyWidgets::autonumericInput(
                inputId = ns("endurance"),
                label = "Endurance Level",
                value = 10,
                align = "right",
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 0,
                maximumValue = 99,
                minimumValue = 0
            ),
            bslib::input_switch(id = ns("havel.ring"), label = "Havel's Ring", value = FALSE, width = NULL),
            bslib::input_switch(id = ns("favor.ring"), label = "Ring of Favor and Protection", value = FALSE, width = NULL),
            shiny::hr(),
            weapon.input("right.1"),
            weapon.input("right.2"),
            weapon.input("left.1"),
            weapon.input("left.2"),
            shiny::hr(),
            piece.input("head", "Head", HEAD.PIECES, "No Head"),
            piece.input("chest", "Chest", CHEST.PIECES, "No Chest"),
            piece.input("hands", "Hands", HANDS.PIECES, "No Hands"),
            piece.input("legs", "Legs", LEGS.PIECES, "No Legs"),
            shiny::hr(),
            shiny::actionButton(inputId = ns("calculate"), label = "Calculate")
        ),
        shiny::div(style = "color: red; font-size: 20px; font-weight: bold;", shiny::textOutput(outputId = ns("message"))),
        shiny::uiOutput(outputId = ns("readout")),
        bslib::card(
            bslib::card_header("Try it: examples right on a threshold"),
            shiny::p(
                "Most players never notice, but the game adds weights in 32-bit floating point, so a setup exactly on a movement line on paper",
                "can land a hair over it - or stay within it. These fill in a random setup exactly on a line on paper and calculate it."
            ),
            shiny::div(
                shiny::actionButton(inputId = ns("random_over"), label = "Random failing example"),
                shiny::actionButton(inputId = ns("random_within"), label = "Random succeeding example")
            )
        )
    )
}

## The page's calculation: on Calculate, or when a random example is filled in
equip.load.server <- function(id){
    shiny::moduleServer(id, function(input, output, session){

        ## The setup the inputs describe: Endurance rounded and kept to 0-99 (a client can send
        ## anything), and anything that isn't one of the choices read as an empty slot
        current.setup <- shiny::reactive({
            endurance <- if(shiny::isTruthy(input$endurance)) min(99, max(0, round(input$endurance, 0))) else NA_real_
            choice <- function(value, choices, empty){ if(length(value) == 1 && value %in% choices) value else empty }
            list(
                endurance = as.numeric(endurance), havel.ring = isTRUE(input$havel.ring), favor.ring = isTRUE(input$favor.ring),
                weapons = vapply(names(EQUIP.WEAPON.LABELS), function(slot){ choice(input[[paste0("weapon.", slot)]], weapon.data$WEAPON, "None") }, character(1)),
                head = choice(input$head, HEAD.PIECES$ARMOR, "No Head"),
                chest = choice(input$chest, CHEST.PIECES$ARMOR, "No Chest"),
                hands = choice(input$hands, HANDS.PIECES$ARMOR, "No Hands"),
                legs = choice(input$legs, LEGS.PIECES$ARMOR, "No Legs")
            )
        })

        ## The setup last calculated
        calculated <- shiny::reactiveVal(NULL)

        shiny::observeEvent(input$calculate, {
            if(!is.na(current.setup()$endurance)){
                calculated(current.setup())
            }
        })

        ## A random example: its setup is calculated straight away, and the inputs are set to it
        fill.example <- function(outcome){
            setup <- equip.load.example(outcome)
            shinyWidgets::updateAutonumericInput(session = session, inputId = "endurance", value = setup$endurance)
            bslib::update_switch(id = "havel.ring", value = setup$havel.ring)
            bslib::update_switch(id = "favor.ring", value = setup$favor.ring)
            for(slot in names(EQUIP.WEAPON.LABELS)){
                shinyWidgets::updatePickerInput(session = session, inputId = paste0("weapon.", slot), selected = setup$weapons[[slot]])
            }
            shinyWidgets::updatePickerInput(session = session, inputId = "head", selected = setup$head)
            shinyWidgets::updatePickerInput(session = session, inputId = "chest", selected = setup$chest)
            shinyWidgets::updatePickerInput(session = session, inputId = "hands", selected = setup$hands)
            shinyWidgets::updatePickerInput(session = session, inputId = "legs", selected = setup$legs)
            calculated(setup)
        }
        shiny::observeEvent(input$random_over, { fill.example("over") })
        shiny::observeEvent(input$random_within, { fill.example("within") })

        output$message <- shiny::renderText({
            if(is.null(calculated())){
                "Choose your setup in the sidebar and click 'Calculate'"
            } else if(!identical(current.setup(), calculated())){
                "Inputs have changed - click 'Calculate' to update the equip load"
            } else{
                ""
            }
        })

        breakdown <- shiny::reactive({
            shiny::req(calculated())
            equip.load.breakdown(calculated())
        })

        ## The readout: the four headline values, a note when the game and paper disagree or the total
        ## is exactly on a line, then the step-by-step weights and the movement lines
        output$readout <- shiny::renderUI({
            b <- breakdown()
            line.of <- function(movement){ b$lines[MOVEMENT == movement] }
            note <- NULL
            if(b$movement.game != b$movement.paper){
                line <- line.of(b$movement.paper)
                note <- bslib::card(
                    bslib::card_header("The game disagrees with paper"),
                    shiny::p(sprintf(
                        "On paper your total, %s, is within the %s line, %s. But the game works in 32-bit floating point: it adds your weights one at a time in a fixed order (left hand 1, right hand 1, left hand 2, right hand 2, then head, chest, hands, legs), rounding each sum, and its equip load's multipliers aren't exact either. Its total, %s, is over its %s line, %s, so it gives %s.",
                        paper.text(b$total.paper), b$movement.paper, paper.text(line$PAPER_LINE), game.text(b$total.game), b$movement.paper, game.text(line$GAME_LINE), b$movement.game
                    )),
                    if(nrow(arrangements()) > 0){
                        shiny::tagList(
                            shiny::p(sprintf("The order matters: with the same weapons in other slots, the game agrees with paper (%s):", b$movement.paper)),
                            shiny::tableOutput(outputId = session$ns("arrangements"))
                        )
                    } else{
                        shiny::p("No arrangement of your weapons across the four slots changes this.")
                    }
                )
            } else if(!is.na(b$on.line)){
                line <- line.of(b$on.line)
                note <- bslib::card(
                    bslib::card_header(sprintf("Exactly on the %s line", b$on.line)),
                    shiny::p(sprintf(
                        "On paper your total, %s, is exactly the %s line. The game, adding up in 32-bit floating point, agrees: its total, %s, is within its line, %s.",
                        paper.text(b$total.paper), b$on.line, game.text(b$total.game), game.text(line$GAME_LINE)
                    ))
                )
            }
            shiny::tagList(
                bslib::layout_column_wrap(
                    width = 1/4,
                    bslib::value_box(title = "Equip load", value = paper.text(b$load.paper), shiny::p(paste("in the game:", game.text(b$load.game)))),
                    bslib::value_box(title = "Weight", value = paper.text(b$total.paper), shiny::p(paste("in the game:", game.text(b$total.game)))),
                    bslib::value_box(title = "Equip percentage", value = sprintf("%.2f%%", 100*b$total.paper/b$load.paper),
                                     shiny::p(sprintf("from the game's values: %.7f%%", 100*b$total.game/b$load.game))),
                    bslib::value_box(title = "Movement", value = b$movement.game,
                                     shiny::p(if(b$movement.game == b$movement.paper) "the same on paper" else paste("on paper:", b$movement.paper)))
                ),
                note,
                bslib::card(
                    bslib::card_header("Step by step, in the order the game adds the weights"),
                    shiny::tableOutput(outputId = session$ns("steps"))
                ),
                bslib::card(
                    bslib::card_header(if(b$mask) "Movement lines (with the Mask of the Father's bonus)" else "Movement lines"),
                    shiny::tableOutput(outputId = session$ns("lines"))
                )
            )
        })

        ## When the game disagrees with paper: the weapon arrangements under which it agrees
        arrangements <- shiny::reactive({
            b <- breakdown()
            if(b$movement.game != b$movement.paper) weapon.arrangements(b) else NULL
        })
        output$arrangements <- shiny::renderTable({ shiny::req(arrangements()) })

        ## Each slot's item and weight (none shown for an empty slot), and the running total
        output$steps <- shiny::renderTable({
            breakdown()$steps[, .(
                SLOT, ITEM,
                WEIGHT = ifelse(ITEM == "None" | ITEM %in% c("No Head", "No Chest", "No Hands", "No Legs"), "", sprintf("%.1f", WEIGHT)),
                PAPER_TOTAL = vapply(PAPER_TOTAL, paper.text, character(1)),
                GAME_TOTAL = game.text(GAME_TOTAL)
            )]
        })

        ## Each movement type's line, on paper and in the game, and the game's total against it
        output$lines <- shiny::renderTable({
            b <- breakdown()
            b$lines[, .(
                MOVEMENT,
                LINE = vapply(PAPER_LINE, paper.text, character(1)),
                GAME_LINE = game.text(GAME_LINE),
                ## Signed, but a total exactly on the line is simply 0
                GAME_TOTAL_VS_LINE = ifelse(b$total.game == GAME_LINE, game.text(0), sprintf("%+.7f", b$total.game - GAME_LINE)),
                WITHIN = ifelse(b$total.game <= GAME_LINE, "Yes", "No")
            )]
        })

        list(calculated = calculated, breakdown = breakdown)

    })
}
