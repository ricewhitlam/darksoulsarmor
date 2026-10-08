## Drives the installed app's server with shiny::testServer. The refresh-message observer runs
## if(all(...)) over every inputs.unchanged flag, so each flag must always be a single TRUE/FALSE:
## an NA there errors inside an observer, which ends the whole Shiny session. All-zero score
## weights used to produce exactly that (their normalization is 0/0 = NaN) when submitted after
## a refresh, via the Score Inputs modal's Done button.
test_that("all-zero score weights submitted after a refresh don't crash the app", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)
        rows.before <- nrow(armordata()$data)
        expect_gt(rows.before, 0)

        ## Resubmitting the weights the last refresh used reads as unchanged
        session$setInputs(weights = 1)
        do.call(session$setInputs, setNames(as.list(weight.values$weights), weight.ids))
        session$setInputs(dismiss_weight_modal = 1)
        expect_true(inputs.unchanged$weight.values)
        expect_equal(output$refreshmessage, "")

        ## All-zero weights read as changed rather than NA, and the session survives
        session$setInputs(weights = 2)
        do.call(session$setInputs, setNames(as.list(rep(0, length(weight.ids))), weight.ids))
        session$setInputs(dismiss_weight_modal = 2)
        expect_false(inputs.unchanged$weight.values)
        expect_match(output$refreshmessage, "Inputs have changed")

        ## Refreshing with them reports the invalid weights and keeps the previous results
        session$setInputs(go = 2)
        expect_match(output$errormessage, "weights")
        expect_equal(nrow(armordata()$data), rows.before)
    })
})

## The Max Table Size widget only limits its value to 1-100,000 in the browser - a client can send
## any value (e.g. Shiny.setInputValue from the browser console), and 5,000,000 once produced a
## 48 second refresh holding 840 MB in that session. The server must clamp it itself.
test_that("max.table.size is clamped to 1-100,000 on the server", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        ## Opening the modal binds every filter widget, which sends its current value along with
        ## whatever the client chooses to send for max.table.size
        submit.max.table.size <- function(value, click){
            session$setInputs(filters = click)
            session$setInputs(
                max.table.size = value,
                starting.class = filter.values$starting.class,
                areas.completed = filter.values$areas.completed,
                upgrade.types = filter.values$upgrade.types,
                head.filter = filter.values$head.filter,
                chest.filter = filter.values$chest.filter,
                hands.filter = filter.values$hands.filter,
                legs.filter = filter.values$legs.filter
            )
            session$setInputs(dismiss_filter_modal = click)
        }

        submit.max.table.size(5e6, 1)
        expect_equal(filter.values$max.table.size, 100000)

        submit.max.table.size(-5, 2)
        expect_equal(filter.values$max.table.size, 1)

        session$setInputs(go = 1)
        expect_equal(armordata()$args$max.table.size, 1)
        expect_equal(nrow(armordata()$data), 1)
    })
})

## Each sidebar button opens a modal whose widgets the browser binds with the current settings;
## the user edits some and clicks Done. This helper replays that: open the modal, send every
## widget's value (`values`, a named list), then click Done. Action button values must keep
## increasing to register as new clicks, hence `click`.
submit.modal <- function(session, open.id, dismiss.id, values, click){
    do.call(session$setInputs, setNames(list(click), open.id))
    do.call(session$setInputs, values)
    do.call(session$setInputs, setNames(list(click), dismiss.id))
}

## The current settings of each modal, as the browser would send them on opening it
filter.inputs <- function(filter.values){
    list(
        max.table.size = filter.values$max.table.size, starting.class = filter.values$starting.class,
        areas.completed = filter.values$areas.completed, upgrade.types = filter.values$upgrade.types,
        head.filter = filter.values$head.filter, chest.filter = filter.values$chest.filter,
        hands.filter = filter.values$hands.filter, legs.filter = filter.values$legs.filter
    )
}
## The Load Inputs modal's widgets: movement, a dropdown per weapon slot, and Endurance
constraint.inputs <- function(movement, weapons, endurance.level){
    list(
        movement = movement,
        weapon.left.1 = weapons[["left.1"]], weapon.right.1 = weapons[["right.1"]],
        weapon.left.2 = weapons[["left.2"]], weapon.right.2 = weapons[["right.2"]],
        endurance.level = endurance.level
    )
}
no.weapons <- c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None")
minima.inputs <- function(minimum.values, minima.ids){ setNames(as.list(minimum.values$minima), minima.ids) }
weight.inputs <- function(weight.values, weight.ids){ setNames(as.list(weight.values$weights), weight.ids) }

test_that("submitting every modal unchanged after a refresh keeps the results current", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)
        expect_equal(output$refreshmessage, "")

        submit.modal(session, "filters", "dismiss_filter_modal", filter.inputs(filter.values), 1)
        submit.modal(session, "upgrades", "dismiss_upgrade_modal", list(regular.level = upgrade.values$regular.level, twinkling.level = upgrade.values$twinkling.level), 1)
        submit.modal(session, "rings", "dismiss_ring_modal", list(havel.ring = ring.values$havel.ring, favor.ring = ring.values$favor.ring, wolf.ring = ring.values$wolf.ring), 1)
        submit.modal(session, "constraints", "dismiss_constraint_modal", constraint.inputs(constraint.values$movement, constraint.values$weapons, constraint.values$endurance.level), 1)
        submit.modal(session, "minima", "dismiss_minimum_modal", minima.inputs(minimum.values, minima.ids), 1)
        submit.modal(session, "weights", "dismiss_weight_modal", weight.inputs(weight.values, weight.ids), 1)

        expect_true(all(unlist(shiny::reactiveValuesToList(inputs.unchanged))))
        expect_equal(output$refreshmessage, "")
    })
})

test_that("an edit in each modal is flagged, then applied by the next refresh", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)
        click <- 1

        ## One edit per modal: the message appears, and the refresh passes the new value on
        check.edit <- function(arg.name, expected){
            expect_match(output$refreshmessage, "Inputs have changed", info = arg.name)
            click <<- click + 1
            session$setInputs(go = click)
            expect_equal(output$refreshmessage, "", info = arg.name)
            expect_equal(armordata()$args[[arg.name]], expected, info = arg.name)
        }

        values <- filter.inputs(filter.values)
        values$starting.class <- "Knight"
        submit.modal(session, "filters", "dismiss_filter_modal", values, click)
        check.edit("starting.class", "Knight")

        submit.modal(session, "upgrades", "dismiss_upgrade_modal", list(regular.level = "+5", twinkling.level = upgrade.values$twinkling.level), click)
        check.edit("regular.level", "+5")

        submit.modal(session, "rings", "dismiss_ring_modal", list(havel.ring = TRUE, favor.ring = ring.values$favor.ring, wolf.ring = ring.values$wolf.ring), click)
        check.edit("havel.ring", TRUE)

        submit.modal(session, "constraints", "dismiss_constraint_modal", constraint.inputs("Mid", constraint.values$weapons, constraint.values$endurance.level), click)
        check.edit("movement", "Mid")

        values <- minima.inputs(minimum.values, minima.ids)
        values$minphysdef <- 50
        submit.modal(session, "minima", "dismiss_minimum_modal", values, click)
        check.edit("minima", c(50, rep(0, 11)))
        expect_true(all(armordata()$data$PHYS_DEF >= 50))

        values <- weight.inputs(weight.values, weight.ids)
        values$physdefweight <- 50
        submit.modal(session, "weights", "dismiss_weight_modal", values, click)
        new.weights <- c(50, 16, 16, 16, 8, 8, 8, 4, 4, 4)
        check.edit("weights", new.weights/sum(new.weights))
    })
})

test_that("empty selections become 'No <slot>' and empty area/upgrade-type lists are honored", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)

        values <- filter.inputs(filter.values)
        values$head.filter <- character(0)
        values$chest.filter <- character(0)
        values$hands.filter <- character(0)
        values$legs.filter <- character(0)
        submit.modal(session, "filters", "dismiss_filter_modal", values, 1)
        session$setInputs(go = 2)
        expect_equal(filter.values$head.filter, "No Head")
        expect_equal(nrow(armordata()$data), 1)
        expect_equal(as.character(unlist(armordata()$data[1, .(HEAD, CHEST, HANDS, LEGS)])), c("No Head", "No Chest", "No Hands", "No Legs"))

        ## No upgrade types: only the always-allowed "No <slot>" pieces remain
        values <- filter.inputs(filter.values)
        values$head.filter <- head.data.unupgraded$ARMOR
        values$chest.filter <- chest.data.unupgraded$ARMOR
        values$hands.filter <- hands.data.unupgraded$ARMOR
        values$legs.filter <- legs.data.unupgraded$ARMOR
        values$upgrade.types <- character(0)
        submit.modal(session, "filters", "dismiss_filter_modal", values, 2)
        session$setInputs(go = 3)
        expect_equal(armordata()$args$upgrade.types, character(0))
        expect_equal(nrow(armordata()$data), 1)

        ## No areas completed: only the starting class's pieces and always-available pieces remain
        values$upgrade.types <- c("Regular", "Twinkling", "None")
        values$areas.completed <- character(0)
        submit.modal(session, "filters", "dismiss_filter_modal", values, 3)
        session$setInputs(go = 4)
        expect_equal(armordata()$args$areas.completed, character(0))
        expect_gt(nrow(armordata()$data), 0)
        available <- function(dt, pieces){ all(pieces %in% dt[STARTING_CLASS == "Warrior" | AREA_MATCH_TYPE == "ALWAYS", ARMOR]) }
        expect_true(available(head.data.unupgraded, as.character(armordata()$data$HEAD)))
        expect_true(available(chest.data.unupgraded, as.character(armordata()$data$CHEST)))
        expect_true(available(hands.data.unupgraded, as.character(armordata()$data$HANDS)))
        expect_true(available(legs.data.unupgraded, as.character(armordata()$data$LEGS)))
    })
})

test_that("row links, the results table, the download, and the User Guide all work", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)
        expect_no_error(output$table)
        expect_no_error(session$setInputs(guide = 1))

        ## A row whose pieces all have wiki links
        row <- which(armordata()$data$HEAD != "No Head")[1]
        session$setInputs(table_rows_selected = row)
        head.link <- head.data.unupgraded[ARMOR == as.character(armordata()$data$HEAD[row]), LINK]
        expect_match(output$tabhead$html, head.link, fixed = TRUE)

        ## A "No Head" row: its link is "N/A", so no head link is shown
        values <- filter.inputs(filter.values)
        values$head.filter <- character(0)
        submit.modal(session, "filters", "dismiss_filter_modal", values, 1)
        session$setInputs(go = 2)
        expect_no_error(session$setInputs(table_rows_selected = 2))
        expect_match(output$tabchest$html, "Chest: ", fixed = TRUE)
    })
})

## The User Guide describes the current app: the refresh driving both tabs, the workbook download,
## movement types, and the Trade-offs tab - not controls that no longer exist.
test_that("the User Guide describes the current app", {
    modals <- character(0)
    local_mocked_bindings(
        showModal = function(ui, ...){
            modals <<- c(modals, as.character(ui))
        },
        .package = "shiny"
    )
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(guide = 1)
    })
    expect_length(modals, 1)
    guide <- modals[1]
    for(text in c("Both tabs, Results and Trade-offs, show the settings of the last refresh", "saves an Excel workbook",
                  "'Movement' is used to specify", "Poop (no limit", "Score Inputs:", "Fat | Poop", "A minimum on the charted stat itself is ignored",
                  "or on a point or row in the Trade-offs tab", "with the game's own upgrade rates, exactly as the game computes them", "The download holds the exact values", "Right Hand Weapon 1' through 'Left Hand Weapon 2' are the weapons", "given your equip load and weapons", "install_github(\"ricewhitlam/darksoulsarmor\")")){
        expect_match(guide, text, fixed = TRUE)
    }
    for(text in c("Compute Trade-offs", "Roll Type", "Weight Inputs", "Overloaded", "Fast", "Detail", "approximated", "Weight without Armor", "weigh slightly more than the weight it's charted at")){
        expect_false(grepl(text, guide, fixed = TRUE), info = text)
    }
})

## The app computes with exact values (e.g. strike 5.1499996) and rounds only what it shows: the
## Results table's column filters take their ranges from the data it's given, so it's given values
## already at display precision (raw 32-bit values made those filters show ~14 decimals), and the
## chart's hover shows a defense to 1 decimal. The download and the links keep the exact values.
test_that("the app keeps exact values and shows them at display precision", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)
        strike <- armordata()$data$STRIKE_DEF
        expect_false(all(strike == round(strike, 1)))
        filters <- jsonlite::fromJSON(output$table, simplifyVector = FALSE)$x$filterHTML
        scales <- as.numeric(sub('data-scale="([0-9]+)"', "\\1", regmatches(filters, gregexpr('data-scale="[0-9]+"', filters))[[1]]))
        expect_gt(length(scales), 0)
        expect_true(all(scales <= 4))
        bounds <- sub('data-(min|max)="([^"]*)"', "\\2", regmatches(filters, gregexpr('data-(min|max)="[^"]*"', filters))[[1]])
        expect_false(any(grepl("[.][0-9]{5,}", bounds)))

        session$setInputs(tradeoff_metric = "STRIKE_DEF", main_tabs = "Trade-offs")
        shown <- sub("Strike Defense: ", "", regmatches(output$tradeoff_plot, gregexpr("Strike Defense: [^<]*", output$tradeoff_plot))[[1]])
        expect_gt(length(shown), 0)
        expect_true(all(grepl("^[0-9]+[.][0-9]$", shown)))
        expect_true(any(abs(tradeoffdata()$data$BEST_VALUE - round(tradeoffdata()$data$BEST_VALUE, 1)) > 1e-6, na.rm = TRUE))
    })
})

## "Download Armor Data" saves one workbook describing the last refresh: the Results table, the
## Trade-offs table for the current Maximize choice - computed for the download if the tab
## hasn't shown it since that refresh, and reused otherwise - and the settings behind both.
test_that("the download saves the results, trade-offs, and settings of the last refresh", {
    skip_if_not_installed("readxl")
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        ## Nothing to save before the first refresh
        session$setInputs(tradeoff_metric = "POISE", main_tabs = "Results")
        expect_error(output$download, "Refresh Armor Data")

        values <- minima.inputs(minimum.values, minima.ids)
        values[[metric.input.id("POISE", "minima")]] <- 30
        submit.modal(session, "minima", "dismiss_minimum_modal", values, 1)
        submit.modal(session, "rings", "dismiss_ring_modal", list(havel.ring = FALSE, favor.ring = FALSE, wolf.ring = TRUE), 1)
        submit.modal(session, "filters", "dismiss_filter_modal", utils::modifyList(filter.inputs(filter.values), list(max.table.size = 200)), 1)
        submit.modal(session, "constraints", "dismiss_constraint_modal", constraint.inputs("Light", c(left.1 = "Talisman", right.1 = "Claymore", left.2 = "Dagger", right.2 = "None"), 40), 1)
        session$setInputs(go = 1)
        ## An unsaved edit afterwards isn't part of the download
        submit.modal(session, "constraints", "dismiss_constraint_modal", constraint.inputs("Mid", no.weapons, 10), 1)

        ## The Trade-offs tab was never opened, so the download computes the curve (and keeps it)
        expect_null(tradeoffdata())
        file <- output$download
        ## Named for the time of the download
        expect_match(basename(file), "^ds_armor_data_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}[.]xlsx$")
        expect_equal(readxl::excel_sheets(file), c("Results", "Trade-offs", "Settings"))
        expect_equal(tradeoff.computations(), 1)
        expect_equal(tradeoffdata()$metric, "POISE")

        results <- as.data.frame(armordata()$data)
        results[] <- lapply(results, function(x){ if(is.factor(x)) as.character(x) else x })
        expect_equal(as.data.frame(readxl::read_xlsx(file, sheet = "Results")), results)
        expect_equal(nrow(results), 200)
        expect_equal(as.data.frame(readxl::read_xlsx(file, sheet = "Trade-offs")), as.data.frame(tradeoff.table(tradeoffdata())))

        settings <- readxl::read_xlsx(file, sheet = "Settings")
        setting <- function(name){ settings$VALUE[settings$SETTING == name] }
        expect_equal(setting("Max Table Size"), "200")
        expect_equal(setting("Movement"), "Light")
        expect_equal(setting("Wolf Ring"), "Yes")
        expect_equal(setting("Havel's Ring"), "No")
        expect_equal(setting("Minimum POISE"), "30")
        expect_equal(setting("Minimum PHYS_DEF"), "0")
        expect_equal(setting("Score Weight PHYS_DEF"), "16%")
        expect_equal(setting("Score Weight CURSE_RES"), "4%")
        expect_equal(setting("Head"), paste(armordata()$args$head.filter, collapse = "; "))
        ## Movement, Endurance, then the weapons right hand first, as Load Inputs shows them
        first <- match("Movement", settings$SETTING)
        expect_equal(settings$SETTING[first + 0:5], c("Movement", "Endurance Level", "Right Hand Weapon 1", "Right Hand Weapon 2", "Left Hand Weapon 1", "Left Hand Weapon 2"))
        expect_equal(settings$VALUE[first + 1:5], c("40", "Claymore", "None", "Talisman", "Dagger"))
        ## Only the settings of the search itself: the rest is in the data
        expect_equal(nrow(settings), 19 + 12 + 10)

        ## A curve the tab already holds is reused, and the score's table has its own columns
        session$setInputs(tradeoff_metric = "SCORE", main_tabs = "Trade-offs")
        expect_equal(tradeoff.computations(), 2)
        file <- output$download
        expect_equal(tradeoff.computations(), 2)
        trade.offs <- readxl::read_xlsx(file, sheet = "Trade-offs")
        expect_equal(names(trade.offs), c("ARMOR_WEIGHT_LIMIT", "SCORE_RAW", "SCORE_QUALITY", "MOVEMENT", "HEAD", "CHEST", "HANDS", "LEGS"))
        expect_equal(as.data.frame(trade.offs), as.data.frame(tradeoff.table(tradeoffdata())))
    })
})

test_that("'Normalize to 100%' rescales the weights to sum to exactly 100.0 at one decimal", {
    updates <- list()
    local_mocked_bindings(
        updateAutonumericInput = function(session = NULL, inputId, label = NULL, value = NULL, options = NULL){
            updates[[inputId]] <<- value
        },
        .package = "shinyWidgets"
    )
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        raw <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        session$setInputs(weights = 1)
        do.call(session$setInputs, setNames(as.list(raw), weight.ids))
        session$setInputs(normalize_weights = 1)
        normalized <- unlist(updates[weight.ids])
        expect_length(normalized, 10)
        expect_equal(sum(normalized), 100)
        expect_equal(normalized, round(normalized, 1))
        expect_true(all(abs(normalized - 100*raw/sum(raw)) <= 0.1 + 1e-9))
    })
})

## tryCatch(warning = ...) stops at the first warning just like an error does, so any warning
## raised during a refresh (e.g. a dependency's deprecation notice) used to abandon the refresh
## halfway and show the warning as if it were an error. Warnings must let the refresh finish and
## be shown as a notification instead.
test_that("a warning during refresh doesn't abort it, and is shown as a notification", {
    search <- darksoulsarmor::get.optimal.armor.combos
    local_mocked_bindings(
        get.optimal.armor.combos = function(...){
            warning("simulated dependency warning")
            search(...)
        },
        .package = "darksoulsarmor"
    )
    notifications <- character(0)
    local_mocked_bindings(
        showNotification = function(ui, ...){
            notifications <<- c(notifications, as.character(ui))
        },
        .package = "shiny"
    )
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)
        expect_gt(nrow(armordata()$data), 0)
        expect_true(been.refreshed())
        expect_equal(output$errormessage, "")
        expect_match(notifications, "simulated dependency warning")
    })
})

## The Trade-offs tab charts every armor weight, computed in one segment per movement class (each under
## that movement type's load limit, so the Mask of the Father's bonus is credited as it would be), with
## lines where the movement class changes. With endurance 40, no load rings and a Great Club (12), the
## equip load is 80: Light ends at 25% - 12 = 8 armor weight, Mid at 28, Fat at 68 - past the
## heaviest possible armor (52.5), so there's no Poop segment.
test_that("the Trade-offs tab stitches per-movement-class curves over every armor weight", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(constraints = 1)
        do.call(session$setInputs, constraint.inputs("Mid", c(left.1 = "None", right.1 = "Great Club", left.2 = "None", right.2 = "None"), 40))
        session$setInputs(dismiss_constraint_modal = 1)
        session$setInputs(rings = 1)
        session$setInputs(havel.ring = FALSE, favor.ring = FALSE, wolf.ring = TRUE)
        session$setInputs(dismiss_ring_modal = 1)

        session$setInputs(go = 1)
        session$setInputs(tradeoff_metric = "POISE", main_tabs = "Trade-offs")
        result <- tradeoffdata()
        expect_equal(output$errormessage, "")
        expect_equal(unname(result$lines), c(8, 28, 68))
        expect_equal(result$selected.movement, "Mid")
        expect_equal(range(result$data$ARMOR_WEIGHT_LIMIT), c(0, 52.5))
        ## Every 0.1 armor weight, meeting at the lines
        expect_true(all(abs(diff(result$data$ARMOR_WEIGHT_LIMIT) - 0.1) < 1e-9))
        expect_true(all(c(8, 28) %in% round(result$data$ARMOR_WEIGHT_LIMIT, 9)))

        ## Each segment is exactly get.armor.tradeoffs under that segment's movement type
        settings <- list(metric = "POISE", endurance.level = 40, weapons = c(right.1 = "Great Club"), wolf.ring = TRUE)
        segment <- function(movement, from, to){ do.call(get.armor.tradeoffs, c(settings, list(movement = movement, weight.step = 0.1, min.armor.weight = from, max.armor.weight = to)))$data }
        expected <- rbind(segment("Light", 0, 8), segment("Mid", 8, 28)[ARMOR_WEIGHT_LIMIT > 8], segment("Fat", 28, 52.5)[ARMOR_WEIGHT_LIMIT > 28])
        cols <- names(expected)
        expect_equal(result$data[, ..cols], expected)
        expect_equal(result$data$MOVEMENT_LIMIT, rep(c("Light", "Mid", "Fat"), c(81, 200, 245)))
        expect_no_error(output$tradeoff_table)
        ## Points are hovered and clicked by weight alone
        expect_match(output$tradeoff_plot, '"hovermode":"x"', fixed = TRUE)

        ## The table: limit, best value named for the stat, movement, scores, pieces
        table <- tradeoff.table(result)
        expect_identical(names(table), c("ARMOR_WEIGHT_LIMIT", "POISE", "MOVEMENT", "SCORE_RAW", "SCORE_QUALITY", "HEAD", "CHEST", "HANDS", "LEGS"))
        expect_equal(table$POISE, result$data$BEST_VALUE)

        ## Every point's movement class is the game's check on its own set, with the Great Club
        expected.class <- movement.class(expected$HEAD, expected$CHEST, expected$HANDS, expected$LEGS, 12, 80, 84)
        expect_equal(sub(" [(].*", "", result$data$MOVEMENT), expected.class$class)

        ## A table row with a head piece, and a clicked chart point, open their links
        row <- which(!is.na(expected$HEAD) & expected$HEAD != "No Head")[1]
        session$setInputs(tradeoff_table_rows_selected = row)
        expect_match(output$tabhead$html, head.data.unupgraded[ARMOR == expected$HEAD[row], LINK], fixed = TRUE)
        point <- which(!is.na(expected$CHEST) & expected$CHEST != "No Chest")[1]
        session$setInputs(`plotly_click-tradeoffs` = sprintf('[{"curveNumber":0,"pointNumber":%d,"customdata":%d}]', point - 1, point))
        expect_match(output$tabchest$html, chest.data.unupgraded[ARMOR == expected$CHEST[point], LINK], fixed = TRUE)

        ## Another stat: its own value column
        session$setInputs(tradeoff_metric = "MAG_DEF")
        expect_equal(tradeoffdata()$metric, "MAG_DEF")
        expect_identical(names(tradeoff.table(tradeoffdata()))[2], "MAG_DEF")
        expect_no_error(output$tradeoff_table)

        ## With the score as the stat, it's the value column and isn't repeated
        session$setInputs(tradeoff_metric = "SCORE")
        expect_identical(names(tradeoff.table(tradeoffdata())), c("ARMOR_WEIGHT_LIMIT", "SCORE_RAW", "SCORE_QUALITY", "MOVEMENT", "HEAD", "CHEST", "HANDS", "LEGS"))
        expect_no_error(output$tradeoff_table)
    })
})

## Both tabs describe the last successful refresh. The chart is computed only once its tab is open
## after a refresh, from that refresh's settings (never unsaved sidebar edits); it's recomputed when
## a refresh changes the settings or Maximize changes, and kept - only the emphasized movement
## line moving - when a refresh changes just the table size or the movement type. A failed refresh
## changes neither tab.
test_that("the Trade-offs chart follows the last refresh, not unsaved settings", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        ## Before any refresh: nothing to chart, and one message covers both tabs
        session$setInputs(tradeoff_metric = "POISE", main_tabs = "Trade-offs")
        expect_null(tradeoffdata())
        expect_equal(output$refreshmessage, "Adjust settings in the sidebar and click 'Refresh Armor Data' to pull results for both tabs")

        ## A refresh while the Results tab is showing doesn't compute the chart yet
        session$setInputs(main_tabs = "Results")
        session$setInputs(go = 1)
        expect_null(tradeoffdata())
        expect_equal(tradeoff.computations(), 0)

        ## Opening the tab computes it from the refresh's settings (the defaults)
        session$setInputs(main_tabs = "Trade-offs")
        expect_equal(tradeoff.computations(), 1)
        defaults <- get.armor.tradeoffs(metric = "POISE", max.armor.weight = 0)$data
        expect_equal(tradeoffdata()$data[1, names(defaults), with = FALSE], defaults)

        ## Switching tabs back and forth reuses it
        session$setInputs(main_tabs = "Results")
        session$setInputs(main_tabs = "Trade-offs")
        expect_equal(tradeoff.computations(), 1)

        ## An unsaved edit flags both tabs as stale, and charts computed meanwhile still use the
        ## last refresh's settings
        submit.modal(session, "rings", "dismiss_ring_modal", list(havel.ring = FALSE, favor.ring = FALSE, wolf.ring = TRUE), 1)
        expect_equal(output$refreshmessage, "Inputs have changed - click 'Refresh Armor Data' to update both tabs")
        session$setInputs(tradeoff_metric = "SCORE")
        session$setInputs(tradeoff_metric = "POISE")
        expect_equal(tradeoff.computations(), 3)
        expect_equal(tradeoffdata()$data$BEST_VALUE[1], defaults$BEST_VALUE)

        ## Refreshing applies the edit to the chart too
        session$setInputs(go = 2)
        expect_equal(output$refreshmessage, "")
        expect_equal(tradeoff.computations(), 4)
        expect_true(tradeoffdata()$key$settings$wolf.ring)
        expect_equal(tradeoffdata()$data$BEST_VALUE[1], get.armor.tradeoffs(metric = "POISE", max.armor.weight = 0, wolf.ring = TRUE)$data$BEST_VALUE)

        ## A refresh changing only the movement type and the table size keeps the curve
        curve <- tradeoffdata()$data
        expect_equal(tradeoffdata()$selected.movement, "Light")
        submit.modal(session, "constraints", "dismiss_constraint_modal", constraint.inputs("Fat", constraint.values$weapons, constraint.values$endurance.level), 1)
        submit.modal(session, "filters", "dismiss_filter_modal", utils::modifyList(filter.inputs(filter.values), list(max.table.size = 50)), 1)
        session$setInputs(go = 3)
        expect_equal(output$errormessage, "")
        expect_equal(nrow(armordata()$data), 50)
        expect_equal(armordata()$args$movement, "Fat")
        expect_equal(tradeoff.computations(), 4)
        expect_equal(tradeoffdata()$selected.movement, "Fat")
        expect_identical(tradeoffdata()$data, curve)
        expect_no_error(output$tradeoff_plot)

        ## A failed refresh (all-zero score weights) changes neither tab
        submit.modal(session, "weights", "dismiss_weight_modal", setNames(as.list(rep(0, length(weight.ids))), weight.ids), 1)
        session$setInputs(go = 4)
        expect_match(output$errormessage, "weights")
        expect_equal(nrow(armordata()$data), 50)
        expect_equal(tradeoff.computations(), 4)
        expect_identical(tradeoffdata()$data, curve)
    })
})

## A minimum on the charted stat would only cut the curve off below it, so the chart ignores it and
## draws it as a reference line instead; every other minimum still applies. With a Zweihander (10)
## at the default 10 Endurance, the Light segment runs from 0 to 2.5 armor weight.
test_that("the Trade-offs chart ignores only the charted stat's own minimum", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        values <- minima.inputs(minimum.values, minima.ids)
        values[[metric.input.id("POISE", "minima")]] <- 30
        values[[metric.input.id("BLEED_RES", "minima")]] <- 20
        submit.modal(session, "minima", "dismiss_minimum_modal", values, 1)
        submit.modal(session, "constraints", "dismiss_constraint_modal", constraint.inputs("Light", c(left.1 = "None", right.1 = "Zweihander", left.2 = "None", right.2 = "None"), 10), 1)
        zweihander <- c(right.1 = "Zweihander")
        session$setInputs(go = 1)
        expect_equal(armordata()$args$minima[minima.index.of("POISE")], 30)

        ## Charting poise: its minimum is ignored, bleed's applies
        session$setInputs(tradeoff_metric = "POISE", main_tabs = "Trade-offs")
        result <- tradeoffdata()
        expect_equal(result$stat.minimum, 30)
        expect_equal(result$lines[["Light"]], 2.5)
        light <- get.armor.tradeoffs(metric = "POISE", minima = c(BLEED_RES = 20), weapons = zweihander, weight.step = 0.1, max.armor.weight = 2.5)$data
        expect_equal(result$data[seq_len(nrow(light)), names(light), with = FALSE], light)
        expect_true(any(result$data$BEST_VALUE < 30, na.rm = TRUE))
        expect_true(grepl("Your minimum: 30", output$tradeoff_plot, fixed = TRUE))

        ## Charting bleed resistance: the other way around
        session$setInputs(tradeoff_metric = "BLEED_RES")
        result <- tradeoffdata()
        expect_equal(result$stat.minimum, 20)
        light <- get.armor.tradeoffs(metric = "BLEED_RES", minima = c(POISE = 30), weapons = zweihander, weight.step = 0.1, max.armor.weight = 2.5)$data
        expect_equal(result$data[seq_len(nrow(light)), names(light), with = FALSE], light)
        expect_true(grepl("Your minimum: 20", output$tradeoff_plot, fixed = TRUE))

        ## The score has no minimum of its own: both apply, and there's no reference line
        session$setInputs(tradeoff_metric = "SCORE")
        result <- tradeoffdata()
        expect_equal(result$stat.minimum, 0)
        light <- get.armor.tradeoffs(metric = "SCORE", minima = c(POISE = 30, BLEED_RES = 20), weapons = zweihander, weight.step = 0.1, max.armor.weight = 2.5)$data
        expect_equal(result$data[seq_len(nrow(light)), names(light), with = FALSE], light)
        expect_false(grepl("Your minimum", output$tradeoff_plot, fixed = TRUE))
    })
})

## With only the Mask of the Father allowed for the head, every set wears it, so just past the Light
## line (8 armor weight here, with a Great Club at 40 Endurance) the best sets are still Light thanks
## to its bonus (its Light line is 9). The class says so in full in the table, abbreviated in the
## hover.
test_that("Trade-offs points kept light by the Mask of the Father say so", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        submit.modal(session, "constraints", "dismiss_constraint_modal", constraint.inputs("Mid", c(left.1 = "None", right.1 = "Great Club", left.2 = "None", right.2 = "None"), 40), 1)
        submit.modal(session, "filters", "dismiss_filter_modal", utils::modifyList(filter.inputs(filter.values), list(head.filter = "Mask of the Father")), 1)
        session$setInputs(go = 1)
        session$setInputs(tradeoff_metric = "POISE", main_tabs = "Trade-offs")
        result <- tradeoffdata()
        expect_true("Light (Mask of the Father bonus)" %in% result$data$MOVEMENT)
        expect_match(output$tradeoff_plot, "Movement: Light (MotF bonus)", fixed = TRUE)
        expect_false(grepl("Mask of the Father bonus", output$tradeoff_plot, fixed = TRUE))
    })
})

## The Trade-offs tab's movement classes are the game's check on each set. At 60 Endurance with the
## Ring of Favor (load 120, 126 with the Mask), the in-game T1 set - Mask of the Father, 31.5 in all -
## rolls Light only thanks to the Mask (the Light line is 30 without it, 31.5 with it); the same
## weight with Brigand Hood (also 1.2) instead is Mid, as is the Giant set (42.1), and with a
## Dragon Tooth (18) on top, the Giant set is Fat. A point with no set has no class.
test_that("movement classes are the game's check, and note the Mask of the Father's bonus", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        load <- equip.load(60, FALSE, TRUE, father.mask = FALSE)
        load.father.mask <- equip.load(60, FALSE, TRUE, father.mask = TRUE)
        r <- movement.class(
            head = c("Mask of the Father", "Brigand Hood", "Giant Helm", NA),
            chest = c("Armor of Artorias", "Armor of Artorias", "Giant Armor", NA),
            hands = c("Havel's Gauntlets", "Havel's Gauntlets", "Giant Gauntlets", NA),
            legs = c("Steel Leggings", "Steel Leggings", "Giant Leggings", NA),
            carried = 0, load = load, load.father.mask = load.father.mask
        )
        expect_equal(r$class, c("Light", "Mid", "Mid", NA))
        expect_equal(r$by.mask.bonus, c(TRUE, FALSE, FALSE, FALSE))
        heavy <- movement.class("Giant Helm", "Giant Armor", "Giant Gauntlets", "Giant Leggings", carried = 18, load = load, load.father.mask = load.father.mask)
        expect_equal(heavy$class, "Fat")
    })
})

## The Trade-offs tab's efficiency view: the curve simplified by get.tradeoff.efficiency (10%), each
## region shaded by its slope against the curve's average - green at twice it or more, purple at
## flat, grey at the average (half and double equally far from grey)
test_that("the Trade-offs tab shows where extra weight pays off", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        expect_equal(efficiency.color(c(1, 2, 4, 0, 0.5, NA)), c("rgb(189,189,189)", "rgb(27,120,55)", "rgb(27,120,55)", "rgb(118,42,131)", "rgb(118,42,131)", "rgb(189,189,189)"))
        expect_equal(efficiency.color(c(2^0.2, 2^-0.2)), c("rgb(157,175,162)", "rgb(175,160,177)"))
        ## Tinted: mixed with white
        expect_equal(efficiency.color(c(1, 2, 0), tint = 0.35), c("rgb(232,232,232)", "rgb(175,208,185)", "rgb(207,180,212)"))

        session$setInputs(go = 1)
        session$setInputs(tradeoff_metric = "SCORE", main_tabs = "Trade-offs")
        result <- tradeoffdata()
        expect_identical(result$efficiency, get.tradeoff.efficiency(result$data, tolerance = 0.1))
        regions <- result$efficiency$data
        expect_gt(nrow(regions), 1)

        ## Every region shaded behind the curve in its color, and the curve the chart's only
        ## trace, so hovering shows one box
        plot <- jsonlite::fromJSON(output$tradeoff_plot, simplifyVector = FALSE)$x
        expect_length(plot$data, 1)
        bands <- Filter(function(shape) shape$type == "rect", plot$layout$shapes)
        expect_equal(vapply(bands, function(band) band$x0, numeric(1)), regions$FROM)
        expect_equal(vapply(bands, function(band) band$x1, numeric(1)), regions$TO)
        expect_equal(vapply(bands, function(band) band$fillcolor, character(1)), efficiency.color(regions$RATIO_TO_AVERAGE))
        expect_true(all(vapply(bands, function(band) band$opacity, numeric(1)) == 0.35))
        expect_true(all(vapply(bands, function(band) band$layer, character(1)) == "below"))
        ## ...each outlined in white, so there's a gap at every break
        expect_true(all(vapply(bands, function(band) band$line$color, character(1)) == "white"))
        expect_true(all(vapply(bands, function(band) band$line$width, numeric(1)) > 0))
        ## Each point with a set names its one region; where two meet, the one it ends
        hover <- unlist(plot$data[[1]]$text)
        weights <- unlist(plot$data[[1]]$x)
        region.text <- sprintf("Region %.1f-%.1f: ", regions$FROM, regions$TO)
        has.set <- !is.na(result$data$BEST_VALUE)
        expect_true(all(vapply(hover[has.set], function(h) sum(vapply(region.text, grepl, logical(1), x = h, fixed = TRUE)), integer(1)) == 1))
        boundary <- match(regions$TO[1], weights)
        expect_match(hover[boundary], region.text[1], fixed = TRUE)
        expect_match(hover[boundary+1], region.text[2], fixed = TRUE)
        expect_match(hover[boundary], "Score per unit weight", fixed = TRUE)
        ## ...and its hover box takes that region's tint; a point with no set, white
        hover.color <- unlist(plot$data[[1]]$hoverlabel$bgcolor)
        named <- vapply(hover[has.set], function(h) which(vapply(region.text, grepl, logical(1), x = h, fixed = TRUE)), integer(1))
        expect_equal(hover.color[has.set], efficiency.color(regions$RATIO_TO_AVERAGE[named], tint = 0.35), ignore_attr = TRUE)
        expect_true(all(hover.color[!has.set] == "white"))
    })
})
