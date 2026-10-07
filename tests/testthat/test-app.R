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
minima.inputs <- function(minimum.values, minima.ids){ setNames(as.list(minimum.values$minima), minima.ids) }
weight.inputs <- function(weight.values, weight.ids){ setNames(as.list(weight.values$weights), weight.ids) }

test_that("submitting every modal unchanged after a refresh keeps the results current", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(go = 1)
        expect_equal(output$refreshmessage, "")

        submit.modal(session, "filters", "dismiss_filter_modal", filter.inputs(filter.values), 1)
        submit.modal(session, "upgrades", "dismiss_upgrade_modal", list(regular.level = upgrade.values$regular.level, twinkling.level = upgrade.values$twinkling.level), 1)
        submit.modal(session, "rings", "dismiss_ring_modal", list(havel.ring = ring.values$havel.ring, favor.ring = ring.values$favor.ring, wolf.ring = ring.values$wolf.ring), 1)
        submit.modal(session, "constraints", "dismiss_constraint_modal", list(roll = constraint.values$roll, unarmored.weight = constraint.values$unarmored.weight, endurance.level = constraint.values$endurance.level), 1)
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

        submit.modal(session, "constraints", "dismiss_constraint_modal", list(roll = "Mid", unarmored.weight = constraint.values$unarmored.weight, endurance.level = constraint.values$endurance.level), click)
        check.edit("roll", "Mid")

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
        session$setInputs(go = 1)
        ## An unsaved edit afterwards isn't part of the download
        submit.modal(session, "constraints", "dismiss_constraint_modal", list(roll = "Mid", unarmored.weight = 10, endurance.level = 10), 1)

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
        expect_equal(setting("Roll Type"), "Fast")
        expect_equal(setting("Wolf Ring"), "Yes")
        expect_equal(setting("Havel's Ring"), "No")
        expect_equal(setting("Minimum POISE"), "30")
        expect_equal(setting("Minimum PHYS_DEF"), "0")
        expect_equal(setting("Score Weight PHYS_DEF"), "16%")
        expect_equal(setting("Score Weight CURSE_RES"), "4%")
        expect_equal(setting("Head"), paste(armordata()$args$head.filter, collapse = "; "))
        ## Only the settings of the search itself: the rest is in the data
        expect_equal(nrow(settings), 16 + 12 + 10)

        ## A curve the tab already holds is reused, and the score's table has its own columns
        session$setInputs(tradeoff_metric = "SCORE", main_tabs = "Trade-offs")
        expect_equal(tradeoff.computations(), 2)
        file <- output$download
        expect_equal(tradeoff.computations(), 2)
        trade.offs <- readxl::read_xlsx(file, sheet = "Trade-offs")
        expect_equal(names(trade.offs), c("ARMOR_WEIGHT_LIMIT", "SCORE_RAW", "SCORE_QUALITY", "ROLL", "HEAD", "CHEST", "HANDS", "LEGS"))
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

## The Trade-offs tab charts every armor weight, computed in one segment per roll class (each under
## that roll type's load limit, so the Mask of the Father's bonus is credited as it would be), with
## lines where the roll class changes. With endurance 40, no load rings and 12 gear weight, the
## equip load is 80: Fast ends at 25% - 12 = 8 armor weight, Mid at 28, Fat at 68 - past the
## heaviest possible armor (52.5), so there's no overloaded segment.
test_that("the Trade-offs tab stitches per-roll-class curves over every armor weight", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        session$setInputs(constraints = 1)
        session$setInputs(roll = "Mid", unarmored.weight = 12, endurance.level = 40)
        session$setInputs(dismiss_constraint_modal = 1)
        session$setInputs(rings = 1)
        session$setInputs(havel.ring = FALSE, favor.ring = FALSE, wolf.ring = TRUE)
        session$setInputs(dismiss_ring_modal = 1)

        session$setInputs(go = 1)
        session$setInputs(tradeoff_metric = "POISE", main_tabs = "Trade-offs")
        result <- tradeoffdata()
        expect_equal(output$errormessage, "")
        expect_equal(unname(result$lines), c(8, 28, 68))
        expect_equal(result$selected.roll, "Mid")
        expect_equal(range(result$data$ARMOR_WEIGHT_LIMIT), c(0, 52.5))
        ## Every 0.1 armor weight, meeting at the lines
        expect_true(all(abs(diff(result$data$ARMOR_WEIGHT_LIMIT) - 0.1) < 1e-9))
        expect_true(all(c(8, 28) %in% round(result$data$ARMOR_WEIGHT_LIMIT, 9)))

        ## Each segment is exactly get.armor.tradeoffs under that segment's roll type
        settings <- list(metric = "POISE", endurance.level = 40, unarmored.weight = 12, wolf.ring = TRUE)
        segment <- function(roll, from, to){ do.call(get.armor.tradeoffs, c(settings, list(roll = roll, weight.step = 0.1, min.armor.weight = from, max.armor.weight = to)))$data }
        expected <- rbind(segment("Fast", 0, 8), segment("Mid", 8, 28)[ARMOR_WEIGHT_LIMIT > 8], segment("Fat", 28, 52.5)[ARMOR_WEIGHT_LIMIT > 28])
        cols <- names(expected)
        expect_equal(result$data[, ..cols], expected)
        expect_equal(result$data$ROLL_LIMIT, rep(c("Fast", "Mid", "Fat"), c(81, 200, 245)))
        expect_no_error(output$tradeoff_table)
        ## Points are hovered and clicked by weight alone
        expect_match(output$tradeoff_plot, '"hovermode":"x"', fixed = TRUE)

        ## The table: limit, best value named for the stat, roll, scores, pieces
        table <- tradeoff.table(result)
        expect_identical(names(table), c("ARMOR_WEIGHT_LIMIT", "POISE", "ROLL", "SCORE_RAW", "SCORE_QUALITY", "HEAD", "CHEST", "HANDS", "LEGS"))
        expect_equal(table$POISE, result$data$BEST_VALUE)

        ## Every point's roll class is from its own weight plus the gear weight
        expected.class <- roll.class(expected$ARMOR_WEIGHT, expected$HEAD == "Mask of the Father", 12, 80)
        expect_equal(sub(" [(].*", "", result$data$ROLL), expected.class$class)

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
        expect_identical(names(tradeoff.table(tradeoffdata())), c("ARMOR_WEIGHT_LIMIT", "SCORE_RAW", "SCORE_QUALITY", "ROLL", "HEAD", "CHEST", "HANDS", "LEGS"))
        expect_no_error(output$tradeoff_table)
    })
})

## Both tabs describe the last successful refresh. The chart is computed only once its tab is open
## after a refresh, from that refresh's settings (never unsaved sidebar edits); it's recomputed when
## a refresh changes the settings or Maximize changes, and kept - only the emphasized roll
## line moving - when a refresh changes just the table size or the roll type. A failed refresh
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

        ## A refresh changing only the roll type and the table size keeps the curve
        curve <- tradeoffdata()$data
        expect_equal(tradeoffdata()$selected.roll, "Fast")
        submit.modal(session, "constraints", "dismiss_constraint_modal", list(roll = "Fat", unarmored.weight = constraint.values$unarmored.weight, endurance.level = constraint.values$endurance.level), 1)
        submit.modal(session, "filters", "dismiss_filter_modal", utils::modifyList(filter.inputs(filter.values), list(max.table.size = 50)), 1)
        session$setInputs(go = 3)
        expect_equal(output$errormessage, "")
        expect_equal(nrow(armordata()$data), 50)
        expect_equal(armordata()$args$roll, "Fat")
        expect_equal(tradeoff.computations(), 4)
        expect_equal(tradeoffdata()$selected.roll, "Fat")
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
## draws it as a reference line instead; every other minimum still applies. With the defaults the
## Fast segment runs from 0 to 2.5 armor weight.
test_that("the Trade-offs chart ignores only the charted stat's own minimum", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        values <- minima.inputs(minimum.values, minima.ids)
        values[[metric.input.id("POISE", "minima")]] <- 30
        values[[metric.input.id("BLEED_RES", "minima")]] <- 20
        submit.modal(session, "minima", "dismiss_minimum_modal", values, 1)
        session$setInputs(go = 1)
        expect_equal(armordata()$args$minima[minima.index.of("POISE")], 30)

        ## Charting poise: its minimum is ignored, bleed's applies
        session$setInputs(tradeoff_metric = "POISE", main_tabs = "Trade-offs")
        result <- tradeoffdata()
        expect_equal(result$stat.minimum, 30)
        expect_equal(result$lines[["Fast"]], 2.5)
        fast <- get.armor.tradeoffs(metric = "POISE", minima = c(BLEED_RES = 20), weight.step = 0.1, max.armor.weight = 2.5)$data
        expect_equal(result$data[seq_len(nrow(fast)), names(fast), with = FALSE], fast)
        expect_true(any(result$data$BEST_VALUE < 30, na.rm = TRUE))
        expect_true(grepl("Your minimum: 30", output$tradeoff_plot, fixed = TRUE))

        ## Charting bleed resistance: the other way around
        session$setInputs(tradeoff_metric = "BLEED_RES")
        result <- tradeoffdata()
        expect_equal(result$stat.minimum, 20)
        fast <- get.armor.tradeoffs(metric = "BLEED_RES", minima = c(POISE = 30), weight.step = 0.1, max.armor.weight = 2.5)$data
        expect_equal(result$data[seq_len(nrow(fast)), names(fast), with = FALSE], fast)
        expect_true(grepl("Your minimum: 20", output$tradeoff_plot, fixed = TRUE))

        ## The score has no minimum of its own: both apply, and there's no reference line
        session$setInputs(tradeoff_metric = "SCORE")
        result <- tradeoffdata()
        expect_equal(result$stat.minimum, 0)
        fast <- get.armor.tradeoffs(metric = "SCORE", minima = c(POISE = 30, BLEED_RES = 20), weight.step = 0.1, max.armor.weight = 2.5)$data
        expect_equal(result$data[seq_len(nrow(fast)), names(fast), with = FALSE], fast)
        expect_false(grepl("Your minimum", output$tradeoff_plot, fixed = TRUE))
    })
})

test_that("roll classes account for the Mask of the Father's bonus", {
    shiny::testServer(system.file("shiny", package = "darksoulsarmor"), {
        ## Equip load 80, gear 12: without the Mask, Fast ends at 8 armor weight; with it, at 9
        r <- roll.class(c(8, 8.5, 8.5, 9.0, 28, 30, 70), c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), 12, 80)
        expect_equal(r$class, c("Fast", "Mid", "Fast", "Fast", "Mid", "Fat", "Overloaded"))
        expect_equal(r$by.mask.bonus, c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
    })
})
