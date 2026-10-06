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

        downloaded <- data.table::fread(output$download)
        expect_equal(names(downloaded), names(armordata()$data))
        expect_equal(nrow(downloaded), nrow(armordata()$data))
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
