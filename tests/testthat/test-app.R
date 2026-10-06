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
