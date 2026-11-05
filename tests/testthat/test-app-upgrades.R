## Upgrade Inputs on its own (inst/shiny/R/upgrades.R): its dialog shows the saved levels, Done saves
## the chosen ones (an empty choice keeps what was saved), and unchanged says whether the saved levels
## are the last refresh's
source(system.file("shiny", "R", "upgrades.R", package = "darksoulsarmor"), local = TRUE)

test_that("Upgrade Inputs saves its levels on Done and compares them with the last refresh", {
    modals <- character(0)
    local_mocked_bindings(showModal = function(ui, ...){ modals <<- c(modals, as.character(ui)) }, .package = "shiny")
    last.args <- shiny::reactiveVal(NULL)
    shiny::testServer(upgrade.inputs.server, args = list(last.args = last.args), {
        ## +0 for both to start with, and nothing to compare before the first refresh
        expect_identical(c(values$regular.level, values$twinkling.level), c("+0", "+0"))
        expect_true(session$returned$unchanged())
        last.args(list(regular.level = "+0", twinkling.level = "+0", movement = "Light"))
        expect_true(session$returned$unchanged())

        ## The dialog: one choice per kind of armor, under the module's own ids
        session$setInputs(open = 1)
        expect_length(modals, 1)
        expect_match(modals[1], sprintf('id="%s"', session$ns("regular.level")), fixed = TRUE)
        expect_match(modals[1], sprintf('id="%s"', session$ns("twinkling.level")), fixed = TRUE)
        expect_match(modals[1], "Armor Level (Regular)", fixed = TRUE)
        expect_match(modals[1], "Armor Level (Twinkling)", fixed = TRUE)

        ## Done saves them; until the next refresh, they differ from the last one's
        session$setInputs(regular.level = "+10", twinkling.level = "+5")
        session$setInputs(done = 1)
        expect_identical(c(values$regular.level, values$twinkling.level), c("+10", "+5"))
        expect_false(session$returned$unchanged())
        last.args(list(regular.level = "+10", twinkling.level = "+5", movement = "Light"))
        expect_true(session$returned$unchanged())

        ## An empty choice keeps what was saved
        session$setInputs(open = 2)
        session$setInputs(regular.level = "", twinkling.level = NULL)
        session$setInputs(done = 2)
        expect_identical(c(values$regular.level, values$twinkling.level), c("+10", "+5"))
    })
})
