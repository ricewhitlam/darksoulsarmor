## Ring Inputs on its own (inst/shiny/R/rings.R): its dialog shows the saved choices, Done saves
## each switch (anything but TRUE or FALSE from a client is ignored), and unchanged says whether the
## saved choices are the last refresh's
source(system.file("shiny", "R", "rings.R", package = "darksoulsarmor"), local = TRUE)

test_that("Ring Inputs saves its switches on Done and compares them with the last refresh", {
    modals <- character(0)
    local_mocked_bindings(showModal = function(ui, ...){ modals <<- c(modals, as.character(ui)) }, .package = "shiny")
    last.args <- shiny::reactiveVal(NULL)
    shiny::testServer(ring.inputs.server, args = list(last.args = last.args), {
        ## No rings to start with, and nothing to compare before the first refresh
        expect_identical(shiny::reactiveValuesToList(values)[names(RINGS)], list(havel.ring = FALSE, favor.ring = FALSE, wolf.ring = FALSE))
        expect_true(session$returned$unchanged())
        last.args(list(havel.ring = FALSE, favor.ring = FALSE, wolf.ring = FALSE, movement = "Light"))
        expect_true(session$returned$unchanged())

        ## The dialog: one switch per ring, under the module's own ids
        session$setInputs(open = 1)
        expect_length(modals, 1)
        for(ring in names(RINGS)){
            expect_match(modals[1], sprintf('id="%s"', session$ns(ring)), fixed = TRUE)
            expect_match(modals[1], RINGS[[ring]], fixed = TRUE)
        }

        ## Done saves the switches; until the next refresh, they differ from the last one's
        session$setInputs(havel.ring = TRUE, favor.ring = FALSE, wolf.ring = TRUE)
        session$setInputs(done = 1)
        expect_identical(shiny::reactiveValuesToList(values)[names(RINGS)], list(havel.ring = TRUE, favor.ring = FALSE, wolf.ring = TRUE))
        expect_false(session$returned$unchanged())
        last.args(list(havel.ring = TRUE, favor.ring = FALSE, wolf.ring = TRUE, movement = "Light"))
        expect_true(session$returned$unchanged())

        ## Anything but TRUE or FALSE keeps what was saved
        session$setInputs(open = 2)
        session$setInputs(havel.ring = "yes", favor.ring = NA, wolf.ring = c(FALSE, TRUE))
        session$setInputs(done = 2)
        expect_identical(shiny::reactiveValuesToList(values)[names(RINGS)], list(havel.ring = TRUE, favor.ring = FALSE, wolf.ring = TRUE))
        expect_true(session$returned$unchanged())
    })
})
