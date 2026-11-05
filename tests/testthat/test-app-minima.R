## Minimum Inputs on its own (inst/shiny/R/minima.R): its dialog has a box per metric, Done saves
## them (to one decimal, and only if every box holds a value), and unchanged says whether the saved
## minima are the last refresh's
source(system.file("shiny", "R", "minima.R", package = "darksoulsarmor"), local = TRUE)

test_that("Minimum Inputs saves its boxes on Done and compares them with the last refresh", {
    modals <- character(0)
    local_mocked_bindings(showModal = function(ui, ...){ modals <<- c(modals, as.character(ui)) }, .package = "shiny")
    last.args <- shiny::reactiveVal(NULL)
    ## The twelve metrics, in the search's order
    expect_identical(MINIMUM.METRICS, c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "POISE", "BLEED_RES", "POIS_RES", "CURSE_RES", "DURABILITY"))
    shiny::testServer(minimum.inputs.server, args = list(last.args = last.args), {
        boxes <- function(minima){ setNames(as.list(minima), vapply(MINIMUM.METRICS, minimum.input.id, character(1))) }

        ## No minima to start with, and nothing to compare before the first refresh
        expect_identical(values$minima, rep(0.0, 12))
        expect_true(session$returned$unchanged())
        last.args(list(minima = rep(0.0, 12), movement = "Light"))
        expect_true(session$returned$unchanged())

        ## The dialog: a box per metric, under the module's own ids
        session$setInputs(open = 1)
        expect_length(modals, 1)
        for(stat in MINIMUM.METRICS){
            expect_match(modals[1], sprintf('id="%s"', session$ns(minimum.input.id(stat))), fixed = TRUE)
        }

        ## Done saves them, to one decimal; until the next refresh, they differ from the last one's
        minima <- c(50.04, rep(0, 6), 31, rep(0, 4))
        do.call(session$setInputs, boxes(minima))
        session$setInputs(done = 1)
        expect_identical(values$minima, round(minima, 1))
        expect_false(session$returned$unchanged())
        last.args(list(minima = round(minima, 1), movement = "Light"))
        expect_true(session$returned$unchanged())

        ## An empty box keeps every saved minimum
        session$setInputs(open = 2)
        do.call(session$setInputs, replace(boxes(rep(5, 12)), 3, list(NULL)))
        session$setInputs(done = 2)
        expect_identical(values$minima, round(minima, 1))
    })
})
