## The Trade-offs tab on its own (inst/shiny/R/tradeoffs.R): it computes its curve only while it's
## open and there's been a refresh, for that refresh's settings; a row of its table shows that set's
## links; errors go to the app's error message; and a download gets the curve it holds, or one
## computed for it
source(system.file("shiny", "R", "tradeoffs.R", package = "darksoulsarmor"), local = TRUE)

test_that("the Trade-offs tab computes for the last refresh while it's open", {
    last.args <- shiny::reactiveVal(NULL)
    shown <- shiny::reactiveVal(FALSE)
    links <- list()
    errors <- character(0)
    show.links <- function(set){ links[[length(links)+1]] <<- set }
    show.error <- function(message){ errors <<- c(errors, message) }
    local_mocked_bindings(show_modal_spinner = function(...){ NULL }, remove_modal_spinner = function(...){ NULL }, .package = "shinybusy")
    ## The curve's search, made to fail on demand
    search <- darksoulsarmor::get.armor.tradeoffs
    failing <- FALSE
    fail.search <- function(){ failing <<- TRUE }
    local_mocked_bindings(get.armor.tradeoffs = function(...){ if(failing) stop("simulated failure"); search(...) }, .package = "darksoulsarmor")
    ## A refresh's settings, as the search returns them
    refresh.args <- function(...){ get.optimal.armor.combos(max.table.size = 1, weights = default.weights, ...)$args }
    shiny::testServer(tradeoffs.tab.server, args = list(last.args = last.args, shown = shown, show.links = show.links, show.error = show.error), {
        session$setInputs(metric = "POISE", jumps = "some", flats = "some")

        ## Open before any refresh, or refreshed but not open: nothing to compute
        shown(TRUE)
        session$flushReact()
        expect_null(session$returned$data())
        shown(FALSE)
        last.args(refresh.args())
        session$flushReact()
        expect_equal(session$returned$computations(), 0)

        ## Opening it computes the curve for the refresh, and clears the error message
        shown(TRUE)
        session$flushReact()
        expect_equal(session$returned$computations(), 1)
        expect_equal(session$returned$data()$metric, "POISE")
        expect_identical(session$returned$data()$key, tradeoff.key(last.args(), "POISE"))
        expect_identical(errors, "")
        expect_no_error(output$plot)

        ## A download of that refresh reuses it; of another refresh, computes one
        expect_identical(session$returned$download.table(last.args()), tradeoff.table(session$returned$data()))
        expect_equal(session$returned$computations(), 1)
        wolf <- refresh.args(wolf.ring = TRUE)
        session$returned$download.table(wolf)
        expect_equal(session$returned$computations(), 2)
        expect_true(session$returned$data()$key$settings$wolf.ring)

        ## A table row with a set shows its links
        row <- which(!is.na(session$returned$data()$data$HEAD))[1]
        session$setInputs(table_rows_selected = row)
        expect_length(links, 1)
        expect_identical(links[[1]]$HEAD, session$returned$data()$data$HEAD[row])

        ## An error computing the curve goes to the app's error message
        fail.search()
        session$setInputs(metric = "MAG_DEF")
        expect_identical(tail(errors, 1), "simulated failure")
        expect_equal(session$returned$data()$metric, "POISE")
    })
})
