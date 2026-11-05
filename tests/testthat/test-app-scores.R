## Score Inputs on its own (inst/shiny/R/scores.R): its dialog has Stat Scale, Weights Mode and a box
## per stat or per group of stats; Done saves them, and unchanged says whether the saved settings are
## the last refresh's (its weights compared as shares, since the refresh normalized them)
source(system.file("shiny", "R", "scores.R", package = "darksoulsarmor"), local = TRUE)

test_that("Score Inputs saves its settings on Done and compares them with the last refresh", {
    modals <- character(0)
    local_mocked_bindings(showModal = function(ui, ...){ modals <<- c(modals, as.character(ui)) }, .package = "shiny")
    last.args <- shiny::reactiveVal(NULL)
    ## The ten stats, in the search's order, and the three groups
    expect_identical(WEIGHT.METRICS, c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES"))
    expect_identical(GROUP.METRICS, c("PHYSICAL_DEFENSES", "ELEMENTAL_DEFENSES", "RESISTANCES"))
    shiny::testServer(score.inputs.server, args = list(last.args = last.args), {
        defaults <- c(16, 16, 16, 16, 8, 8, 8, 4, 4, 4)

        ## Std Dev, Individual and 16/8/4 to start with, and nothing to compare before the first refresh
        expect_identical(list(values$stat.scale, values$weights.mode, values$weights), list("sd", "individual", defaults))
        expect_true(session$returned$unchanged())
        last.args(list(stat.scale = "sd", weights.mode = "individual", weights = defaults/sum(defaults), movement = "Light"))
        expect_true(session$returned$unchanged())

        ## The dialog, under the module's own ids, its two sets of boxes shown by the module's own
        ## Weights Mode
        session$setInputs(open = 1)
        expect_length(modals, 1)
        for(id in c("stat_scale", "weights_mode", "normalize_weights", "done", WEIGHT.IDS, GROUP.IDS)){
            expect_match(modals[1], sprintf('id="%s"', session$ns(id)), fixed = TRUE, info = id)
        }
        expect_length(gregexpr(sprintf('data-ns-prefix="%s"', session$ns("")), modals[1], fixed = TRUE)[[1]], 2)

        ## Done saves them; until the next refresh, they differ from the last one's
        session$setInputs(stat_scale = "max", weights_mode = "individual")
        do.call(session$setInputs, setNames(as.list(2*defaults), WEIGHT.IDS))
        session$setInputs(done = 1)
        expect_identical(list(values$stat.scale, values$weights.mode, values$weights), list("max", "individual", 2*defaults))
        expect_false(session$returned$unchanged())
        ## The same shares as the last refresh's are unchanged
        last.args(list(stat.scale = "max", weights.mode = "individual", weights = defaults/sum(defaults)))
        expect_true(session$returned$unchanged())

        ## All-zero weights read as changed rather than NA
        do.call(session$setInputs, setNames(as.list(rep(0, 10)), WEIGHT.IDS))
        session$setInputs(done = 2)
        expect_false(session$returned$unchanged())

        ## An empty box keeps every saved weight
        do.call(session$setInputs, replace(setNames(as.list(rep(5, 10)), WEIGHT.IDS), 3, list(NULL)))
        session$setInputs(done = 3)
        expect_identical(values$weights, rep(0, 10))
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
    local_mocked_bindings(showModal = function(ui, ...){ NULL }, .package = "shiny")
    shiny::testServer(score.inputs.server, args = list(last.args = shiny::reactiveVal(NULL)), {
        raw <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        session$setInputs(open = 1)
        do.call(session$setInputs, setNames(as.list(raw), WEIGHT.IDS))
        session$setInputs(normalize_weights = 1)
        normalized <- unlist(updates[WEIGHT.IDS])
        expect_length(normalized, 10)
        expect_equal(sum(normalized), 100)
        expect_equal(normalized, round(normalized, 1))
        expect_true(all(abs(normalized - 100*raw/sum(raw)) <= 0.1 + 1e-9))
    })
})

## Score Inputs' Weights Mode: switching fills the newly shown boxes from the ones just hidden - a
## group gets the sum of its stats' weights, and a group's weight is split evenly across its stats
## in whole tenths, the leftover tenths going to its stats in order - so each group's total is kept
test_that("switching Weights Mode carries the weights over", {
    updates <- list()
    clear.updates <- function(){ updates <<- list() }
    local_mocked_bindings(
        updateAutonumericInput = function(session = NULL, inputId, label = NULL, value = NULL, options = NULL){
            updates[[inputId]] <<- value
        },
        .package = "shinyWidgets"
    )
    local_mocked_bindings(showModal = function(ui, ...){ NULL }, .package = "shiny")
    shiny::testServer(score.inputs.server, args = list(last.args = shiny::reactiveVal(NULL)), {
        ## The browser opening the dialog: its boxes and radio buttons as the module filled them
        session$setInputs(open = 1)
        do.call(session$setInputs, setNames(as.list(values$weights), WEIGHT.IDS))
        session$setInputs(weights_mode = "individual", stat_scale = "sd")
        expect_length(updates, 0)

        ## Individual to grouped: the defaults' sums
        session$setInputs(weights_mode = "grouped")
        expect_equal(unname(unlist(updates[GROUP.IDS])), c(64, 24, 12))

        ## Grouped to individual, split in whole tenths: 33.4 over four stats is 8.4, 8.4, 8.3, 8.3
        clear.updates()
        do.call(session$setInputs, setNames(list(33.4, 33.3, 33.3), GROUP.IDS))
        session$setInputs(weights_mode = "individual")
        split <- unname(unlist(updates[WEIGHT.IDS]))
        expect_equal(split, c(8.4, 8.4, 8.3, 8.3, 11.1, 11.1, 11.1, 11.1, 11.1, 11.1))
        expect_equal(sum(split), 100)

        ## ...and back again exactly
        clear.updates()
        do.call(session$setInputs, setNames(as.list(split), WEIGHT.IDS))
        session$setInputs(weights_mode = "grouped")
        expect_equal(unname(unlist(updates[GROUP.IDS])), c(33.4, 33.3, 33.3))

        ## Uneven weights within a group keep the group's total, evened out
        clear.updates()
        session$setInputs(weights_mode = "individual")
        do.call(session$setInputs, setNames(as.list(c(20, 12, 16, 16, 8, 8, 8, 4, 4, 4)), WEIGHT.IDS))
        session$setInputs(weights_mode = "grouped")
        expect_equal(unname(unlist(updates[GROUP.IDS])), c(64, 24, 12))
        do.call(session$setInputs, setNames(list(64, 24, 12), GROUP.IDS))
        session$setInputs(weights_mode = "individual")
        expect_equal(unname(unlist(updates[WEIGHT.IDS])), c(16, 16, 16, 16, 8, 8, 8, 4, 4, 4))

        ## Normalize works on the boxes showing
        clear.updates()
        session$setInputs(weights_mode = "grouped")
        do.call(session$setInputs, setNames(list(1, 1, 1), GROUP.IDS))
        session$setInputs(normalize_weights = 1)
        expect_equal(unname(unlist(updates[GROUP.IDS])), c(33.4, 33.3, 33.3))
        expect_null(updates[[WEIGHT.IDS[1]]])
    })
})
