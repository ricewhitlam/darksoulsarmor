## Filter Inputs on its own (inst/shiny/R/filters.R): its dialog has Max Table Size, the starting
## class, areas completed, upgrade types and a list of pieces per armor slot, plus buttons narrowing
## those lists to poise, efficient or quiet armor; Done saves the choices (Max Table Size clamped,
## an empty slot as its "No <slot>" piece), and unchanged says whether the saved choices are the last
## refresh's
source(system.file("shiny", "R", "filters.R", package = "darksoulsarmor"), local = TRUE)

test_that("Filter Inputs saves its choices on Done and compares them with the last refresh", {
    modals <- character(0)
    updates <- list()
    local_mocked_bindings(showModal = function(ui, ...){ modals <<- c(modals, as.character(ui)) }, .package = "shiny")
    local_mocked_bindings(updatePickerInput = function(session = NULL, inputId, selected = NULL, ...){ updates[[inputId]] <<- selected }, .package = "shinyWidgets")
    clear.updates <- function(){ updates <<- list() }
    last.args <- shiny::reactiveVal(NULL)
    shiny::testServer(filter.inputs.server, args = list(last.args = last.args), {
        saved <- function(){ shiny::reactiveValuesToList(values)[c("max.table.size", "starting.class", "areas.completed", "upgrade.types", "head.filter", "chest.filter", "hands.filter", "legs.filter")] }
        defaults <- list(
            max.table.size = 1000, starting.class = classes[1], areas.completed = areas, upgrade.types = c("Regular", "Twinkling", "None"),
            head.filter = head.data.unupgraded$ARMOR, chest.filter = chest.data.unupgraded$ARMOR,
            hands.filter = hands.data.unupgraded$ARMOR, legs.filter = legs.data.unupgraded$ARMOR
        )
        ## The dialog's widgets as the browser sends them, under the module's own ids
        send <- function(choices){ do.call(session$setInputs, choices) }

        ## Everything allowed to start with, and nothing to compare before the first refresh
        expect_identical(saved(), defaults)
        expect_true(session$returned$unchanged())
        last.args(c(defaults, list(movement = "Light")))
        expect_true(session$returned$unchanged())

        ## The dialog, under the module's own ids
        session$setInputs(open = 1)
        expect_length(modals, 1)
        for(id in c(names(defaults), "poise_only", "stam_only", "quiet_only")){
            expect_match(modals[1], sprintf('id="%s"', session$ns(id)), fixed = TRUE, info = id)
        }

        ## The quick filters keep only the chosen pieces with the property
        send(defaults)
        session$setInputs(poise_only = 1)
        expect_setequal(updates$head.filter, head.data.unupgraded[POISE > 0]$ARMOR)
        expect_setequal(updates$legs.filter, legs.data.unupgraded[POISE > 0]$ARMOR)
        session$setInputs(stam_only = 1)
        expect_setequal(updates$chest.filter, chest.data.unupgraded[STAM_MOD >= 0]$ARMOR)
        session$setInputs(quiet_only = 1)
        expect_setequal(updates$hands.filter, hands.data.unupgraded[SOUND_MOD <= 1]$ARMOR)

        ## Done saves them, Max Table Size clamped; until the next refresh, they differ from the last one's
        choices <- utils::modifyList(defaults, list(max.table.size = 5e6, starting.class = "Knight", head.filter = c("No Head", "Mask of the Father")))
        send(choices)
        session$setInputs(done = 1)
        expect_identical(saved(), utils::modifyList(choices, list(max.table.size = 100000)))
        expect_false(session$returned$unchanged())
        last.args(utils::modifyList(choices, list(max.table.size = 100000)))
        expect_true(session$returned$unchanged())
        send(list(max.table.size = -5))
        session$setInputs(done = 2)
        expect_identical(values$max.table.size, 1)

        ## Empty slots become their "No <slot>" piece alone (in the dialog too), and empty area and
        ## upgrade type lists are kept as empty
        clear.updates()
        send(list(head.filter = character(0), chest.filter = character(0), hands.filter = character(0), legs.filter = character(0), areas.completed = character(0), upgrade.types = character(0)))
        session$setInputs(done = 3)
        expect_identical(list(values$head.filter, values$chest.filter, values$hands.filter, values$legs.filter), list("No Head", "No Chest", "No Hands", "No Legs"))
        expect_identical(updates, list(head.filter = "No Head", chest.filter = "No Chest", hands.filter = "No Hands", legs.filter = "No Legs"))
        expect_identical(list(values$areas.completed, values$upgrade.types), list(character(0), character(0)))
    })
})
