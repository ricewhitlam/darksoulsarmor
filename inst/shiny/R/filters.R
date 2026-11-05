
## Filter Inputs: the sidebar's button, and the dialog behind it, as a Shiny module. The module keeps
## the saved Max Table Size, starting class, areas completed, upgrade types and the pieces allowed in
## each armor slot, shows them in a dialog (with buttons that narrow the pieces to poise, efficient
## or quiet armor), and saves the dialog's choices on Done.

## Largest Max Table Size the app allows. The widget enforces it in the browser, but a client can
## send any value (e.g. via Shiny.setInputValue), so Done also clamps it server-side - an unbounded
## value can make one refresh hold gigabytes of results.
MAX_TABLE_SIZE <- 100000

## The sidebar's button
filter.inputs.button <- function(id){
    shiny::actionButton(inputId = shiny::NS(id, "open"), label = "Filter Inputs")
}

## The dialog and the saved choices. last.args: the last refresh's arguments (NULL before the first).
## Returns the saved choices (values: max.table.size, starting.class, areas.completed, upgrade.types,
## head.filter, chest.filter, hands.filter, legs.filter) and whether they match the last refresh
## (unchanged).
filter.inputs.server <- function(id, last.args){
    shiny::moduleServer(id, function(input, output, session){

        values <- shiny::reactiveValues(
            max.table.size = 1000,
            starting.class = classes[1],
            areas.completed = areas,
            upgrade.types = c("Regular", "Twinkling", "None"),
            head.filter = head.data.unupgraded$ARMOR,
            chest.filter = chest.data.unupgraded$ARMOR,
            hands.filter = hands.data.unupgraded$ARMOR,
            legs.filter = legs.data.unupgraded$ARMOR
        )

        shiny::observeEvent(input$open, {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Filter Inputs",
                    footer = shiny::actionButton(inputId = session$ns("done"), label = "Done"),
                    shiny::fluidRow(
                        shinyWidgets::autonumericInput(
                            inputId = session$ns("max.table.size"),
                            label = "Max Table Size",
                            value = values$max.table.size,
                            align = "right",
                            decimalCharacter = ".",
                            digitGroupSeparator = ",",
                            decimalPlaces = 0,
                            maximumValue = MAX_TABLE_SIZE,
                            minimumValue = 1
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("starting.class"),
                            label = "Starting Class",
                            choices = classes,
                            selected = values$starting.class,
                            multiple = FALSE,
                            options = list(size = 10),
                            choicesOpt = NULL,
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("areas.completed"),
                            label = "Areas Completed",
                            choices = areas,
                            selected = values$areas.completed,
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 19),
                            choicesOpt = NULL,
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("upgrade.types"),
                            label = "Upgrades With",
                            choices = c("Regular", "Twinkling", "None"),
                            selected = values$upgrade.types,
                            multiple = TRUE,
                            options = list(size = 10),
                            choicesOpt = NULL,
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("head.filter"),
                            label = "Head",
                            choices = head.data.unupgraded$ARMOR,
                            selected = values$head.filter,
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10, `show-subtext` = TRUE),
                            choicesOpt = list(subtext = ifelse(head.data.unupgraded$ARMOR == "No Head", "", sprintf("%.1f", head.data.unupgraded$WEIGHT))),
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("chest.filter"),
                            label = "Chest",
                            choices = chest.data.unupgraded$ARMOR,
                            selected = values$chest.filter,
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10, `show-subtext` = TRUE),
                            choicesOpt = list(subtext = ifelse(chest.data.unupgraded$ARMOR == "No Chest", "", sprintf("%.1f", chest.data.unupgraded$WEIGHT))),
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("hands.filter"),
                            label = "Hands",
                            choices = hands.data.unupgraded$ARMOR,
                            selected = values$hands.filter,
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10, `show-subtext` = TRUE),
                            choicesOpt = list(subtext = ifelse(hands.data.unupgraded$ARMOR == "No Hands", "", sprintf("%.1f", hands.data.unupgraded$WEIGHT))),
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("legs.filter"),
                            label = "Legs",
                            choices = legs.data.unupgraded$ARMOR,
                            selected = values$legs.filter,
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10, `show-subtext` = TRUE),
                            choicesOpt = list(subtext = ifelse(legs.data.unupgraded$ARMOR == "No Legs", "", sprintf("%.1f", legs.data.unupgraded$WEIGHT))),
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shiny::column(6, shiny::p(),
                            shiny::actionButton(inputId = session$ns("poise_only"), label = "Poise Armor Only"), shiny::p(),
                            shiny::actionButton(inputId = session$ns("stam_only"), label = "Efficient Armor Only"), shiny::p(),
                            shiny::actionButton(inputId = session$ns("quiet_only"), label = "Quiet Armor Only"), shiny::p()
                        )
                    )
                )
            )
        })

        ## The quick filters: each keeps only the chosen pieces with the property (the update functions
        ## add the module's prefix to these ids themselves)
        shiny::observeEvent(input$poise_only, {
            shinyWidgets::updatePickerInput(inputId = "head.filter", selected = input$head.filter[input$head.filter %in% head.data.unupgraded[POISE > 0]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "chest.filter", selected = input$chest.filter[input$chest.filter %in% chest.data.unupgraded[POISE > 0]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "hands.filter", selected = input$hands.filter[input$hands.filter %in% hands.data.unupgraded[POISE > 0]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "legs.filter", selected = input$legs.filter[input$legs.filter %in% legs.data.unupgraded[POISE > 0]$ARMOR])
        })

        shiny::observeEvent(input$stam_only, {
            shinyWidgets::updatePickerInput(inputId = "head.filter", selected = input$head.filter[input$head.filter %in% head.data.unupgraded[STAM_MOD >= 0]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "chest.filter", selected = input$chest.filter[input$chest.filter %in% chest.data.unupgraded[STAM_MOD >= 0]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "hands.filter", selected = input$hands.filter[input$hands.filter %in% hands.data.unupgraded[STAM_MOD >= 0]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "legs.filter", selected = input$legs.filter[input$legs.filter %in% legs.data.unupgraded[STAM_MOD >= 0]$ARMOR])
        })

        shiny::observeEvent(input$quiet_only, {
            shinyWidgets::updatePickerInput(inputId = "head.filter", selected = input$head.filter[input$head.filter %in% head.data.unupgraded[SOUND_MOD <= 1]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "chest.filter", selected = input$chest.filter[input$chest.filter %in% chest.data.unupgraded[SOUND_MOD <= 1]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "hands.filter", selected = input$hands.filter[input$hands.filter %in% hands.data.unupgraded[SOUND_MOD <= 1]$ARMOR])
            shinyWidgets::updatePickerInput(inputId = "legs.filter", selected = input$legs.filter[input$legs.filter %in% legs.data.unupgraded[SOUND_MOD <= 1]$ARMOR])
        })

        ## Done saves the choices: Max Table Size clamped to 1-MAX_TABLE_SIZE, an empty area or upgrade
        ## type list as no areas or types, and an empty slot as its "No <slot>" piece alone
        shiny::observeEvent(input$done, {
            if(shiny::isTruthy(input$max.table.size)){ values$max.table.size <- min(MAX_TABLE_SIZE, max(1, round(input$max.table.size, 0))) }
            if(shiny::isTruthy(input$starting.class)){ values$starting.class <- input$starting.class }
            if(shiny::isTruthy(input$areas.completed)){ values$areas.completed <- input$areas.completed } else if(length(input$areas.completed) == 0){ values$areas.completed <- character(0) }
            if(shiny::isTruthy(input$upgrade.types)){ values$upgrade.types <- input$upgrade.types } else if(length(input$upgrade.types) == 0){ values$upgrade.types <- character(0) }
            if(shiny::isTruthy(input$head.filter)){ values$head.filter <- input$head.filter } else if(length(input$head.filter) == 0){
                values$head.filter <- "No Head"
                shinyWidgets::updatePickerInput(inputId = "head.filter", selected = "No Head")
            }
            if(shiny::isTruthy(input$chest.filter)){ values$chest.filter <- input$chest.filter } else if(length(input$chest.filter) == 0){
                values$chest.filter <- "No Chest"
                shinyWidgets::updatePickerInput(inputId = "chest.filter", selected = "No Chest")
            }
            if(shiny::isTruthy(input$hands.filter)){ values$hands.filter <- input$hands.filter } else if(length(input$hands.filter) == 0){
                values$hands.filter <- "No Hands"
                shinyWidgets::updatePickerInput(inputId = "hands.filter", selected = "No Hands")
            }
            if(shiny::isTruthy(input$legs.filter)){ values$legs.filter <- input$legs.filter } else if(length(input$legs.filter) == 0){
                values$legs.filter <- "No Legs"
                shinyWidgets::updatePickerInput(inputId = "legs.filter", selected = "No Legs")
            }
            shiny::removeModal()
        }, ignoreInit = TRUE)

        ## Whether the saved choices are the last refresh's (so before the first, trivially)
        unchanged <- shiny::reactive({
            args <- last.args()
            is.null(args) || all(vapply(names(values), function(name){ identical(args[[name]], values[[name]]) }, logical(1)))
        })

        list(values = values, unchanged = unchanged)

    })
}
