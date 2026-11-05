
## Load Inputs: the sidebar's button, and the dialog behind it, as a Shiny module. The module keeps
## the saved movement type, the weapon in each of the four weapon slots and the Endurance level,
## shows them in a dialog (with the weapons' total weight as they're chosen), and saves the dialog's
## choices on Done.

## The weapon slots, by the search's names, with their labels in the dialog (right hand first, as
## the game lists them)
WEAPON.SLOT.LABELS <- c(
    right.1 = "Right Hand Weapon 1", right.2 = "Right Hand Weapon 2",
    left.1 = "Left Hand Weapon 1", left.2 = "Left Hand Weapon 2"
)

## The sidebar's button
load.inputs.button <- function(id){
    shiny::actionButton(inputId = shiny::NS(id, "open"), label = "Load Inputs")
}

## The dialog and the saved choices. last.args: the last refresh's arguments (NULL before the first).
## Returns the saved choices (values: movement, weapons, endurance.level) and whether they match the
## last refresh (unchanged).
load.inputs.server <- function(id, last.args){
    shiny::moduleServer(id, function(input, output, session){

        values <- shiny::reactiveValues(
            movement = "Light",
            weapons = c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None"),
            endurance.level = 10
        )

        ## One dropdown per weapon slot, each weapon's weight beside its name (in the list and once chosen)
        weapon.input <- function(slot){
            shinyWidgets::pickerInput(
                inputId = session$ns(paste0("weapon.", slot)),
                label = WEAPON.SLOT.LABELS[[slot]],
                choices = c("None", weapon.data$WEAPON),
                selected = values$weapons[[slot]],
                multiple = FALSE,
                options = list(`live-search` = TRUE, size = 10, `show-subtext` = TRUE),
                choicesOpt = list(subtext = c("", sprintf("%.1f", weapon.data$WEIGHT))),
                width = NULL,
                inline = FALSE,
                stateInput = TRUE,
                autocomplete = FALSE
            )
        }

        shiny::observeEvent(input$open, {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Load Inputs",
                    footer = shiny::actionButton(inputId = session$ns("done"), label = "Done"),
                    shiny::fluidRow(
                        shiny::radioButtons(
                            inputId = session$ns("movement"),
                            label = "Movement",
                            choices = c("Light", "Mid", "Fat", "Poop"),
                            selected = values$movement,
                            inline = TRUE,
                            width = NULL,
                            choiceNames = NULL,
                            choiceValues = NULL
                        ),
                        shinyWidgets::autonumericInput(
                            inputId = session$ns("endurance.level"),
                            label = "Endurance Level",
                            value = values$endurance.level,
                            align = "right",
                            decimalCharacter = ".",
                            digitGroupSeparator = ",",
                            decimalPlaces = 0,
                            maximumValue = 99,
                            minimumValue = 0
                        ),
                        weapon.input("right.1"),
                        weapon.input("right.2"),
                        weapon.input("left.1"),
                        weapon.input("left.2"),
                        shiny::helpText(shiny::textOutput(outputId = session$ns("weapon_summary"), inline = TRUE))
                    )
                )
            )
        })

        ## Under the weapon dropdowns, as they're changed: the weapons' total weight
        output$weapon_summary <- shiny::renderText({
            weapons <- values$weapons
            for(slot in names(weapons)){
                chosen <- input[[paste0("weapon.", slot)]]
                if(shiny::isTruthy(chosen)){
                    weapons[[slot]] <- chosen
                }
            }
            sprintf("Weapons: %.1f in total", darksoulsarmor:::weapons.weight(weapons))
        })

        ## Done saves the choices: the weapons only if every slot has one
        shiny::observeEvent(input$done, {
            if(shiny::isTruthy(input$movement)){ values$movement <- input$movement }
            if(shiny::isTruthy(input$weapon.left.1) && shiny::isTruthy(input$weapon.right.1) && shiny::isTruthy(input$weapon.left.2) && shiny::isTruthy(input$weapon.right.2)){
                values$weapons <- c(left.1 = input$weapon.left.1, right.1 = input$weapon.right.1, left.2 = input$weapon.left.2, right.2 = input$weapon.right.2)
            }
            if(shiny::isTruthy(input$endurance.level)){ values$endurance.level <- round(input$endurance.level, 0) }
            shiny::removeModal()
        }, ignoreInit = TRUE)

        ## Whether the saved choices are the last refresh's (so before the first, trivially)
        unchanged <- shiny::reactive({
            args <- last.args()
            is.null(args) || (
                identical(args$movement, values$movement) &&
                identical(args$weapons, values$weapons) &&
                identical(args$endurance.level, values$endurance.level)
            )
        })

        list(values = values, unchanged = unchanged)

    })
}
