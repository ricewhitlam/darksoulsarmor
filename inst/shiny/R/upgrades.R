
## Upgrade Inputs: the sidebar's button, and the dialog behind it, as a Shiny module. The module keeps
## the saved upgrade level for armor upgraded with regular titanite and for armor upgraded with
## twinkling titanite, shows them in a dialog, and saves the dialog's choices on Done.

## The levels each kind of armor can be upgraded to
REGULAR.LEVELS <- c("+0", "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8", "+9", "+10")
TWINKLING.LEVELS <- c("+0", "+1", "+2", "+3", "+4", "+5")

## The sidebar's button
upgrade.inputs.button <- function(id){
    shiny::actionButton(inputId = shiny::NS(id, "open"), label = "Upgrade Inputs")
}

## The dialog and the saved levels. last.args: the last refresh's arguments (NULL before the first).
## Returns the saved levels (values: regular.level, twinkling.level) and whether they match the last
## refresh (unchanged).
upgrade.inputs.server <- function(id, last.args){
    shiny::moduleServer(id, function(input, output, session){

        values <- shiny::reactiveValues(regular.level = "+0", twinkling.level = "+0")

        shiny::observeEvent(input$open, {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Upgrade Inputs",
                    footer = shiny::actionButton(inputId = session$ns("done"), label = "Done"),
                    shiny::fluidRow(
                        shinyWidgets::pickerInput(
                            inputId = session$ns("regular.level"),
                            label = "Armor Level (Regular)",
                            choices = REGULAR.LEVELS,
                            selected = values$regular.level,
                            multiple = FALSE,
                            options = list(size = 11),
                            choicesOpt = NULL,
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = session$ns("twinkling.level"),
                            label = "Armor Level (Twinkling)",
                            choices = TWINKLING.LEVELS,
                            selected = values$twinkling.level,
                            multiple = FALSE,
                            options = list(size = 10),
                            choicesOpt = NULL,
                            width = NULL,
                            inline = FALSE,
                            stateInput = TRUE,
                            autocomplete = FALSE
                        )
                    )
                )
            )
        })

        shiny::observeEvent(input$done, {
            if(shiny::isTruthy(input$regular.level)){ values$regular.level <- input$regular.level }
            if(shiny::isTruthy(input$twinkling.level)){ values$twinkling.level <- input$twinkling.level }
            shiny::removeModal()
        }, ignoreInit = TRUE)

        ## Whether the saved levels are the last refresh's (so before the first, trivially)
        unchanged <- shiny::reactive({
            args <- last.args()
            is.null(args) || (identical(args$regular.level, values$regular.level) && identical(args$twinkling.level, values$twinkling.level))
        })

        list(values = values, unchanged = unchanged)

    })
}
