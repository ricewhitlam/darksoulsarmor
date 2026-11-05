
## Minimum Inputs: the sidebar's button, and the dialog behind it, as a Shiny module. The module keeps
## the saved minimum for each of the search's twelve metrics, shows them in a dialog, and saves the
## dialog's boxes on Done.

## The metrics, in the order the search's minima argument takes them (darksoulsarmor's METRICS)
MINIMUM.METRICS <- darksoulsarmor:::METRICS$metric[order(darksoulsarmor:::METRICS$minima.index)]

## A metric's box in the dialog, before the module's prefix (e.g. "PHYS_DEF" -> "minphysdef")
minimum.input.id <- function(stat){ paste0("min", tolower(gsub("_", "", stat))) }

## The sidebar's button
minimum.inputs.button <- function(id){
    shiny::actionButton(inputId = shiny::NS(id, "open"), label = "Minimum Inputs")
}

## The dialog and the saved minima. last.args: the last refresh's arguments (NULL before the first).
## Returns the saved minima (values$minima, in MINIMUM.METRICS' order) and whether they match the
## last refresh (unchanged).
minimum.inputs.server <- function(id, last.args){
    shiny::moduleServer(id, function(input, output, session){

        values <- shiny::reactiveValues(minima = rep(0.0, length(MINIMUM.METRICS)))

        minimum.input <- function(stat){
            shinyWidgets::autonumericInput(
                inputId = session$ns(minimum.input.id(stat)),
                label = stat,
                value = values$minima[match(stat, MINIMUM.METRICS)],
                align = "right",
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = ifelse(stat %in% c("POISE", "DURABILITY"), 0, 1),
                maximumValue = 999,
                minimumValue = 0
            )
        }

        shiny::observeEvent(input$open, {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Minimum Inputs",
                    footer = shiny::actionButton(inputId = session$ns("done"), label = "Done"),
                    shiny::fluidRow(
                        shiny::column(width = 3, minimum.input("PHYS_DEF"), minimum.input("MAG_DEF"), minimum.input("BLEED_RES")),
                        shiny::column(width = 3, minimum.input("STRIKE_DEF"), minimum.input("FIRE_DEF"), minimum.input("POIS_RES")),
                        shiny::column(width = 3, minimum.input("SLASH_DEF"), minimum.input("LITNG_DEF"), minimum.input("CURSE_RES")),
                        shiny::column(width = 3, minimum.input("THRUST_DEF"), minimum.input("POISE"), minimum.input("DURABILITY"))
                    )
                )
            )
        })

        ## Done saves the boxes, if every one holds a value
        shiny::observeEvent(input$done, {
            boxes <- lapply(MINIMUM.METRICS, function(stat){ input[[minimum.input.id(stat)]] })
            if(all(vapply(boxes, shiny::isTruthy, logical(1)))){
                values$minima <- round(unlist(boxes), 1)
            }
            shiny::removeModal()
        }, ignoreInit = TRUE)

        ## Whether the saved minima are the last refresh's (so before the first, trivially)
        unchanged <- shiny::reactive({
            args <- last.args()
            is.null(args) || identical(args$minima, values$minima)
        })

        list(values = values, unchanged = unchanged)

    })
}
