
## Ring Inputs: the sidebar's button, and the dialog behind it, as a Shiny module. The module keeps
## the saved choice for each ring the search takes (Havel's Ring and the Ring of Favor raise the
## equip load, the Wolf Ring adds poise), shows them in a dialog, and saves the dialog's switches on
## Done. Shiny loads this folder (R/) before ui.R and server.R.

## The rings, by the search's argument names, with their labels in the dialog
RINGS <- c(
    havel.ring = "Havel's Ring (+50% Eq Load)",
    favor.ring = "Ring of Favor (+20% Eq Load)",
    wolf.ring = "Wolf Ring (+40 Poise)"
)

## The sidebar's button
ring.inputs.button <- function(id){
    shiny::actionButton(inputId = shiny::NS(id, "open"), label = "Ring Inputs")
}

## The dialog and the saved choices. last.args: the last refresh's arguments (NULL before the first).
## Returns the saved choices (values: TRUE or FALSE per ring) and whether they match the last
## refresh (unchanged).
ring.inputs.server <- function(id, last.args){
    shiny::moduleServer(id, function(input, output, session){

        values <- shiny::reactiveValues(havel.ring = FALSE, favor.ring = FALSE, wolf.ring = FALSE)

        shiny::observeEvent(input$open, {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Ring Inputs",
                    footer = shiny::actionButton(inputId = session$ns("done"), label = "Done"),
                    shiny::fluidRow(
                        bslib::input_switch(id = session$ns("havel.ring"), label = RINGS[["havel.ring"]], value = values$havel.ring, width = NULL),
                        bslib::input_switch(id = session$ns("favor.ring"), label = RINGS[["favor.ring"]], value = values$favor.ring, width = NULL),
                        bslib::input_switch(id = session$ns("wolf.ring"), label = RINGS[["wolf.ring"]], value = values$wolf.ring, width = NULL)
                    )
                )
            )
        })

        ## Done saves each switch - a client sending anything but TRUE or FALSE keeps what was saved
        shiny::observeEvent(input$done, {
            for(ring in names(RINGS)){
                chosen <- input[[ring]]
                if(isTRUE(chosen) || isFALSE(chosen)){
                    values[[ring]] <- chosen
                }
            }
            shiny::removeModal()
        }, ignoreInit = TRUE)

        ## Whether the saved choices are the last refresh's (so before the first, trivially)
        unchanged <- shiny::reactive({
            args <- last.args()
            is.null(args) || all(vapply(names(RINGS), function(ring){ identical(args[[ring]], values[[ring]]) }, logical(1)))
        })

        list(values = values, unchanged = unchanged)

    })
}
