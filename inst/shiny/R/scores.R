
## Score Inputs: the sidebar's button, and the dialog behind it, as a Shiny module. The module keeps
## the saved score settings - the scale each stat is weighted in (stat.scale), whether the weights
## are per stat or per group of stats (weights.mode), and the weights themselves, ten or three -
## shows them in a dialog (with a button rescaling the weights to 100%), and saves the dialog's
## choices on Done.

## The ten stats with weights, in the order the search's weights argument takes them, and the three
## groups of stats (darksoulsarmor's METRICS and stat.groups)
WEIGHT.METRICS <- darksoulsarmor:::METRICS[!is.na(weight.index)][order(weight.index), metric]
GROUP.METRICS <- names(darksoulsarmor:::stat.groups)

## A stat's or group's box in the dialog, before the module's prefix (e.g. "PHYS_DEF" ->
## "physdefweight")
weight.input.id <- function(stat){ paste0(tolower(gsub("_", "", stat)), "weight") }
## unname(): vapply() names its result after its input, and Shiny sends a named vector to the
## browser differently (jsonlite's keep_vec_names), which mangles the ids
WEIGHT.IDS <- unname(vapply(WEIGHT.METRICS, weight.input.id, character(1)))
GROUP.IDS <- unname(vapply(GROUP.METRICS, weight.input.id, character(1)))

## Carrying weights across a mode switch, in tenths (the boxes' precision): a group gets the sum
## of its stats' weights; a group's weight is split evenly across its stats, in whole tenths,
## with any tenths left over going to its stats in order (33.4 -> 8.4, 8.4, 8.3, 8.3) - so each
## group's total, and with it the overall sum, is kept exactly either way
grouped.from.individual <- function(weights){
    unname(vapply(darksoulsarmor:::stat.groups, function(stats){ round(sum(weights[match(stats, WEIGHT.METRICS)]), 1) }, numeric(1)))
}
individual.from.grouped <- function(weights){
    out <- rep(0, length(WEIGHT.METRICS))
    for(g in seq_along(darksoulsarmor:::stat.groups)){
        members <- match(darksoulsarmor:::stat.groups[[g]], WEIGHT.METRICS)
        tenths <- round(10*weights[g])
        share <- rep(tenths %/% length(members), length(members))
        extra <- tenths %% length(members)
        share[seq_len(extra)] <- share[seq_len(extra)]+1
        out[members] <- share/10
    }
    out
}

## The sidebar's button
score.inputs.button <- function(id){
    shiny::actionButton(inputId = shiny::NS(id, "open"), label = "Score Inputs")
}

## The dialog and the saved settings. last.args: the last refresh's arguments (NULL before the
## first). Returns the saved settings (values: weights, stat.scale, weights.mode) and whether they
## match the last refresh (unchanged).
score.inputs.server <- function(id, last.args){
    shiny::moduleServer(id, function(input, output, session){

        values <- shiny::reactiveValues(weights = c(16.0, 16.0, 16.0, 16.0, 8.0, 8.0, 8.0, 4.0, 4.0, 4.0), stat.scale = "sd", weights.mode = "individual")

        ## Both sets of boxes as the dialog opens: the saved weights in their own mode, converted into
        ## the other
        individual.weights <- function(){ if(values$weights.mode == "individual") values$weights else individual.from.grouped(values$weights) }
        grouped.weights <- function(){ if(values$weights.mode == "grouped") values$weights else grouped.from.individual(values$weights) }
        ## The mode the dialog's boxes were last filled for, so that a switch converts from the other set
        shown.mode <- shiny::reactiveVal("individual")

        weight.input <- function(stat){
            shinyWidgets::autonumericInput(
                inputId = session$ns(weight.input.id(stat)),
                label = stat,
                value = if(stat %in% GROUP.METRICS) grouped.weights()[match(stat, GROUP.METRICS)] else individual.weights()[match(stat, WEIGHT.METRICS)],
                align = "right",
                currencySymbol = "%",
                currencySymbolPlacement = "s",
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 1,
                maximumValue = 100,
                minimumValue = 0
            )
        }

        shiny::observeEvent(input$open, {
            shown.mode(values$weights.mode)
            shiny::showModal(
                shiny::modalDialog(
                    title = "Score Inputs",
                    footer = shiny::fluidRow(shiny::column(8, shiny::actionButton(inputId = session$ns("normalize_weights"), label = "Normalize to 100%")), shiny::column(4, shiny::actionButton(inputId = session$ns("done"), label = "Done"))),
                    ## Room under each choice, so the two and the boxes below read as separate
                    shiny::div(style = "margin-bottom: 1.25rem;", shiny::radioButtons(inputId = session$ns("stat_scale"), label = "Stat Scale", choices = c("Std Dev" = "sd", "Max" = "max", "Raw" = "raw"), selected = values$stat.scale, inline = TRUE)),
                    shiny::div(style = "margin-bottom: 1.25rem;", shiny::radioButtons(inputId = session$ns("weights_mode"), label = "Weights Mode", choices = c("Individual" = "individual", "Grouped" = "grouped"), selected = values$weights.mode, inline = TRUE)),
                    ## Both sets of boxes, the mode showing one (ns: the condition's input under the
                    ## module's prefix)
                    shiny::conditionalPanel(
                        condition = "input.weights_mode == 'individual'",
                        ns = session$ns,
                        shiny::fluidRow(
                            shiny::column(width = 3, weight.input("PHYS_DEF"), weight.input("MAG_DEF"), weight.input("BLEED_RES")),
                            shiny::column(width = 3, weight.input("STRIKE_DEF"), weight.input("FIRE_DEF"), weight.input("POIS_RES")),
                            shiny::column(width = 3, weight.input("SLASH_DEF"), weight.input("LITNG_DEF"), weight.input("CURSE_RES")),
                            shiny::column(width = 3, weight.input("THRUST_DEF"))
                        )
                    ),
                    shiny::conditionalPanel(
                        condition = "input.weights_mode == 'grouped'",
                        ns = session$ns,
                        ## One column: the group names are too long to sit side by side
                        shiny::fluidRow(
                            shiny::column(width = 6, weight.input("PHYSICAL_DEFENSES"), weight.input("ELEMENTAL_DEFENSES"), weight.input("RESISTANCES"))
                        )
                    )
                )
            )
        })

        ## A mode switch in the dialog fills the newly shown boxes from the ones just hidden (the
        ## update function adds the module's prefix to the ids itself)
        shiny::observeEvent(input$weights_mode, {
            mode <- input$weights_mode
            if(!(mode %in% c("individual", "grouped")) || identical(mode, shown.mode())){
                return(invisible(NULL))
            }
            from.ids <- if(mode == "grouped") WEIGHT.IDS else GROUP.IDS
            from <- lapply(from.ids, function(id){ input[[id]] })
            if(all(vapply(from, shiny::isTruthy, logical(1)))){
                to <- if(mode == "grouped") grouped.from.individual(unlist(from)) else individual.from.grouped(unlist(from))
                to.ids <- if(mode == "grouped") GROUP.IDS else WEIGHT.IDS
                for(i in seq_along(to.ids)){
                    shinyWidgets::updateAutonumericInput(inputId = to.ids[i], value = to[i])
                }
            }
            shown.mode(mode)
        }, ignoreInit = TRUE)

        ## The boxes the dialog is showing: the ten stats' or the three groups'
        shown.weight.ids <- function(){ if(identical(shown.mode(), "grouped")) GROUP.IDS else WEIGHT.IDS }

        shiny::observeEvent(input$normalize_weights, {

            weight.inputs <- lapply(shown.weight.ids(), function(id){ input[[id]] })
            ## sum(weights) > 1e-15 mirrors get.optimal.armor.combos' own weights validation - without
            ## it, all-zero (or otherwise ~0-sum) inputs divide by ~0 below, producing NaN that later
            ## crashes on if(diff > 0) with "missing value where TRUE/FALSE needed".
            if(all(vapply(weight.inputs, shiny::isTruthy, logical(1))) && sum(unlist(weight.inputs)) > 1e-15){

                weights <- unlist(weight.inputs)

                weights <- 1000*weights/sum(weights)
                int.parts <- floor(weights)
                frac.parts <- (weights-int.parts)
                diff <- 1000-sum(int.parts)
                weights <- int.parts
                if(diff > 0){
                    indices <- order(frac.parts, decreasing = TRUE)[seq_len(diff)]
                    weights[indices] <- weights[indices]+1
                }
                weights <- 0.1*weights

                ## Update inputs
                ids <- shown.weight.ids()
                for(i in seq_along(ids)){
                    shinyWidgets::updateAutonumericInput(inputId = ids[i], value = weights[i])
                }

            }

        }, ignoreInit = TRUE)

        ## Done saves the scale, the mode and the boxes showing (a client sending anything else for the
        ## scale or mode keeps what was saved)
        shiny::observeEvent(input$done, {
            weight.inputs <- lapply(shown.weight.ids(), function(id){ input[[id]] })
            if(all(vapply(weight.inputs, shiny::isTruthy, logical(1)))){
                values$weights <- unlist(weight.inputs)
                values$weights.mode <- shown.mode()
            }
            if(isTRUE(input$stat_scale %in% c("sd", "max", "raw")) && length(input$stat_scale) == 1){
                values$stat.scale <- input$stat_scale
            }
            shiny::removeModal()
        }, ignoreInit = TRUE)

        ## Whether the saved settings are the last refresh's (so before the first, trivially). The
        ## refresh normalized its weights, so these are compared as shares. isTRUE(all(...)) keeps
        ## this a single TRUE/FALSE - all-zero weights normalize to 0/0 = NaN, and an NA here would
        ## reach the if() in the app's refresh-message observer, erroring inside an observer and
        ## ending the session. Weights in the other mode differ by count alone.
        unchanged <- shiny::reactive({
            args <- last.args()
            is.null(args) || (
                identical(values$stat.scale, args$stat.scale) &&
                identical(values$weights.mode, args$weights.mode) &&
                length(values$weights) == length(args$weights) &&
                isTRUE(all(abs(values$weights/sum(values$weights)-args$weights) < 1e-10))
            )
        })

        list(values = values, unchanged = unchanged)

    })
}
