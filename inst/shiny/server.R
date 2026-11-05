
## The User Guide (guide.html, beside this file), read once as the app loads - Shiny loads server.R
## from the app's own folder, here and in the browser version alike
guide.html <- shiny::HTML(paste(readLines("guide.html", encoding = "UTF-8", warn = FALSE), collapse = "\n"))

server <- function(input, output, session){


    output$mode_label <- shiny::renderText("Mode (Light or Dark)")

    shiny::observeEvent(input$guide, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "User Guide", 
                easyClose = TRUE,
                footer = NULL,
                size = "l",
                guide.html
            ) 
        ) 
    }) 


    been.refreshed <- shiny::reactiveVal(FALSE)


    shiny::observe({
        if(been.refreshed()){
            if(filters$unchanged() && upgrades$unchanged() && constraints$unchanged() && minima$unchanged() && rings$unchanged() && scores$unchanged()){
                output$refreshmessage <- shiny::renderText("")
            } else{
                output$refreshmessage <- shiny::renderText("Inputs have changed - click 'Refresh Armor Data' to update both tabs")
            }
        } else{
            output$refreshmessage <- shiny::renderText("Adjust settings in the sidebar and click 'Refresh Armor Data' to pull results for both tabs")
        }
    })


    ## The dialogs (R/), each compared with the last refresh's arguments once there's been one
    last.args <- shiny::reactive({ if(been.refreshed()) armordata()$args else NULL })
    filters <- filter.inputs.server("filters", last.args)
    upgrades <- upgrade.inputs.server("upgrades", last.args)
    minima <- minimum.inputs.server("minima", last.args)
    constraints <- load.inputs.server("constraints", last.args)
    rings <- ring.inputs.server("rings", last.args)
    scores <- score.inputs.server("scores", last.args)


    armordata <- 
        shiny::reactiveVal(
            list(
                args = 
                    list(
                        max.table.size = 1000,
                        starting.class = classes[1],
                        areas.completed = areas,
                        upgrade.types = c("Regular", "Twinkling", "None"),
                        head.filter = head.data.unupgraded$ARMOR,
                        chest.filter = chest.data.unupgraded$ARMOR,
                        hands.filter = hands.data.unupgraded$ARMOR,
                        legs.filter = legs.data.unupgraded$ARMOR,
                        regular.level = c("+0", "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8", "+9", "+10")[1], 
                        twinkling.level = c("+0", "+1", "+2", "+3", "+4", "+5")[1],
                        movement = c("Light", "Mid", "Fat", "Poop")[1],
                        weapons = c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None"),
                        endurance.level = 10,
                        havel.ring = FALSE,
                        favor.ring = FALSE,
                        wolf.ring = FALSE,
                        minima = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                        weights = c(0.16, 0.16, 0.16, 0.16, 0.08, 0.08, 0.08, 0.04, 0.04, 0.04),
                        stat.scale = "sd",
                        weights.mode = "individual"
                    ),
                data = 
                    data.table::data.table(
                        SCORE = numeric(0),
                        SCORE_QUALITY = character(0),
                        HEAD = factor(0),
                        CHEST = factor(0),
                        HANDS = factor(0),
                        LEGS = factor(0),
                        PHYS_DEF = numeric(0),
                        STRIKE_DEF = numeric(0),
                        SLASH_DEF = numeric(0),
                        THRUST_DEF = numeric(0),
                        MAG_DEF = numeric(0),
                        FIRE_DEF = numeric(0),
                        LITNG_DEF = numeric(0),
                        BLEED_RES = numeric(0),
                        POIS_RES = numeric(0),
                        CURSE_RES = numeric(0),
                        DURABILITY = numeric(0),
                        ARMOR_POISE = numeric(0),
                        TOTAL_POISE = numeric(0),
                        POISE_TIMER = numeric(0),
                        ARMOR_WEIGHT = numeric(0),
                        TOTAL_WEIGHT = numeric(0),
                        EQUIP_LOAD = numeric(0),
                        PCT_LOAD = numeric(0)
                    )
            )
        )

    
    output$table <-
        DT::renderDataTable({
            ## The table shows each column at its display precision, from a rounded copy of the exact
            ## values (armordata() keeps those, for the links and the download): its column filters
            ## take their ranges from the data, so they then show the same precision as the cells
            one.decimal <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES", "ARMOR_WEIGHT", "TOTAL_WEIGHT", "EQUIP_LOAD")
            whole <- c("DURABILITY", "ARMOR_POISE", "TOTAL_POISE")
            three.decimals <- c("SCORE", "POISE_TIMER")
            shown <- data.table::copy(armordata()$data)
            shown[, (one.decimal) := lapply(.SD, round, 1), .SDcols = one.decimal]
            shown[, (whole) := lapply(.SD, round, 0), .SDcols = whole]
            shown[, (three.decimals) := lapply(.SD, round, 3), .SDcols = three.decimals]
            shown[, PCT_LOAD := round(PCT_LOAD, 4)]
            DT::datatable(
                shown,
                selection = "single",
                filter = "top", 
                options = 
                list(
                    scrollX = TRUE, 
                    scrollY = TRUE, 
                    scrollCollapse = TRUE, 
                    columnDefs = 
                    list(
                        list(targets = setdiff(colnames(armordata()$data), c("HEAD", "CHEST", "HANDS", "LEGS")), searchable = FALSE)
                    )
                )
            ) |> 
            DT::formatPercentage("PCT_LOAD", 2) |>
            DT::formatCurrency(one.decimal, currency = "", interval = 3, mark = ",", digits = 1) |>
            DT::formatCurrency(whole, currency = "", interval = 3, mark = ",", digits = 0) |>
            DT::formatCurrency(three.decimals, currency = "", interval = 3, mark = ",", digits = 3)
        })


    ## Wiki links for one armor set (a one-row table with HEAD, CHEST, HANDS and LEGS), in a modal -
    ## for a row of the results table, or a point of the trade-offs chart or its table
    show.armor.links <- function(data.selected){
        head.link <- head.data.unupgraded$LINK[match(data.selected$HEAD, head.data.unupgraded$ARMOR)]
        if(head.link != "N/A"){
            output$tabhead <- shiny::renderUI({shiny::tagList("Head: ", shiny::a(data.selected$HEAD, href = head.link, target = "_blank"))})
        } else{
            output$tabhead <- NULL
        }
        chest.link <- chest.data.unupgraded$LINK[match(data.selected$CHEST, chest.data.unupgraded$ARMOR)]
        if(chest.link != "N/A"){
            output$tabchest <- shiny::renderUI({shiny::tagList("Chest: ", shiny::a(data.selected$CHEST, href = chest.link, target = "_blank"))})
        } else{
            output$tabchest <- NULL
        }
        hands.link <- hands.data.unupgraded$LINK[match(data.selected$HANDS, hands.data.unupgraded$ARMOR)]
        if(hands.link != "N/A"){
            output$tabhands <- shiny::renderUI({shiny::tagList("Hands: ", shiny::a(data.selected$HANDS, href = hands.link, target = "_blank"))})
        } else{
            output$tabhands <- NULL
        }
        legs.link <- legs.data.unupgraded$LINK[match(data.selected$LEGS, legs.data.unupgraded$ARMOR)]
        if(legs.link != "N/A"){
            output$tablegs <- shiny::renderUI({shiny::tagList("Legs: ", shiny::a(data.selected$LEGS, href = legs.link, target = "_blank"))})
        } else{
            output$tablegs <- NULL
        }
        shiny::showModal(
            shiny::modalDialog(
                title = "Links to Armor on Wikidot",
                easyClose = TRUE,
                footer = NULL,
                shiny::fluidRow(
                    shiny::column(12, shiny::uiOutput("tabhead")),
                    shiny::column(12, shiny::uiOutput("tabchest")),
                    shiny::column(12, shiny::uiOutput("tabhands")),
                    shiny::column(12, shiny::uiOutput("tablegs"))
                )
            )
        )
    }

    shiny::observeEvent(input$table_rows_selected, {
        show.armor.links(armordata()$data[input$table_rows_selected])
    })


    ## The search settings currently saved from the sidebar's modals, which "Refresh Armor Data" applies
    ## (both tabs then read them back from armordata()$args)
    current.settings <- function(){
        list(
            starting.class = filters$values$starting.class,
            areas.completed = filters$values$areas.completed,
            upgrade.types = filters$values$upgrade.types,
            head.filter = filters$values$head.filter,
            chest.filter = filters$values$chest.filter,
            hands.filter = filters$values$hands.filter,
            legs.filter = filters$values$legs.filter,
            regular.level = upgrades$values$regular.level,
            twinkling.level = upgrades$values$twinkling.level,
            movement = constraints$values$movement,
            weapons = constraints$values$weapons,
            endurance.level = constraints$values$endurance.level,
            havel.ring = rings$values$havel.ring,
            favor.ring = rings$values$favor.ring,
            wolf.ring = rings$values$wolf.ring,
            minima = minima$values$minima,
            weights = scores$values$weights,
            stat.scale = scores$values$stat.scale,
            weights.mode = scores$values$weights.mode
        )
    }

    shiny::observeEvent(input$go, {

        ## Errors abort the refresh (tryCatch below) and leave the previous results in place, but
        ## warnings must not: tryCatch(warning = ...) would stop at the first warning exactly like
        ## an error. withCallingHandlers records each warning and lets the refresh carry on; any
        ## recorded warnings are shown as a notification once it finishes.
        refresh.warnings <- character(0)

        tryCatch(

        withCallingHandlers({

            shinybusy::show_modal_spinner()

            armordata({
                result <- do.call(get.optimal.armor.combos, c(list(max.table.size = filters$values$max.table.size), current.settings()))
                result$data[, c("HEAD", "CHEST", "HANDS", "LEGS") := lapply(.SD, as.factor), .SDcols = c("HEAD", "CHEST", "HANDS", "LEGS")]
                result
            })
            gc()

            been.refreshed(TRUE)

            output$refreshmessage <- shiny::renderText("")
            output$errormessage <- shiny::renderText("")

            for(message in unique(refresh.warnings)){
                shiny::showNotification(message, type = "warning")
            }

        },

        warning = function(w){
            refresh.warnings <<- c(refresh.warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }),

        error = function(e) {
            output$errormessage <- shiny::renderText(conditionMessage(e))
        },

        finally = {
            shinybusy::remove_modal_spinner()
        }

        )

    })


    ## The Trade-offs tab (R/tradeoffs.R), for the last refresh's settings while it's open
    tradeoffs <- tradeoffs.tab.server(
        "tradeoffs", last.args,
        shown = shiny::reactive(identical(input$main_tabs, "Trade-offs")),
        show.links = show.armor.links,
        show.error = function(message){ output$errormessage <- shiny::renderText(message) }
    )


    ## The settings behind a download (the last refresh's), one per row, under the app's own labels
    settings.sheet <- function(args){
        yes.no <- function(x){ if(x) "Yes" else "No" }
        listed <- function(x){ paste(x, collapse = "; ") }
        data.table::data.table(
            SETTING = c(
                "Max Table Size", "Starting Class", "Areas Completed", "Upgrades With",
                "Head", "Chest", "Hands", "Legs",
                "Armor Level (Regular)", "Armor Level (Twinkling)",
                "Havel's Ring", "Ring of Favor", "Wolf Ring",
                "Movement", "Endurance Level", "Right Hand Weapon 1", "Right Hand Weapon 2", "Left Hand Weapon 1", "Left Hand Weapon 2",
                paste("Minimum", MINIMUM.METRICS),
                "Stat Scale", "Weights Mode",
                paste("Score Weight", if(args$weights.mode == "grouped") GROUP.METRICS else WEIGHT.METRICS)
            ),
            VALUE = c(
                format(args$max.table.size, scientific = FALSE), args$starting.class, listed(args$areas.completed), listed(args$upgrade.types),
                listed(args$head.filter), listed(args$chest.filter), listed(args$hands.filter), listed(args$legs.filter),
                args$regular.level, args$twinkling.level,
                yes.no(args$havel.ring), yes.no(args$favor.ring), yes.no(args$wolf.ring),
                args$movement, as.character(args$endurance.level), unname(args$weapons[c("right.1", "right.2", "left.1", "left.2")]),
                as.character(args$minima),
                c(sd = "Std Dev", max = "Max", raw = "Raw")[[args$stat.scale]], c(individual = "Individual", grouped = "Grouped")[[args$weights.mode]],
                paste0(as.character(round(100*args$weights, 6)), "%")
            )
        )
    }

    ## Everything the last refresh produced, in one workbook: the Results table, the Trade-offs table
    ## for the current Maximize choice (computed now if the tab hasn't shown it since that
    ## refresh), and the settings behind both
    output$download <- shiny::downloadHandler(
        ## Stamped with the time of the download (the server's clock), without colons for Windows
        filename = function(){paste0("ds_armor_data_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx")},
        content = function(file){
            if(!been.refreshed()){
                stop("Click 'Refresh Armor Data' before downloading")
            }
            snapshot <- armordata()$args
            writexl::write_xlsx(
                list(
                    Results = armordata()$data,
                    `Trade-offs` = tradeoffs$download.table(snapshot),
                    Settings = settings.sheet(snapshot)
                ),
                file
            )
        }
    )


}
