
server <- function(input, output, session){
    

    output$mode <- shiny::renderText("Mode (Light or Dark)")

    shiny::observeEvent(input$guide, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "User Guide", 
                easyClose = TRUE,
                footer = NULL,
                size = "l",
                shiny::HTML(paste0("

                    To use the app, adjust inputs in the sidebar at left and then click the button 'Refresh Armor Data'. <br> <br>  

                    Filter Inputs: <br>  
                    'Max Table Size' is used to specify how large the created table can be and is used to avoid unnecessary slowness - it cannot be set higher than 100,000. <br> 
                    'Starting Class' is used to specify the character's starting class. This indicates that the associated armor set is available. <br>  
                    'Areas Completed' is used to specify which areas have been completed. This loosely indicates which armor pieces are available. 
                    Please note: the tool will not infer that one area has been completed because another later area has been completed.
                    For example, indicating that Anor Londo has been completed will not result in Sens Fortress being assumed complete. <br>  
                    'Head' is used to specify which head armor pieces can be included in the created table - 'Chest', 'Hands' and 'Legs' are used similarly. <br> <br>

                    Upgrade Inputs: <br>  
                    'Armor Level (Regular)' is used to specify the upgrade level of armor pieces ascended via regular titanite. Options are +0 thru +10. <br>  
                    'Armor Level (Twinkling)' is used to specify the upgrade level of armor pieces ascended via twinkling titanite. Options are +0 thru +5. <br>  
                    For the options between unupgraded and fully upgraded, metrics are approximated based on the game's default upgrade patterns.
                    These approximations should be very accurate but will differ slightly from true values. <br> <br>

                    Ring Inputs: <br>  
                    'Havel's Ring', 'Ring of Favor', and 'Wolf Ring' are used to specify whether the player has the relevant ring equipped.
                    The app will allow all three to be selected, but this is obviously not possible in game. <br>  
                    Havel's Ring and the Ring of Favor both boost equip load which is helpful when trying to achieve a faster roll speed. 
                    The Wolf Ring gives 40 poise and is immensely helpful in hitting key poise breakpoints. These are 21/46/61 for PVE and 31/61 for PVP, as explained here: ", 
                    tags$a("Dark Souls Dissected #13 - Poise Mechanics (and glitches!)", href = "https://www.youtube.com/watch?v=pwffSOSzcAM", target = "_blank"), " . <br> <br>

                    Load Inputs: <br>  
                    'Roll Type' is used to specify the player's desired roll speed. <br>  
                    'Weight without Armor' is used to specify the character's weight before any armor pieces have been equipped. <br>  
                    'Endurance Level' is used to specify the character's current level in the Endurance stat - Endurance affects equip load. <br> <br>  

                    Minimum Inputs: <br>  
                    Here, minima may be specified for a set of relevant metrics. Only combinations which achieve or exceed the specified minima will be considered. <br> <br>

                    Weight Inputs: <br>  
                    Here, weights may be specified for a set of relevant metrics. A score for each armor combination is calculated as
                    (w_1*x_1+...+w_n*x_n)/(w_1+...+w_n), where each x_i is the standardized value of the relevant metric (standardized means that all metrics have been shifted and scaled to mean 0 and variance 1). 
                    This overall score is then transformed so that it also has mean 0 and variance 1. This value is presented as 'SCORE_RAW' and is also transformed to an approximate percentile between 0 and 1 and presented as 'SCORE_PCT'.  
                    A value of 100% is the best possible for the set of weights specified and a value of 0% is the worst possible.
                    The percentage is computed by  applying the standard normal cumulative distribution function to SCORE_RAW. 
                    These scores are global within the same set of weights: direct comparisons can be made across different inputs. <br> <br>

                    Miscellaneous notes: <br> <br>
                    Some armor pieces reduce stamina regeneration speed, as does being above 50% load or 100% load. Information on this can be found here: ",
                    tags$a("Stamina", href = "http://darksouls.wikidot.com/stamina#toc3 ", target = "_blank"), " <br> <br>
                    Durability is aggregated by taking the minimum i.e. the total durability for a set is the lowest durability across each component of the set. <br> <br> 
                    The impact to equip load of Mask of the Father (x1.05) is accounted for, but the impact to magic defense of Crown of Dusk (x0.7) is not. <br> <br>
                    Clicking on a row in the table will produce a set of links to the Dark Souls Wikidot site for the relevant armor pieces. <br> <br>",
                    "If maximizing Physical Defenses, Elemental Defenses, or Resistances, the following non-armor items are useful: <br>",
                    tags$a("Ring of Steel Protection", href = "http://darksouls.wikidot.com/ring-of-steel-protection", target = "_blank"), " (+50 to all Physical Defenses) <br>",
                    tags$a("Spell Stoneplate Ring", href = "http://darksouls.wikidot.com/spell-stoneplate-ring", target = "_blank"), " (+50 Magic Defense) <br>",
                    tags$a("Flame Stoneplate Ring", href = "http://darksouls.wikidot.com/flame-stoneplate-ring", target = "_blank"), " (+50 Fire Defense) <br>",
                    tags$a("Thunder Stoneplate Ring", href = "http://darksouls.wikidot.com/thunder-stoneplate-ring", target = "_blank"), " (+50 Lightning Defense) <br>",
                    tags$a("Speckled Stoneplate Ring", href = "http://darksouls.wikidot.com/speckled-stoneplate-ring", target = "_blank"), " (+25 to all Elemental Defenses)  <br>",
                    tags$a("Poisonbite Ring", href = "http://darksouls.wikidot.com/poisonbite-ring", target = "_blank"), " (x4 Unarmored Poison Resistance) <br>",
                    tags$a("Bloodbite Ring", href = "http://darksouls.wikidot.com/bloodbite-ring", target = "_blank"), " (x4 Unarmored Bleed Resistance) <br>",
                    tags$a("Cursebite Ring", href = "http://darksouls.wikidot.com/cursebite-ring", target = "_blank"), " (x4 Unarmored Curse Resistance) <br>",
                    tags$a("Gargoyle's Halberd", href = "http://darksouls.wikidot.com/gargoyle-halberd/", target = "_blank"), " (x1.25 Unarm Pois/Bleed Res) <br>",
                    tags$a("Gargoyle Tail Axe", href = "http://darksouls.wikidot.com/gargoyle-tail-axe", target = "_blank"), " (x2 Unarm Pois/Bleed Res) <br>",
                    tags$a("Bloodshield", href = "http://darksouls.wikidot.com/bloodshield", target = "_blank"), " (x1.5 Unarm Pois/Bleed/Curse Res) <br>",
                    tags$a("Humanity", href = "http://darksouls.wikidot.com/humanity", target = "_blank"), " (Boosts all Defenses and Curse Res, see Wiki) <br> <br>",
                    "
                    The improvements to magic defense of the Spell Stoneplate Ring and Speckled Stoneplate Ring apply after the reduction of Crown of Dusk. <br> <br>
                    In general, Toxic Resistance is the same as Poison Resistance, with the exception of the Poisonbite Ring: it does not improve Toxic Resistance. <br> <br>
                    To explain Unarmored Resistance: if base Bleed Resistance is X, Armor Bleed Resistance is Y, and the Bloodbite Ring and Gargoyle Tail Axe are equipped, then final Bleed Resistance is X*4*2+Y. <br> <br>
                    Links to the Wikidot site have been included in various parts of this app. 
                    However, over the course of this app's creation, I have found multiple inaccuracies there (no disrespect intended towards all the great folks who have taken the time and energy to contribute).
                    Do not be surprised if it contradicts the information here.
                "))
            ) 
        ) 
    }) 


    been.refreshed <- shiny::reactiveVal(FALSE)


    inputs.unchanged <- 
        shiny::reactiveValues(
            filter.values = TRUE,
            upgrade.values = TRUE,
            ring.values = TRUE,
            constraint.values = TRUE,
            minimum.values = TRUE,
            weight.values = TRUE
        )

    shiny::observe({
        if(been.refreshed()){
            if(all(unlist(shiny::reactiveValuesToList(inputs.unchanged)))){
                output$refreshmessage <- shiny::renderText("")
            } else{
                output$refreshmessage <- shiny::renderText("Inputs have changed - click 'Refresh Armor Data' to pull new results")
            }
        } else{
            output$refreshmessage <- shiny::renderText("Adjust settings in the sidebar and click 'Refresh Armor Data' to pull results")
        }
    })


    filter.values <- 
        shiny::reactiveValues(
            max.table.size = 1000,
            starting.class = classes[1], 
            areas.completed = areas, 
            upgrade.types = c("Regular", "Twinkling", "None"),
            head.filter = head.data_00$ARMOR,
            chest.filter = chest.data_00$ARMOR,
            hands.filter = hands.data_00$ARMOR,
            legs.filter = legs.data_00$ARMOR
        )
    
    shiny::observeEvent(input$filters, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Filter Inputs", 
                footer = shiny::actionButton(inputId = "dismiss_filter_modal", label = "Done"), 
                shiny::fluidRow(
                    shinyWidgets::autonumericInput(
                        inputId = "max.table.size",
                        label = "Max Table Size",
                        value = filter.values$max.table.size,
                        align = "right",
                        decimalCharacter = ".",
                        digitGroupSeparator = ",",
                        decimalPlaces = 0,
                        maximumValue = 100000,
                        minimumValue = 1
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "starting.class",
                        label = "Starting Class",
                        choices = classes,
                        selected = filter.values$starting.class,
                        multiple = FALSE,
                        options = list(size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "areas.completed",
                        label = "Areas Completed",
                        choices = areas,
                        selected = filter.values$areas.completed,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "upgrade.types",
                        label = "Upgrades With",
                        choices = c("Regular", "Twinkling", "None"),
                        selected = filter.values$upgrade.types,
                        multiple = TRUE,
                        options = list(size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "head.filter",
                        label = "Head",
                        choices = head.data_00$ARMOR,
                        selected = filter.values$head.filter,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "chest.filter",
                        label = "Chest",
                        choices = chest.data_00$ARMOR,
                        selected = filter.values$chest.filter,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "hands.filter",
                        label = "Hands",
                        choices = hands.data_00$ARMOR,
                        selected = filter.values$hands.filter,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "legs.filter",
                        label = "Legs",
                        choices = legs.data_00$ARMOR,
                        selected = filter.values$legs.filter,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
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

    shiny::observeEvent(input$dismiss_filter_modal, {

        if(shiny::isTruthy(input$max.table.size)){filter.values$max.table.size <- round(input$max.table.size, 0)}
        if(shiny::isTruthy(input$starting.class)){filter.values$starting.class <- input$starting.class}
        if(shiny::isTruthy(input$areas.completed)){filter.values$areas.completed <- input$areas.completed} else if(length(input$areas.completed) == 0){filter.values$areas.completed <- character(0)}
        if(shiny::isTruthy(input$upgrade.types)){filter.values$upgrade.types <- input$upgrade.types} else if(length(input$upgrade.types) == 0){filter.values$upgrade.types <- character(0)}
        if(shiny::isTruthy(input$head.filter)){filter.values$head.filter <- input$head.filter} else if(length(input$head.filter) == 0){
            filter.values$head.filter <- "No Head"
            shinyWidgets::updatePickerInput(inputId = "head.filter", selected = "No Head")
        }
        if(shiny::isTruthy(input$chest.filter)){filter.values$chest.filter <- input$chest.filter} else if(length(input$chest.filter) == 0){
            filter.values$chest.filter <- "No Chest"
            shinyWidgets::updatePickerInput(inputId = "chest.filter", selected = "No Chest")
        }
        if(shiny::isTruthy(input$hands.filter)){filter.values$hands.filter <- input$hands.filter} else if(length(input$hands.filter) == 0){
            filter.values$hands.filter <- "No Hands"
            shinyWidgets::updatePickerInput(inputId = "hands.filter", selected = "No Hands")
        }
        if(shiny::isTruthy(input$legs.filter)){filter.values$legs.filter <- input$legs.filter} else if(length(input$legs.filter) == 0){
            filter.values$legs.filter <- "No Legs"
            shinyWidgets::updatePickerInput(inputId = "legs.filter", selected = "No Legs")
        }

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$filter.values <- all(sapply(names(filter.values), function(name){identical(curr.vals[[name]], filter.values[[name]])}))
        }
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    upgrade.values <- 
        shiny::reactiveValues(
            regular.level = "+0", 
            twinkling.level = "+0"
        )
    
    shiny::observeEvent(input$upgrades, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Upgrade Inputs", 
                footer = shiny::actionButton(inputId = "dismiss_upgrade_modal", label = "Done"), 
                shiny::fluidRow(
                    shinyWidgets::pickerInput(
                        inputId = "regular.level",
                        label = "Armor Level (Regular)",
                        choices = c("+0", "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8", "+9", "+10"),
                        selected = upgrade.values$regular.level,
                        multiple = FALSE,
                        options = list(size = 11),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "twinkling.level",
                        label = "Armor Level (Twinkling)",
                        choices = c("+0", "+1", "+2", "+3", "+4", "+5"),
                        selected = upgrade.values$twinkling.level,
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

    shiny::observeEvent(input$dismiss_upgrade_modal, {

        if(shiny::isTruthy(input$regular.level)){upgrade.values$regular.level <- input$regular.level}
        if(shiny::isTruthy(input$twinkling.level)){upgrade.values$twinkling.level <- input$twinkling.level}

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$upgrade.values <- all(sapply(names(upgrade.values), function(name){identical(curr.vals[[name]], upgrade.values[[name]])}))
        }
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    ring.values <- 
        shiny::reactiveValues(
            havel.ring = FALSE,
            fap.ring = FALSE,
            wolf.ring = FALSE
        )
    
    shiny::observeEvent(input$rings, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Ring Inputs", 
                footer = shiny::actionButton(inputId = "dismiss_ring_modal", label = "Done"), 
                shiny::fluidRow(
                    bslib::input_switch(id = "havel.ring", label = "Havel's Ring (+50% Eq Load)", value = ring.values$havel.ring, width = NULL),
                    bslib::input_switch(id = "fap.ring", label = "Ring of Favor (+20% Eq Load)", value = ring.values$fap.ring, width = NULL),
                    bslib::input_switch(id = "wolf.ring", label = "Wolf Ring (+40 Poise)", value = ring.values$wolf.ring, width = NULL)
                )
            ) 
        ) 
    })

    shiny::observeEvent(input$dismiss_ring_modal, {

        if(shiny::isTruthy(input$havel.ring)){ring.values$havel.ring <- input$havel.ring} else if(length(input$havel.ring) == 1){if(!is.na(input$havel.ring) && is.logical(input$havel.ring)){ring.values$havel.ring <- input$havel.ring}}
        if(shiny::isTruthy(input$fap.ring)){ring.values$fap.ring <- input$fap.ring} else if(length(input$fap.ring) == 1){if(!is.na(input$fap.ring) && is.logical(input$fap.ring)){ring.values$fap.ring <- input$fap.ring}}
        if(shiny::isTruthy(input$wolf.ring)){ring.values$wolf.ring <- input$wolf.ring} else if(length(input$wolf.ring) == 1){if(!is.na(input$wolf.ring) && is.logical(input$wolf.ring)){ring.values$wolf.ring <- input$wolf.ring}}

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$ring.values <- all(sapply(names(ring.values), function(name){identical(curr.vals[[name]], ring.values[[name]])}))
        }
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    constraint.values <- 
        shiny::reactiveValues(
            roll = "Fast",
            unarmored.weight = 10, 
            endurance.level = 10
        )
    
    shiny::observeEvent(input$constraints, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Load Inputs", 
                footer = shiny::actionButton(inputId = "dismiss_constraint_modal", label = "Done"), 
                shiny::fluidRow(
                    shiny::radioButtons(
                        inputId = "roll",
                        label = "Roll Type",
                        choices = c("Fast", "Mid", "Fat", "None"),
                        selected = constraint.values$roll,
                        inline = TRUE,
                        width = NULL,
                        choiceNames = NULL,
                        choiceValues = NULL
                    ),
                    shinyWidgets::autonumericInput(
                        inputId = "unarmored.weight",
                        label = "Weight without Armor",
                        value = constraint.values$unarmored.weight,
                        align = "right",
                        decimalCharacter = ".",
                        digitGroupSeparator = ",",
                        decimalPlaces = 1,
                        maximumValue = 999,
                        minimumValue = 0
                    ),
                    shinyWidgets::autonumericInput(
                        inputId = "endurance.level",
                        label = "Endurance Level",
                        value = constraint.values$endurance.level,
                        align = "right",
                        decimalCharacter = ".",
                        digitGroupSeparator = ",",
                        decimalPlaces = 0,
                        maximumValue = 99,
                        minimumValue = 0
                    )
                )
            ) 
        ) 
    })

    shiny::observeEvent(input$dismiss_constraint_modal, {

        if(shiny::isTruthy(input$roll)){constraint.values$roll <- input$roll}
        if(shiny::isTruthy(input$unarmored.weight)){constraint.values$unarmored.weight <- round(input$unarmored.weight, 0)}
        if(shiny::isTruthy(input$endurance.level)){constraint.values$endurance.level <- round(input$endurance.level, 0)}

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$constraint.values <- all(sapply(names(constraint.values), function(name){identical(curr.vals[[name]], constraint.values[[name]])}))
        }
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    minimum.values <- shiny::reactiveValues(minima = rep(0.0, 12))

    shiny::observeEvent(input$minima, { 
      shiny::showModal( 
        shiny::modalDialog( 
          title = "Minimum Inputs", 
          footer = shiny::actionButton(inputId = "dismiss_minimum_modal", label = "Done"), 
          shiny::fluidRow(
            shiny::column(width = 3,
                   shinyWidgets::autonumericInput(
                     inputId = "minphysdef",
                     label = "PHYS_DEF",
                     value = minimum.values$minima[1],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "minmagdef",
                     label = "MAG_DEF",
                     value = minimum.values$minima[5],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "minbleedres",
                     label = "BLEED_RES",
                     value = minimum.values$minima[9],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   )
            ),
            shiny::column(width = 3,
                   shinyWidgets::autonumericInput(
                     inputId = "minstrikedef",
                     label = "STRIKE_DEF",
                     value = minimum.values$minima[2],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "minfiredef",
                     label = "FIRE_DEF",
                     value = minimum.values$minima[6],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "minpoisres",
                     label = "POIS_RES",
                     value = minimum.values$minima[10],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   )
            ),
            shiny::column(width = 3,
                   shinyWidgets::autonumericInput(
                     inputId = "minslashdef",
                     label = "SLASH_DEF",
                     value = minimum.values$minima[3],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "minlitngdef",
                     label = "LITNG_DEF",
                     value = minimum.values$minima[7],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "mincurseres",
                     label = "CURSE_RES",
                     value = minimum.values$minima[11],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   )
            ),
            shiny::column(width = 3,
                   shinyWidgets::autonumericInput(
                     inputId = "minthrustdef",
                     label = "THRUST_DEF",
                     value = minimum.values$minima[4],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 1,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "minpoise",
                     label = "POISE",
                     value = minimum.values$minima[8],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 0,
                     maximumValue = 999,
                     minimumValue = 0
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "mindurability",
                     label = "DURABILITY",
                     value = minimum.values$minima[12],
                     align = "right",
                     decimalCharacter = ".",
                     digitGroupSeparator = ",",
                     decimalPlaces = 0,
                     maximumValue = 999,
                     minimumValue = 0
                   )
            )
          )
        ) 
      ) 
    })

    shiny::observeEvent(input$dismiss_minimum_modal, {

        if(
            shiny::isTruthy(input$minphysdef) &&
            shiny::isTruthy(input$minstrikedef) &&
            shiny::isTruthy(input$minslashdef) &&
            shiny::isTruthy(input$minthrustdef) &&
            shiny::isTruthy(input$minmagdef) &&
            shiny::isTruthy(input$minfiredef) &&
            shiny::isTruthy(input$minlitngdef) &&
            shiny::isTruthy(input$minpoise) &&
            shiny::isTruthy(input$minbleedres) &&
            shiny::isTruthy(input$minpoisres) &&
            shiny::isTruthy(input$mincurseres) &&
            shiny::isTruthy(input$mindurability)
        ){
            minimum.values$minima <- 
                round(c(
                    input$minphysdef, input$minstrikedef, input$minslashdef, input$minthrustdef,
                    input$minmagdef, input$minfiredef, input$minlitngdef, input$minpoise,
                    input$minbleedres, input$minpoisres, input$mincurseres, input$mindurability
                ), 1)
        }

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$minimum.values <- all(sapply(names(minimum.values), function(name){identical(curr.vals[[name]], minimum.values[[name]])}))
        }
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    weight.values <- shiny::reactiveValues(weights = c(16.0, 16.0, 16.0, 16.0, 8.0, 8.0, 8.0, 4.0, 4.0, 4.0))
    
    shiny::observeEvent(input$weights, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Score Inputs", 
                footer = shiny::fluidRow(shiny::column(8, shiny::actionButton(inputId = "normalize_weights", label = "Normalize to 100%")), shiny::column(4, shiny::actionButton(inputId = "dismiss_weight_modal", label = "Done"))), 
                shiny::fluidRow(
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "physdefweight",
                                label = "PHYS_DEF",
                                value = weight.values$weights[1],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "magdefweight",
                                label = "MAG_DEF",
                                value = weight.values$weights[5],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "bleedresweight",
                                label = "BLEED_RES",
                                value = weight.values$weights[8],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    ),
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "strikedefweight",
                                label = "STRIKE_DEF",
                                value = weight.values$weights[2],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "firedefweight",
                                label = "FIRE_DEF",
                                value = weight.values$weights[6],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "poisresweight",
                                label = "POIS_RES",
                                value = weight.values$weights[9],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    ),
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "slashdefweight",
                                label = "SLASH_DEF",
                                value = weight.values$weights[3],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "litngdefweight",
                                label = "LITNG_DEF",
                                value = weight.values$weights[7],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "curseresweight",
                                label = "CURSE_RES",
                                value = weight.values$weights[10],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    ),
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "thrustdefweight",
                                label = "THRUST_DEF",
                                value = weight.values$weights[4],
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 1,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    )
                )
            ) 
        ) 
    })

    shiny::observeEvent(input$normalize_weights, {

        if(
            shiny::isTruthy(input$physdefweight) &&
            shiny::isTruthy(input$strikedefweight) &&
            shiny::isTruthy(input$slashdefweight) &&
            shiny::isTruthy(input$thrustdefweight) &&
            shiny::isTruthy(input$magdefweight) &&
            shiny::isTruthy(input$firedefweight) &&
            shiny::isTruthy(input$litngdefweight) &&
            shiny::isTruthy(input$bleedresweight) &&
            shiny::isTruthy(input$poisresweight) &&
            shiny::isTruthy(input$curseresweight)
        ){

            weights <- 
                c(
                    input$physdefweight, input$strikedefweight, input$slashdefweight, input$thrustdefweight,
                    input$magdefweight, input$firedefweight, input$litngdefweight, 
                    input$bleedresweight, input$poisresweight, input$curseresweight
                )
            
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
            shinyWidgets::updateAutonumericInput(inputId = "physdefweight", value = weights[1])
            shinyWidgets::updateAutonumericInput(inputId = "strikedefweight", value = weights[2])
            shinyWidgets::updateAutonumericInput(inputId = "slashdefweight", value = weights[3])
            shinyWidgets::updateAutonumericInput(inputId = "thrustdefweight", value = weights[4])
            shinyWidgets::updateAutonumericInput(inputId = "magdefweight", value = weights[5])
            shinyWidgets::updateAutonumericInput(inputId = "firedefweight", value = weights[6])
            shinyWidgets::updateAutonumericInput(inputId = "litngdefweight", value = weights[7])
            shinyWidgets::updateAutonumericInput(inputId = "bleedresweight", value = weights[8])
            shinyWidgets::updateAutonumericInput(inputId = "poisresweight", value = weights[9])
            shinyWidgets::updateAutonumericInput(inputId = "curseresweight", value = weights[10])

        } 
        
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$dismiss_weight_modal, {

        if(
            shiny::isTruthy(input$physdefweight) &&
            shiny::isTruthy(input$strikedefweight) &&
            shiny::isTruthy(input$slashdefweight) &&
            shiny::isTruthy(input$thrustdefweight) &&
            shiny::isTruthy(input$magdefweight) &&
            shiny::isTruthy(input$firedefweight) &&
            shiny::isTruthy(input$litngdefweight) &&
            shiny::isTruthy(input$bleedresweight) &&
            shiny::isTruthy(input$poisresweight) &&
            shiny::isTruthy(input$curseresweight)
        ){
            weight.values$weights <- 
                c(
                    input$physdefweight, input$strikedefweight, input$slashdefweight, input$thrustdefweight,
                    input$magdefweight, input$firedefweight, input$litngdefweight, 
                    input$bleedresweight, input$poisresweight, input$curseresweight
                )
        }

        if(been.refreshed()){
            ## Custom logic here due to special nature of weights argument
            inputs.unchanged$weight.values <- (abs(weight.values$weights/sum(weight.values$weights)-armordata()$args$weights) < 1e-10) 
        }
        
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    armordata <- 
        shiny::reactiveVal(
            list(
                args = 
                    list(
                        max.table.size = 1000,
                        starting.class = classes[1],
                        areas.completed = areas,
                        upgrade.types = c("Regular", "Twinkling", "None"),
                        head.filter = head.data_00$ARMOR,
                        chest.filter = chest.data_00$ARMOR,
                        hands.filter = hands.data_00$ARMOR,
                        legs.filter = legs.data_00$ARMOR,
                        regular.level = c("+0", "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8", "+9", "+10")[1], 
                        twinkling.level = c("+0", "+1", "+2", "+3", "+4", "+5")[1],
                        roll = c("Fast", "Mid", "Fat", "None")[1],
                        unarmored.weight = 10,
                        endurance.level = 10,
                        havel.ring = FALSE,
                        fap.ring = FALSE,
                        wolf.ring = FALSE,
                        minima = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                        weights = c(0.16, 0.16, 0.16, 0.16, 0.08, 0.08, 0.08, 0.04, 0.04, 0.04)
                    ),
                data = 
                    data.table::data.table(
                        SCORE_RAW = numeric(0),
                        SCORE_PCT = numeric(0),
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
                        ARMOR_WEIGHT = numeric(0),
                        TOTAL_WEIGHT = numeric(0),
                        EQUIP_LOAD = numeric(0),
                        PCT_LOAD = numeric(0)
                    )
            )
        )

    
    output$table <- 
        DT::renderDataTable({
            DT::datatable(
                armordata()$data,
                selection = "single",
                filter = "top", 
                options = 
                list(
                    scrollX = TRUE, 
                    scrollY = TRUE, 
                    scrollCollapse = TRUE, 
                    columnDefs = 
                    list(
                        list(targets = c(1, 2, 7:23), searchable = FALSE)
                    )
                )
            ) |> 
            DT::formatPercentage(c("SCORE_PCT", "PCT_LOAD"), 2) |>
            DT::formatCurrency(c(
              "PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF",
                "MAG_DEF", "FIRE_DEF", "LITNG_DEF",
                "BLEED_RES", "POIS_RES", "CURSE_RES",
                "ARMOR_WEIGHT", "TOTAL_WEIGHT", "EQUIP_LOAD"
            ), currency = "", interval = 3, mark = ",", digits = 1) |>
            DT::formatCurrency(c("DURABILITY", "ARMOR_POISE", "TOTAL_POISE"), currency = "", interval = 3, mark = ",", digits = 0) |>
            DT::formatCurrency("SCORE_RAW", currency = "", interval = 3, mark = ",", digits = 3)
        })


    shiny::observeEvent(input$table_rows_selected, {
        data.selected <- armordata()$data[input$table_rows_selected]
        head.link <- head.data_00$LINK[match(data.selected$HEAD, head.data_00$ARMOR)]
        if(head.link != "N/A"){
            output$tabhead <- shiny::renderUI({shiny::tagList("Head: ", shiny::a(data.selected$HEAD, href = head.link, target = "_blank"))})
        } else{
            output$tabhead <- NULL
        }
        chest.link <- chest.data_00$LINK[match(data.selected$CHEST, chest.data_00$ARMOR)]
        if(chest.link != "N/A"){
            output$tabchest <- shiny::renderUI({shiny::tagList("Chest: ", shiny::a(data.selected$CHEST, href = chest.link, target = "_blank"))})
        } else{
            output$tabchest <- NULL
        }
        hands.link <- hands.data_00$LINK[match(data.selected$HANDS, hands.data_00$ARMOR)]
        if(hands.link != "N/A"){
            output$tabhands <- shiny::renderUI({shiny::tagList("Hands: ", shiny::a(data.selected$HANDS, href = hands.link, target = "_blank"))})
        } else{
            output$tabhands <- NULL
        }
        legs.link <- legs.data_00$LINK[match(data.selected$LEGS, legs.data_00$ARMOR)]
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
                    shiny::column(12, uiOutput("tabhead")),
                    shiny::column(12, uiOutput("tabchest")),
                    shiny::column(12, uiOutput("tabhands")),
                    shiny::column(12, uiOutput("tablegs"))
                )
            )
        )
    })


    shiny::observeEvent(input$go, {

        tryCatch(

        {   
            
            shinybusy::show_modal_spinner()

            armordata(
                get.optimal.armor.combos(
                    max.table.size = filter.values$max.table.size,
                    starting.class = filter.values$starting.class,
                    areas.completed = filter.values$areas.completed,
                    upgrade.types = filter.values$upgrade.types,
                    head.filter = filter.values$head.filter,
                    chest.filter = filter.values$chest.filter,
                    hands.filter = filter.values$hands.filter,
                    legs.filter = filter.values$legs.filter,
                    regular.level = upgrade.values$regular.level,
                    twinkling.level = upgrade.values$twinkling.level,
                    roll = constraint.values$roll,
                    unarmored.weight = constraint.values$unarmored.weight,
                    endurance.level = constraint.values$endurance.level,
                    havel.ring = ring.values$havel.ring,
                    fap.ring = ring.values$fap.ring,
                    wolf.ring = ring.values$wolf.ring,
                    minima = minimum.values$minima,
                    weights = weight.values$weights
                )
            )
            gc()

            armordata()$data[, c("HEAD", "CHEST", "HANDS", "LEGS") := lapply(.SD, as.factor), .SDcols = c("HEAD", "CHEST", "HANDS", "LEGS")]

            been.refreshed(TRUE)

            inputs.unchanged$filter.values <- TRUE
            inputs.unchanged$upgrade.values <- TRUE
            inputs.unchanged$ring.values <- TRUE
            inputs.unchanged$constraint.values <- TRUE
            inputs.unchanged$minimum.values <- TRUE
            inputs.unchanged$weight.values <- TRUE

            output$refreshmessage <- shiny::renderText("")
            output$errormessage <- shiny::renderText("")

        },
        
        warning = function(w){
            output$errormessage <- shiny::renderText(conditionMessage(w))
        },
            
        error = function(e) {
            output$errormessage <- shiny::renderText(conditionMessage(e))
        },

        finally = {
            shinybusy::remove_modal_spinner()
        }

        )

    })


    output$download <- shiny::downloadHandler(
        filename = function(){"ds_armor_data.csv"}, 
        content = function(file){
            data.table::fwrite(armordata()$data, file)
        }
    )


}
