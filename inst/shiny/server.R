
server <- function(input, output, session){
    

    output$mode <- shiny::renderText("Mode (Light or Dark)")
    output$rings <- shiny::renderText("Rings")

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
                    Please note: the tool will not infer that one area has been completed because another later area has been completed. <br>  
                    'Upgrades With' is used to specify which upgrade materials to consider. For example, only armor pieces that upgrade with twinkling titanite could be considered. <br>  
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
                    The Wolf Ring gives 40 poise and is immensely helpful in hitting key poise breakpoints. 
                    Some useful breakpoints are 21/46/61 for PVE and 31/61 for PVP, as explained here: ", 
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
                    This overall score is then transformed to an approximate percentile between 0 and 1 to make interpretation easier. 
                    A value of 100% is the best possible for the set of weights specified and a value of 0% is the worst possible.
                    The value is created by (1) standardizing the scores (w_1*x_1+...+w_n*x_n)/(w_1+...+w_n) and (2) applying the standard normal cumulative distribution function. 
                    Important note: values should only be compared within the same selections of 'Armor Level (Regular)' and 'Armor Level (Twinkling)', since they are calculated with 
                    respect to the applicable mean and variance of such armor combinations. <br> <br>

                    Miscellaneous notes: <br> <br>
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


    filter.values <- 
        shiny::reactiveValues(
            tablesize = 1000,
            class = classes[1], 
            area = areas, 
            upgradetype = c("Regular", "Twinkling", "None"),
            head = head.data_00$ARMOR,
            chest = chest.data_00$ARMOR,
            hands = hands.data_00$ARMOR,
            legs = legs.data_00$ARMOR
        )

    shiny::observeEvent(
        list(input$tablesize, input$class, input$area, input$upgradetype, input$head, input$chest, input$hands, input$legs), 
    {
        if(shiny::isTruthy(input$tablesize)){filter.values$tablesize <- input$tablesize}
        if(shiny::isTruthy(input$class)){filter.values$class <- input$class}
        if(shiny::isTruthy(input$area)){filter.values$area <- input$area} else if(length(input$area) == 0){filter.values$area <- character(0)}
        if(shiny::isTruthy(input$upgradetype)){filter.values$upgradetype <- input$upgradetype} else if(length(input$upgradetype) == 0){filter.values$upgradetype <- character(0)}
        if(shiny::isTruthy(input$head)){filter.values$head <- input$head} else if(length(input$head) == 0){
            filter.values$head <- "No Head"
            shinyWidgets::updatePickerInput(inputId = "head", selected = "No Head")
        }
        if(shiny::isTruthy(input$chest)){filter.values$chest <- input$chest} else if(length(input$chest) == 0){
            filter.values$chest <- "No Chest"
            shinyWidgets::updatePickerInput(inputId = "chest", selected = "No Chest")
        }
        if(shiny::isTruthy(input$hands)){filter.values$hands <- input$hands} else if(length(input$hands) == 0){
            filter.values$hands <- "No Hands"
            shinyWidgets::updatePickerInput(inputId = "hands", selected = "No Hands")
        }
        if(shiny::isTruthy(input$legs)){filter.values$legs <- input$legs} else if(length(input$legs) == 0){
            filter.values$legs <- "No Legs"
            shinyWidgets::updatePickerInput(inputId = "legs", selected = "No Legs")
        }
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$filters, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Filter Inputs", 
                footer = shiny::modalButton("Done"),
                shiny::fluidRow(
                    shinyWidgets::autonumericInput(
                        inputId = "tablesize",
                        label = "Max Table Size",
                        value = filter.values$tablesize,
                        align = "right",
                        decimalCharacter = ".",
                        digitGroupSeparator = ",",
                        decimalPlaces = 0,
                        maximumValue = 100000,
                        minimumValue = 1
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "class",
                        label = "Starting Class",
                        choices = classes,
                        selected = filter.values$class,
                        multiple = FALSE,
                        options = list(size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "area",
                        label = "Areas Completed",
                        choices = areas,
                        selected = filter.values$area,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "upgradetype",
                        label = "Upgrades With",
                        choices = c("Regular", "Twinkling", "None"),
                        selected = filter.values$upgradetype,
                        multiple = TRUE,
                        options = list(size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "head",
                        label = "Head",
                        choices = head.data_00$ARMOR,
                        selected = filter.values$head,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "chest",
                        label = "Chest",
                        choices = chest.data_00$ARMOR,
                        selected = filter.values$chest,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "hands",
                        label = "Hands",
                        choices = hands.data_00$ARMOR,
                        selected = filter.values$hands,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "legs",
                        label = "Legs",
                        choices = legs.data_00$ARMOR,
                        selected = filter.values$legs,
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


    upgrade.values <- 
        shiny::reactiveValues(
            reglvl = "+0", 
            twinklvl = "+0"
        )

    shiny::observeEvent(
        list(input$reglvl, input$twinklvl), 
    {
        if(shiny::isTruthy(input$reglvl)){upgrade.values$reglvl <- input$reglvl}
        if(shiny::isTruthy(input$twinklvl)){upgrade.values$twinklvl <- input$twinklvl}
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$upgrades, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Upgrade Inputs", 
                footer = shiny::modalButton("Done"),
                shiny::fluidRow(
                    shinyWidgets::pickerInput(
                        inputId = "reglvl",
                        label = "Armor Level (Regular)",
                        choices = c("+0", "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8", "+9", "+10"),
                        selected = upgrade.values$reglvl,
                        multiple = FALSE,
                        options = list(size = 11),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "twinklvl",
                        label = "Armor Level (Twinkling)",
                        choices = c("+0", "+1", "+2", "+3", "+4", "+5"),
                        selected = upgrade.values$twinklvl,
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


    ring.values <- 
        shiny::reactiveValues(
            havel = FALSE,
            fap = FALSE,
            wolf = FALSE
        )

    shiny::observeEvent(
        list(input$havel, input$fap, input$wolf), 
    {
        if(shiny::isTruthy(input$havel)){constraint.values$havel <- input$havel} else if(length(input$havel) == 1){if(!is.na(input$havel) && is.logical(input$havel)){constraint.values$havel <- input$havel}}
        if(shiny::isTruthy(input$fap)){constraint.values$fap <- input$fap} else if(length(input$fap) == 1){if(!is.na(input$fap) && is.logical(input$fap)){constraint.values$fap <- input$fap}}
        if(shiny::isTruthy(input$wolf)){constraint.values$wolf <- input$wolf} else if(length(input$wolf) == 1){if(!is.na(input$wolf) && is.logical(input$wolf)){constraint.values$wolf <- input$wolf}}
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$rings, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Ring Inputs", 
                footer = shiny::modalButton("Done"),
                shiny::fluidRow(
                    # shiny::textOutput(outputId = "rings"),
                    bslib::input_switch(id = "havel", label = "Havel's Ring (+50% Eq Load)", value = constraint.values$havel, width = NULL),
                    bslib::input_switch(id = "fap", label = "Ring of Favor (+20% Eq Load)", value = constraint.values$fap, width = NULL),
                    bslib::input_switch(id = "wolf", label = "Wolf Ring (+40 Poise)", value = constraint.values$wolf, width = NULL)
                )
            ) 
        ) 
    })


    constraint.values <- 
        shiny::reactiveValues(
            roll = "Fast",
            baseweight = 10, 
            endurance = 10, 
            havel = FALSE,
            fap = FALSE,
            wolf = FALSE,
            minpoise = 0
        )

    shiny::observeEvent(
        list(input$roll, input$baseweight, input$endurance), 
    {
        if(shiny::isTruthy(input$roll)){constraint.values$roll <- input$roll}
        if(shiny::isTruthy(input$baseweight)){constraint.values$baseweight <- input$baseweight}
        if(shiny::isTruthy(input$endurance)){constraint.values$endurance <- input$endurance}
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$constraints, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Load Inputs", 
                footer = shiny::modalButton("Done"),
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
                        inputId = "baseweight",
                        label = "Weight without Armor",
                        value = constraint.values$baseweight,
                        align = "right",
                        decimalCharacter = ".",
                        digitGroupSeparator = ",",
                        decimalPlaces = 1,
                        maximumValue = 999,
                        minimumValue = 0
                    ),
                    shinyWidgets::autonumericInput(
                        inputId = "endurance",
                        label = "Endurance Level",
                        value = constraint.values$endurance,
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


    minimum.values <- 
        shiny::reactiveValues(
            minphysdef = 0, 
            minstrikedef = 0, 
            minslashdef = 0, 
            minthrustdef = 0, 
            minmagdef = 0, 
            minfiredef = 0, 
            minlitngdef = 0, 
            minbleedres = 0, 
            minpoisres = 0, 
            mincurseres = 0, 
            minpoise = 0, 
            mindurability = 0
        )

    shiny::observeEvent(
        list(input$minphysdef, input$minstrikedef, input$minslashdef, input$minthrustdef, input$minmagdef, input$minfiredef, input$minlitngdef, input$minbleedres, input$minpoisres, input$mincurseres, input$minpoise, input$mindurability), 
    {
        if(shiny::isTruthy(input$minphysdef)){minimum.values$minphysdef <- input$minphysdef}
        if(shiny::isTruthy(input$minstrikedef)){minimum.values$minstrikedef <- input$minstrikedef}
        if(shiny::isTruthy(input$minslashdef)){minimum.values$minslashdef <- input$minslashdef}
        if(shiny::isTruthy(input$minthrustdef)){minimum.values$minthrustdef <- input$minthrustdef}
        if(shiny::isTruthy(input$minmagdef)){minimum.values$minmagdef <- input$minmagdef}
        if(shiny::isTruthy(input$minfiredef)){minimum.values$minfiredef <- input$minfiredef}
        if(shiny::isTruthy(input$minlitngdef)){minimum.values$minlitngdef <- input$minlitngdef}
        if(shiny::isTruthy(input$minbleedres)){minimum.values$minbleedres <- input$minbleedres}
        if(shiny::isTruthy(input$minpoisres)){minimum.values$minpoisres <- input$minpoisres}
        if(shiny::isTruthy(input$mincurseres)){minimum.values$mincurseres <- input$mincurseres}
        if(shiny::isTruthy(input$minpoise)){minimum.values$minpoise <- input$minpoise}
        if(shiny::isTruthy(input$mindurability)){minimum.values$mindurability <- input$mindurability} 
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$minima, { 
      shiny::showModal( 
        shiny::modalDialog( 
          title = "Minimum Inputs", 
          footer = shiny::modalButton("Done"),
          shiny::fluidRow(
            shiny::column(width = 3,
                   shinyWidgets::autonumericInput(
                     inputId = "minphysdef",
                     label = "PHYS_DEF",
                     value = minimum.values$minphysdef,
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
                     value = minimum.values$minmagdef,
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
                     value = minimum.values$minbleedres,
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
                     value = minimum.values$minstrikedef,
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
                     value = minimum.values$minfiredef,
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
                     value = minimum.values$minpoisres,
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
                     value = minimum.values$minslashdef,
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
                     value = minimum.values$minlitngdef,
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
                     value = minimum.values$mincurseres,
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
                     value = minimum.values$minthrustdef,
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
                     value = minimum.values$minpoise,
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
                     value = minimum.values$mindurability,
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


    weight.values <- 
        shiny::reactiveValues(
            physdefweight = 16, 
            strikedefweight = 16, 
            slashdefweight = 16, 
            thrustdefweight = 16, 
            magdefweight = 8, 
            firedefweight = 8, 
            litngdefweight = 8, 
            bleedresweight = 4, 
            poisresweight = 4, 
            curseresweight = 4
        )

    shiny::observeEvent(
        list(input$physdefweight, input$strikedefweight, input$slashdefweight, input$thrustdefweight, input$magdefweight, input$firedefweight, input$litngdefweight, input$bleedresweight, input$poisresweight, input$curseresweight), 
    {
        if(shiny::isTruthy(input$physdefweight)){weight.values$physdefweight <- input$physdefweight}
        if(shiny::isTruthy(input$strikedefweight)){weight.values$strikedefweight <- input$strikedefweight}
        if(shiny::isTruthy(input$slashdefweight)){weight.values$slashdefweight <- input$slashdefweight}
        if(shiny::isTruthy(input$thrustdefweight)){weight.values$thrustdefweight <- input$thrustdefweight}
        if(shiny::isTruthy(input$magdefweight)){weight.values$magdefweight <- input$magdefweight}
        if(shiny::isTruthy(input$firedefweight)){weight.values$firedefweight <- input$firedefweight}
        if(shiny::isTruthy(input$litngdefweight)){weight.values$litngdefweight <- input$litngdefweight}
        if(shiny::isTruthy(input$bleedresweight)){weight.values$bleedresweight <- input$bleedresweight}
        if(shiny::isTruthy(input$poisresweight)){weight.values$poisresweight <- input$poisresweight}
        if(shiny::isTruthy(input$curseresweight)){weight.values$curseresweight <- input$curseresweight}
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$weights, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Score Inputs", 
                footer = shiny::modalButton("Done"),
                shiny::fluidRow(
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "physdefweight",
                                label = "PHYS_DEF",
                                value = weight.values$physdefweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "magdefweight",
                                label = "MAG_DEF",
                                value = weight.values$magdefweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "bleedresweight",
                                label = "BLEED_RES",
                                value = weight.values$bleedresweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    ),
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "strikedefweight",
                                label = "STRIKE_DEF",
                                value = weight.values$strikedefweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "firedefweight",
                                label = "FIRE_DEF",
                                value = weight.values$firedefweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "poisresweight",
                                label = "POIS_RES",
                                value = weight.values$poisresweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    ),
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "slashdefweight",
                                label = "SLASH_DEF",
                                value = weight.values$slashdefweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "litngdefweight",
                                label = "LITNG_DEF",
                                value = weight.values$litngdefweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            ),
                            shinyWidgets::autonumericInput(
                                inputId = "curseresweight",
                                label = "CURSE_RES",
                                value = weight.values$curseresweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    ),
                    shiny::column(width = 3,
                            shinyWidgets::autonumericInput(
                                inputId = "thrustdefweight",
                                label = "THRUST_DEF",
                                value = weight.values$thrustdefweight,
                                align = "right",
                                currencySymbol = "%",
                                currencySymbolPlacement = "s",
                                decimalCharacter = ".",
                                digitGroupSeparator = ",",
                                decimalPlaces = 0,
                                maximumValue = 100,
                                minimumValue = 0
                            )
                    )
                )
            ) 
        ) 
    })


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
                        SCORE = numeric(0),
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
                        list(targets = c(1, 6:22), searchable = FALSE)
                    )
                )
            ) |> DT::formatPercentage(c("SCORE", "PCT_LOAD"), 2) |>
            DT::formatCurrency(c(
              "PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF",
                "MAG_DEF", "FIRE_DEF", "LITNG_DEF",
                "BLEED_RES", "POIS_RES", "CURSE_RES",
                "DURABILITY", "ARMOR_POISE", "TOTAL_POISE",
                "ARMOR_WEIGHT", "TOTAL_WEIGHT", "EQUIP_LOAD"
            ), currency = "", interval = 3, mark = ",", digits = 1)
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
      
            minima <-             
                c(
                    minimum.values$minphysdef, minimum.values$minstrikedef, minimum.values$minslashdef, minimum.values$minthrustdef,
                    minimum.values$minmagdef, minimum.values$minfiredef, minimum.values$minlitngdef, minimum.values$minpoise, 
                    minimum.values$minbleedres, minimum.values$minpoisres, minimum.values$mincurseres, minimum.values$mindurability
                )

            weights <- 
                c(
                    weight.values$physdefweight, weight.values$strikedefweight, weight.values$slashdefweight, weight.values$thrustdefweight, 
                    weight.values$magdefweight, weight.values$firedefweight, weight.values$litngdefweight, 
                    weight.values$bleedresweight, weight.values$poisresweight, weight.values$curseresweight
                )

            armordata(
                get.optimal.armor.combos(
                    max.table.size = filter.values$tablesize,
                    starting.class = filter.values$class,
                    areas.completed = filter.values$area,
                    upgrade.types = filter.values$upgradetype,
                    head.filter = filter.values$head,
                    chest.filter = filter.values$chest,
                    hands.filter = filter.values$hands,
                    legs.filter = filter.values$legs,
                    regular.level = upgrade.values$reglvl,
                    twinkling.level = upgrade.values$twinklvl,
                    roll = constraint.values$roll,
                    unarmored.weight = constraint.values$baseweight,
                    endurance.level = constraint.values$endurance,
                    havel.ring = constraint.values$havel,
                    fap.ring = constraint.values$fap,
                    wolf.ring = constraint.values$wolf,
                    minima = minima,
                    weights = weights
                )
            )
            gc()

            armordata()$data[, c("HEAD", "CHEST", "HANDS", "LEGS") := lapply(.SD, as.factor), .SDcols = c("HEAD", "CHEST", "HANDS", "LEGS")]

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
  
    # shiny::observeEvent(
    #     list(
    #         input$tablesize, 
    #         input$class,
    #         input$area,
    #         input$head,
    #         input$chest,
    #         input$hands,
    #         input$legs,
    #         input$upgradetype,
    #         input$reglvl,
    #         input$twinklvl,
    #         input$roll,
    #         input$baseweight,
    #         input$endurance,
    #         input$havel,
    #         input$fap,
    #         input$wolf,
    #         input$minpoise,
    #         input$physdefweight,
    #         input$strikedefweight,
    #         input$slashdefweight,
    #         input$thrustdefweight,
    #         input$magdefweight,
    #         input$firedefweight,
    #         input$litngdefweight,
    #         input$bleedresweight,
    #         input$poisresweight,
    #         input$curseresweight
    #     ), 
    # {
    #     output$refreshmessage <- shiny::renderText("Inputs have changed: click 'Refresh Armor Data' to see new results.")
    # }, ignoreInit = TRUE)


}
