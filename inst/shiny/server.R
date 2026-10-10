
server <- function(input, output, session){


    ## Largest Max Table Size the app allows. The widget enforces it in the browser, but a client can
    ## send any value (e.g. via Shiny.setInputValue), so the filter modal also clamps it server-side -
    ## an unbounded value can make one refresh hold gigabytes of results.
    MAX_TABLE_SIZE <- 100000

    output$mode_label <- shiny::renderText("Mode (Light or Dark)")

    shiny::observeEvent(input$guide, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "User Guide", 
                easyClose = TRUE,
                footer = NULL,
                size = "l",
                shiny::HTML(paste0("

                    To use the app, adjust inputs in the sidebar at left and then click 'Refresh Armor Data'. Both tabs, Results and Trade-offs, show the settings of the last refresh;
                    if you change a setting afterwards, a message says so until you refresh again. <br>
                    'Download Armor Data' saves an Excel workbook of the last refresh: the Results table, the Trade-offs table for the stat currently chosen in that tab, and the settings behind both. <br> <br>

                    Filter Inputs: <br>  
                    'Max Table Size' is used to specify how large the created table can be and is used to avoid unnecessary slowness - it cannot be set higher than 100,000. <br> 
                    'Starting Class' is used to specify the character's starting class. This indicates that the associated armor set is available. <br>  
                    'Areas Completed' is used to specify which areas have been completed. This loosely indicates which armor pieces are available. 
                    Please note: the tool will not infer that one area has been completed because another later area has been completed.
                    For example, indicating that Anor Londo has been completed will not result in Sens Fortress being assumed complete. <br>  
                    'Upgrades With' is used to specify which upgrade materials to consider. For example, only armor pieces that upgrade with twinkling titanite could be considered. <br>  
                    'Head' is used to specify which head armor pieces can be included in the created table - 'Chest', 'Hands' and 'Legs' are used similarly. <br> 
                    'Poise Armor Only' filters the current selections in 'Head', 'Chest', 'Hands', and 'Legs' down to only those entries with positive Poise. 
                    Each equipped armor piece with positive Poise reduces the Poise cooldown timer. <br> 
                    'Efficient Armor Only' filters the current selections in 'Head', 'Chest', 'Hands', and 'Legs' down to only those entries which do not penalize Stamina recovery. <br>
                    'Quiet Armor Only' filters the current selections in 'Chest' down to only those entries which are quiet. This helps with stealth, 
                    since enemies detect the player via sound, as explained here: ", 
                    shiny::tags$a("How Enemies Detect You in Dark Souls: Part 1 - Sound", href = "https://www.youtube.com/watch?v=mdL75pAvt8I", target = "_blank"), " <br> <br>

                    Upgrade Inputs: <br>  
                    'Armor Level (Regular)' is used to specify the upgrade level of armor pieces ascended via regular titanite. Options are +0 thru +10. <br>  
                    'Armor Level (Twinkling)' is used to specify the upgrade level of armor pieces ascended via twinkling titanite. Options are +0 thru +5. <br>  
                    Every upgrade level is computed from each piece's +0 values with the game's own upgrade rates, exactly as the game computes them.
                    The app shows values rounded to one decimal; the game rounds an occasional exact tie (such as 9.25) the other way, so a value can differ from the game's own display by 0.1.
                    The download holds the exact values. <br> <br>

                    Ring Inputs: <br>  
                    'Havel's Ring', 'Ring of Favor', and 'Wolf Ring' are used to specify whether the player has the relevant ring equipped.
                    The app will allow all three to be selected, but this is obviously not possible in game. <br>  
                    Havel's Ring and the Ring of Favor both boost equip load which is helpful when trying to quicken movement.
                    The Wolf Ring gives 40 poise and is immensely helpful in hitting key poise breakpoints. These are 21/46/61 for PVE and 31/61 for PVP, as explained here: ", 
                    shiny::tags$a("Dark Souls Dissected #13 - Poise Mechanics (and glitches!)", href = "https://www.youtube.com/watch?v=pwffSOSzcAM", target = "_blank"), " . <br> <br>

                    Load Inputs: <br>  
                    'Movement' is used to specify the heaviest movement type the character may have: Light (armor and other equipment at or below 25% of equip load, for a light roll),
                    Mid (at or below 50%, mid roll), Fat (at or below 100%, fat roll), or Poop (no limit: over 100%, the character can't roll and walks slowly). <br>
                    'Endurance Level' is used to specify the character's current level in the Endurance stat - Endurance affects equip load. <br>
                    'Right Hand Weapon 1' through 'Left Hand Weapon 2' are the weapons, shields, catalysts, talismans and bows in the four weapon slots. Only their weight is used, and rings and ammunition weigh nothing,
                    so together with your armor they are everything you carry. They're chosen per slot because the game adds up equipment weight one item at a time, in slot order, with limited precision.
                    A set weighing exactly 25.0% on paper can still give a mid roll, and with two or more talismans equipped, which slot holds what can make the difference. The app checks movement exactly as the game does. <br> <br>

                    Minimum Inputs: <br>  
                    Here, minima may be specified for a set of relevant metrics. Only combinations which achieve or exceed the specified minima will be considered. <br> <br>

                    Score Inputs: <br>
                    Here, weights may be specified for a set of relevant metrics. A score for each armor combination is calculated as
                    (w_1*x_1+...+w_n*x_n)/(w_1+...+w_n), where each x_i is the standardized value of the relevant metric (standardized means that all metrics have been shifted and scaled to mean 0 and variance 1). 
                    This overall score is then transformed so that it also has mean 0 and variance 1. This value is presented as 'SCORE_RAW'. 'SCORE_QUALITY' describes how rare that score is among every possible armor combination at the selected upgrade levels, such as 'Top 1 in 40' or 'Bottom 1 in 40'.
                    It is an exact count, not an estimate: 'Top 1 in 40' means 1 in every 40 combinations at those upgrade levels scores at least as well (ties included), regardless of the filters chosen. Combinations in the bottom half read 'Bottom 1 in N' instead, counting those that score at most as well.
                    SCORE_RAW is global within the same set of weights: direct comparisons can be made across different inputs, including different upgrade levels. SCORE_QUALITY is relative to the selected upgrade levels. <br> <br>

                    Trade-offs Tab: <br>
                    For every armor weight, from 0 up to the heaviest possible armor, every 0.1, the chart shows the most of the chosen stat ('Maximize': the score, Poise, or a single defense or resistance)
                    that any armor set can reach without weighing more. It uses the settings of the last refresh (filters, upgrade levels, rings, load, and minima).
                    Among sets tied on the chosen stat, the best-scoring one is shown. This shows what each extra unit of armor weight buys; for example, the lightest armor that reaches a Poise breakpoint
                    (drawn as dotted red lines when charting Poise). <br>
                    Vertical lines mark the armor weights at which your movement type changes (Light | Mid, Mid | Fat, Fat | Poop), given your equip load and weapons.
                    The line for the movement type chosen in 'Load Inputs' is solid, and each set's movement type is shown with it. Each set's movement type is checked exactly as the game checks it,
                    including the Mask of the Father's equip load bonus, so just past a line a set wearing the Mask can still have the lighter movement type. <br>
                    A minimum on the charted stat itself is ignored, since it would only cut the curve off below it. It's drawn as a dashed blue line instead. Every other minimum still applies. <br>
                    The chart is computed when the tab is opened after a refresh, which takes a few seconds. Hover anywhere above or below a point to see its set and movement type.
                    Click there, or click a row of the table below the chart, for the pieces' links. <br>
                    The shaded bands behind the curve split it into regions, each colored by how much of the stat it buys per unit of armor weight compared with the curve's average
                    (its total gain divided by its total weight). Green regions buy more, deepest at twice the average or more; purple regions buy less, deepest where the curve is flat;
                    grey ones are about average. Hover over a point for its region's rate; its hover box takes the region's color. <br> <br>

                    Miscellaneous notes: <br> <br>
                    Some armor pieces reduce stamina regeneration speed, as does being above 50% load or 100% load. Information on this can be found here: ",
                    shiny::tags$a("Stamina", href = "http://darksouls.wikidot.com/stamina#toc3 ", target = "_blank"), " <br> <br>
                    Durability is aggregated by taking the minimum i.e. the total durability for a set is the lowest durability across each component of the set. <br> <br> 
                    The impact to equip load of Mask of the Father (x1.05) is accounted for, but the impact to magic defense of Crown of Dusk (x0.7) is not. <br> <br>
                    Clicking on a row in the Results table, or on a point or row in the Trade-offs tab, will produce a set of links to the Dark Souls Wikidot site for the relevant armor pieces. <br> <br>",
                    "If maximizing Physical Defenses, Elemental Defenses, or Resistances, the following non-armor items are useful: <br>",
                    shiny::tags$a("Ring of Steel Protection", href = "http://darksouls.wikidot.com/ring-of-steel-protection", target = "_blank"), " (+50 to all Physical Defenses) <br>",
                    shiny::tags$a("Spell Stoneplate Ring", href = "http://darksouls.wikidot.com/spell-stoneplate-ring", target = "_blank"), " (+50 Magic Defense) <br>",
                    shiny::tags$a("Flame Stoneplate Ring", href = "http://darksouls.wikidot.com/flame-stoneplate-ring", target = "_blank"), " (+50 Fire Defense) <br>",
                    shiny::tags$a("Thunder Stoneplate Ring", href = "http://darksouls.wikidot.com/thunder-stoneplate-ring", target = "_blank"), " (+50 Lightning Defense) <br>",
                    shiny::tags$a("Speckled Stoneplate Ring", href = "http://darksouls.wikidot.com/speckled-stoneplate-ring", target = "_blank"), " (+25 to all Elemental Defenses)  <br>",
                    shiny::tags$a("Poisonbite Ring", href = "http://darksouls.wikidot.com/poisonbite-ring", target = "_blank"), " (x4 Unarmored Poison Resistance) <br>",
                    shiny::tags$a("Bloodbite Ring", href = "http://darksouls.wikidot.com/bloodbite-ring", target = "_blank"), " (x4 Unarmored Bleed Resistance) <br>",
                    shiny::tags$a("Cursebite Ring", href = "http://darksouls.wikidot.com/cursebite-ring", target = "_blank"), " (x4 Unarmored Curse Resistance) <br>",
                    shiny::tags$a("Gargoyle's Halberd", href = "http://darksouls.wikidot.com/gargoyle-halberd/", target = "_blank"), " (x1.25 Unarm Pois/Bleed Res) <br>",
                    shiny::tags$a("Gargoyle Tail Axe", href = "http://darksouls.wikidot.com/gargoyle-tail-axe", target = "_blank"), " (x2 Unarm Pois/Bleed Res) <br>",
                    shiny::tags$a("Bloodshield", href = "http://darksouls.wikidot.com/bloodshield", target = "_blank"), " (x1.5 Unarm Pois/Bleed/Curse Res) <br>",
                    shiny::tags$a("Humanity", href = "http://darksouls.wikidot.com/humanity", target = "_blank"), " (Boosts all Defenses and Curse Res, see Wiki) <br> <br>",
                    "
                    The improvements to magic defense of the Spell Stoneplate Ring and Speckled Stoneplate Ring apply after the reduction of Crown of Dusk. <br> <br>
                    In general, Toxic Resistance is the same as Poison Resistance, with the exception of the Poisonbite Ring: it does not improve Toxic Resistance. <br> <br>
                    To explain Unarmored Resistance: if base Bleed Resistance is X, Armor Bleed Resistance is Y, and the Bloodbite Ring and Gargoyle Tail Axe are equipped, then final Bleed Resistance is X*4*2+Y. <br> <br>
                    Links to the Wikidot site have been included in various parts of this app. 
                    However, over the course of this app's creation, I have found multiple inaccuracies there (no disrespect intended towards all the great folks who have taken the time and energy to contribute).
                    Do not be surprised if it contradicts the information here. <br> <br>

                    This app is also available as an R package - here is the link to the codebase on GitHub: ",
                    shiny::tags$a("https://github.com/ricewhitlam/darksoulsarmor", href = "https://github.com/ricewhitlam/darksoulsarmor", target = "_blank"),
                    ". To install the R package, run the following command in the R terminal: devtools::install_github(", '"', "ricewhitlam/darksoulsarmor", '"', ")

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
                output$refreshmessage <- shiny::renderText("Inputs have changed - click 'Refresh Armor Data' to update both tabs")
            }
        } else{
            output$refreshmessage <- shiny::renderText("Adjust settings in the sidebar and click 'Refresh Armor Data' to pull results for both tabs")
        }
    })


    filter.values <- 
        shiny::reactiveValues(
            max.table.size = 1000,
            starting.class = classes[1], 
            areas.completed = areas, 
            upgrade.types = c("Regular", "Twinkling", "None"),
            head.filter = head.data.unupgraded$ARMOR,
            chest.filter = chest.data.unupgraded$ARMOR,
            hands.filter = hands.data.unupgraded$ARMOR,
            legs.filter = legs.data.unupgraded$ARMOR
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
                        maximumValue = MAX_TABLE_SIZE,
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
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 19),
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
                        choices = head.data.unupgraded$ARMOR,
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
                        choices = chest.data.unupgraded$ARMOR,
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
                        choices = hands.data.unupgraded$ARMOR,
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
                        choices = legs.data.unupgraded$ARMOR,
                        selected = filter.values$legs.filter,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10),
                        choicesOpt = NULL,
                        width = NULL,
                        inline = FALSE,
                        stateInput = TRUE,
                        autocomplete = FALSE
                    ),
                    shiny::column(6, shiny::p(),
                        shiny::actionButton(inputId = "poise_only", label = "Poise Armor Only"), shiny::p(), 
                        shiny::actionButton(inputId = "stam_only", label = "Efficient Armor Only"), shiny::p(), 
                        shiny::actionButton(inputId = "quiet_only", label = "Quiet Armor Only"), shiny::p()
                    )
                )
            ) 
        ) 
    })

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

    shiny::observeEvent(input$dismiss_filter_modal, {

        if(shiny::isTruthy(input$max.table.size)){filter.values$max.table.size <- min(MAX_TABLE_SIZE, max(1, round(input$max.table.size, 0)))}
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
            favor.ring = FALSE,
            wolf.ring = FALSE
        )
    
    shiny::observeEvent(input$rings, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Ring Inputs", 
                footer = shiny::actionButton(inputId = "dismiss_ring_modal", label = "Done"), 
                shiny::fluidRow(
                    bslib::input_switch(id = "havel.ring", label = "Havel's Ring (+50% Eq Load)", value = ring.values$havel.ring, width = NULL),
                    bslib::input_switch(id = "favor.ring", label = "Ring of Favor (+20% Eq Load)", value = ring.values$favor.ring, width = NULL),
                    bslib::input_switch(id = "wolf.ring", label = "Wolf Ring (+40 Poise)", value = ring.values$wolf.ring, width = NULL)
                )
            ) 
        ) 
    })

    shiny::observeEvent(input$dismiss_ring_modal, {

        if(shiny::isTruthy(input$havel.ring)){ring.values$havel.ring <- input$havel.ring} else if(length(input$havel.ring) == 1){if(!is.na(input$havel.ring) && is.logical(input$havel.ring)){ring.values$havel.ring <- input$havel.ring}}
        if(shiny::isTruthy(input$favor.ring)){ring.values$favor.ring <- input$favor.ring} else if(length(input$favor.ring) == 1){if(!is.na(input$favor.ring) && is.logical(input$favor.ring)){ring.values$favor.ring <- input$favor.ring}}
        if(shiny::isTruthy(input$wolf.ring)){ring.values$wolf.ring <- input$wolf.ring} else if(length(input$wolf.ring) == 1){if(!is.na(input$wolf.ring) && is.logical(input$wolf.ring)){ring.values$wolf.ring <- input$wolf.ring}}

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$ring.values <- all(sapply(names(ring.values), function(name){identical(curr.vals[[name]], ring.values[[name]])}))
        }
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    constraint.values <- 
        shiny::reactiveValues(
            movement = "Light",
            weapons = c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None"),
            endurance.level = 10
        )

    ## One dropdown per weapon slot, each weapon's weight beside its name (in the list and once chosen)
    weapon.input <- function(slot, label){
        shinyWidgets::pickerInput(
            inputId = paste0("weapon.", slot),
            label = label,
            choices = c("None", weapon.data$WEAPON),
            selected = constraint.values$weapons[[slot]],
            multiple = FALSE,
            options = list(`live-search` = TRUE, size = 10, `show-subtext` = TRUE),
            choicesOpt = list(subtext = c("", sprintf("%.1f", weapon.data$WEIGHT))),
            width = NULL,
            inline = FALSE,
            stateInput = TRUE,
            autocomplete = FALSE
        )
    }
    
    shiny::observeEvent(input$constraints, { 
        shiny::showModal( 
            shiny::modalDialog( 
                title = "Load Inputs", 
                footer = shiny::actionButton(inputId = "dismiss_constraint_modal", label = "Done"), 
                shiny::fluidRow(
                    shiny::radioButtons(
                        inputId = "movement",
                        label = "Movement",
                        choices = c("Light", "Mid", "Fat", "Poop"),
                        selected = constraint.values$movement,
                        inline = TRUE,
                        width = NULL,
                        choiceNames = NULL,
                        choiceValues = NULL
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
                    ),
                    weapon.input("right.1", "Right Hand Weapon 1"),
                    weapon.input("right.2", "Right Hand Weapon 2"),
                    weapon.input("left.1", "Left Hand Weapon 1"),
                    weapon.input("left.2", "Left Hand Weapon 2"),
                    shiny::helpText(shiny::textOutput(outputId = "weapon_summary", inline = TRUE))
                )
            )
        )
    })

    ## Under the weapon dropdowns, as they're changed: the weapons' total weight
    output$weapon_summary <- shiny::renderText({
        weapons <- constraint.values$weapons
        for(slot in names(weapons)){
            chosen <- input[[paste0("weapon.", slot)]]
            if(shiny::isTruthy(chosen)){
                weapons[[slot]] <- chosen
            }
        }
        sprintf("Weapons: %.1f in total", weapons.weight(weapons))
    })

    shiny::observeEvent(input$dismiss_constraint_modal, {

        if(shiny::isTruthy(input$movement)){constraint.values$movement <- input$movement}
        if(shiny::isTruthy(input$weapon.left.1) && shiny::isTruthy(input$weapon.right.1) && shiny::isTruthy(input$weapon.left.2) && shiny::isTruthy(input$weapon.right.2)){
            constraint.values$weapons <- c(left.1 = input$weapon.left.1, right.1 = input$weapon.right.1, left.2 = input$weapon.left.2, right.2 = input$weapon.right.2)
        }
        if(shiny::isTruthy(input$endurance.level)){constraint.values$endurance.level <- round(input$endurance.level, 0)}

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$constraint.values <- all(sapply(names(constraint.values), function(name){identical(curr.vals[[name]], constraint.values[[name]])}))
        }
        shiny::removeModal()
        
    }, ignoreInit = TRUE)


    ## The minima/weights modal inputs below are built from darksoulsarmor:::METRICS (see
    ## R/data.R) instead of one hand-typed autonumericInput() block per metric. Without this,
    ## each metric's UI input id, label, and position in the minima/weights vector would have
    ## to be hand-typed in sync across four places (widget creation, the modal-dismiss
    ## reassembly, and - for weights - the normalize-weights update) - the same kind of
    ## parallel hand-maintained ordering that caused the minima-indexing bug fixed earlier in
    ## this package's history. metric.input.id() reproduces the existing widget ids exactly
    ## (e.g. "PHYS_DEF" -> "minphysdef"/"physdefweight") so this is a pure refactor.
    ## The game's equip-load rule (R/equip-load.R) gives each Trade-offs point its movement type.
    METRICS <- darksoulsarmor:::METRICS
    MOVEMENT.SHARES <- darksoulsarmor:::MOVEMENT.SHARES
    weapons.weight <- darksoulsarmor:::weapons.weight
    equip.load <- darksoulsarmor:::equip.load
    movement.line <- darksoulsarmor:::movement.line
    total.weight <- darksoulsarmor:::total.weight
    movement.type <- darksoulsarmor:::movement.type

    metric.input.id <- function(stat, kind){
        base <- tolower(gsub("_", "", stat))
        if(kind == "minima") paste0("min", base) else paste0(base, "weight")
    }
    minima.index.of <- function(stat){ METRICS[metric == stat, minima.index] }
    weight.index.of <- function(stat){ METRICS[metric == stat, weight.index] }

    ## Canonical order matches minima.index/weight.index, i.e. exactly the order
    ## get.optimal.armor.combos expects its minima/weights arguments in.
    minima.metrics <- METRICS[order(minima.index), metric]
    ## unname() below: vapply() over a character vector auto-names its result using the input
    ## values (X itself, per USE.NAMES) - harmless for R-side use, but Shiny serializes a named
    ## vector differently (jsonlite's keep_vec_names), which mangles the inputId sent to the
    ## client, so these must stay plain unnamed strings.
    minima.ids <- unname(vapply(minima.metrics, metric.input.id, character(1), kind = "minima"))
    weight.metrics <- METRICS[!is.na(weight.index)][order(weight.index), metric]
    weight.ids <- unname(vapply(weight.metrics, metric.input.id, character(1), kind = "weight"))

    minimum.values <- shiny::reactiveValues(minima = rep(0.0, 12))

    minimum.input <- function(stat){
        shinyWidgets::autonumericInput(
            inputId = metric.input.id(stat, "minima"),
            label = stat,
            value = minimum.values$minima[minima.index.of(stat)],
            align = "right",
            decimalCharacter = ".",
            digitGroupSeparator = ",",
            decimalPlaces = ifelse(stat %in% c("POISE", "DURABILITY"), 0, 1),
            maximumValue = 999,
            minimumValue = 0
        )
    }

    shiny::observeEvent(input$minima, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Minimum Inputs",
          footer = shiny::actionButton(inputId = "dismiss_minimum_modal", label = "Done"),
          shiny::fluidRow(
            shiny::column(width = 3, minimum.input("PHYS_DEF"), minimum.input("MAG_DEF"), minimum.input("BLEED_RES")),
            shiny::column(width = 3, minimum.input("STRIKE_DEF"), minimum.input("FIRE_DEF"), minimum.input("POIS_RES")),
            shiny::column(width = 3, minimum.input("SLASH_DEF"), minimum.input("LITNG_DEF"), minimum.input("CURSE_RES")),
            shiny::column(width = 3, minimum.input("THRUST_DEF"), minimum.input("POISE"), minimum.input("DURABILITY"))
          )
        )
      )
    })

    shiny::observeEvent(input$dismiss_minimum_modal, {

        minima.inputs <- lapply(minima.ids, function(id){ input[[id]] })
        if(all(vapply(minima.inputs, shiny::isTruthy, logical(1)))){
            minimum.values$minima <- round(unlist(minima.inputs), 1)
        }

        if(been.refreshed()){
            curr.vals <- armordata()$args
            inputs.unchanged$minimum.values <- all(sapply(names(minimum.values), function(name){identical(curr.vals[[name]], minimum.values[[name]])}))
        }
        shiny::removeModal()

    }, ignoreInit = TRUE)


    weight.values <- shiny::reactiveValues(weights = c(16.0, 16.0, 16.0, 16.0, 8.0, 8.0, 8.0, 4.0, 4.0, 4.0))

    weight.input <- function(stat){
        shinyWidgets::autonumericInput(
            inputId = metric.input.id(stat, "weight"),
            label = stat,
            value = weight.values$weights[weight.index.of(stat)],
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

    shiny::observeEvent(input$weights, {
        shiny::showModal(
            shiny::modalDialog(
                title = "Score Inputs",
                footer = shiny::fluidRow(shiny::column(8, shiny::actionButton(inputId = "normalize_weights", label = "Normalize to 100%")), shiny::column(4, shiny::actionButton(inputId = "dismiss_weight_modal", label = "Done"))),
                shiny::fluidRow(
                    shiny::column(width = 3, weight.input("PHYS_DEF"), weight.input("MAG_DEF"), weight.input("BLEED_RES")),
                    shiny::column(width = 3, weight.input("STRIKE_DEF"), weight.input("FIRE_DEF"), weight.input("POIS_RES")),
                    shiny::column(width = 3, weight.input("SLASH_DEF"), weight.input("LITNG_DEF"), weight.input("CURSE_RES")),
                    shiny::column(width = 3, weight.input("THRUST_DEF"))
                )
            )
        )
    })

    shiny::observeEvent(input$normalize_weights, {

        weight.inputs <- lapply(weight.ids, function(id){ input[[id]] })
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
            for(i in seq_along(weight.ids)){
                shinyWidgets::updateAutonumericInput(inputId = weight.ids[i], value = weights[i])
            }

        }

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$dismiss_weight_modal, {

        weight.inputs <- lapply(weight.ids, function(id){ input[[id]] })
        if(all(vapply(weight.inputs, shiny::isTruthy, logical(1)))){
            weight.values$weights <- unlist(weight.inputs)
        }

        if(been.refreshed()){
            ## Custom logic here due to special nature of weights argument. isTRUE(all(...)) keeps
            ## this a single TRUE/FALSE like every other inputs.unchanged entry - all-zero weights
            ## normalize to 0/0 = NaN, and an NA here would reach the if(all(...)) in the
            ## refresh-message observer above, erroring inside an observer and ending the session.
            inputs.unchanged$weight.values <- isTRUE(all(abs(weight.values$weights/sum(weight.values$weights)-armordata()$args$weights) < 1e-10))
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
                        weights = c(0.16, 0.16, 0.16, 0.16, 0.08, 0.08, 0.08, 0.04, 0.04, 0.04)
                    ),
                data = 
                    data.table::data.table(
                        SCORE_RAW = numeric(0),
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
            three.decimals <- c("SCORE_RAW", "POISE_TIMER")
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
            starting.class = filter.values$starting.class,
            areas.completed = filter.values$areas.completed,
            upgrade.types = filter.values$upgrade.types,
            head.filter = filter.values$head.filter,
            chest.filter = filter.values$chest.filter,
            hands.filter = filter.values$hands.filter,
            legs.filter = filter.values$legs.filter,
            regular.level = upgrade.values$regular.level,
            twinkling.level = upgrade.values$twinkling.level,
            movement = constraint.values$movement,
            weapons = constraint.values$weapons,
            endurance.level = constraint.values$endurance.level,
            havel.ring = ring.values$havel.ring,
            favor.ring = ring.values$favor.ring,
            wolf.ring = ring.values$wolf.ring,
            minima = minimum.values$minima,
            weights = weight.values$weights
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
                result <- do.call(get.optimal.armor.combos, c(list(max.table.size = filter.values$max.table.size), current.settings()))
                result$data[, c("HEAD", "CHEST", "HANDS", "LEGS") := lapply(.SD, as.factor), .SDcols = c("HEAD", "CHEST", "HANDS", "LEGS")]
                result
            })
            gc()

            been.refreshed(TRUE)

            inputs.unchanged$filter.values <- TRUE
            inputs.unchanged$upgrade.values <- TRUE
            inputs.unchanged$ring.values <- TRUE
            inputs.unchanged$constraint.values <- TRUE
            inputs.unchanged$minimum.values <- TRUE
            inputs.unchanged$weight.values <- TRUE

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


    ## Trade-offs tab: the most of one stat any armor set can reach at each armor weight (see
    ## get.armor.tradeoffs), for the last refresh's settings, over every armor weight from 0 to
    ## the heaviest possible armor. The curve is computed in one segment per movement class - up to the
    ## Light line as Light movement, from there to the Mid line as Mid, and so on, then Poop past the
    ## Fat line - so every set in a segment moves at least as lightly as that segment's type, by the
    ## game's own check (which credits the Mask of the Father's equip load bonus).
    tradeoff.metric.labels <- c(
        SCORE = "Score", POISE = "Poise",
        PHYS_DEF = "Physical Defense", STRIKE_DEF = "Strike Defense", SLASH_DEF = "Slash Defense", THRUST_DEF = "Thrust Defense",
        MAG_DEF = "Magic Defense", FIRE_DEF = "Fire Defense", LITNG_DEF = "Lightning Defense",
        BLEED_RES = "Bleed Resistance", POIS_RES = "Poison Resistance", CURSE_RES = "Curse Resistance"
    )
    tradeoffdata <- shiny::reactiveVal(NULL)

    ## The color jumps are drawn in on the Trade-offs chart: a dark blue, set apart from the curve's
    ## lighter one by depth and weight
    JUMP.COLOR <- "rgb(8,48,107)"
    CURVE.COLOR <- "rgb(31,119,180)"

    ## A color for each efficiency ratio (a region's slope as a multiple of the curve's average): grey
    ## at the average, deepening to green at twice it or more and to purple at flat (0) - the ends of
    ## ColorBrewer's purple-green scale, which stay distinguishable with the common forms of color
    ## blindness. Measured on the ratio's log, so half and double the average are equally far from
    ## grey; no ratio (a flat curve) is grey. A tint below 1 mixes the color with white: the chart's
    ## bands and hover boxes use 0.35, light enough to read black text on.
    efficiency.color <- function(ratio, tint = 1){
        t <- ifelse(is.na(ratio), 0, pmax(-1, pmin(1, log2(ratio))))
        s <- abs(t)
        red <- 255+(189+(ifelse(t > 0, 27, 118)-189)*s-255)*tint
        green <- 255+(189+(ifelse(t > 0, 120, 42)-189)*s-255)*tint
        blue <- 255+(189+(ifelse(t > 0, 55, 131)-189)*s-255)*tint
        sprintf("rgb(%d,%d,%d)", round(red), round(green), round(blue))
    }

    ## The movement type each set (by its pieces' names) gets with the given weapons and equip loads,
    ## by the game's check, and whether only the Mask of the Father's bonus keeps it that light
    movement.class <- function(head, chest, hands, legs, carried, load, load.father.mask){
        weight.of <- function(table, pieces){ table$WEIGHT[match(pieces, table$ARMOR)] }
        total <- total.weight(carried, weight.of(head.data.unupgraded, head), weight.of(chest.data.unupgraded, chest), weight.of(hands.data.unupgraded, hands), weight.of(legs.data.unupgraded, legs))
        mask <- !is.na(head) & head == "Mask of the Father"
        with.bonus <- movement.type(total, ifelse(mask, load.father.mask, load))
        list(class = with.bonus, by.mask.bonus = mask & with.bonus != movement.type(total, load))
    }

    ## How many times the curve has been computed - lets the tests tell a recompute from a reuse
    tradeoff.computations <- shiny::reactiveVal(0)

    ## What a curve depends on: a refresh's settings except the table size and the movement type (the
    ## curve covers every movement class), and the Maximize choice
    tradeoff.key <- function(snapshot, metric){
        list(settings = snapshot[setdiff(names(snapshot), c("max.table.size", "movement"))], metric = metric)
    }

    ## The curve of `metric`, every 0.1 armor weight (the precision weights are shown at), for a
    ## refresh's settings (snapshot), as tradeoffdata() holds it - for the tab and for the download
    compute.tradeoffs <- function(snapshot, metric){

        ## The last refresh's (validated) settings, less the Results-only table size
        settings <- snapshot[setdiff(names(snapshot), "max.table.size")]
        ## A minimum on the charted stat itself would only cut the curve off below it, so it's
        ## ignored here and drawn as a reference line instead; every other minimum still applies
        stat.minimum <- 0
        if(metric != "SCORE"){
            stat.minimum <- settings$minima[minima.index.of(metric)]
            settings$minima[minima.index.of(metric)] <- 0
        }
        carried <- weapons.weight(settings$weapons)
        load <- equip.load(settings$endurance.level, settings$havel.ring, settings$favor.ring, father.mask = FALSE)
        load.father.mask <- equip.load(settings$endurance.level, settings$havel.ring, settings$favor.ring, father.mask = TRUE)
        heaviest.armor <- max(head.data.unupgraded$WEIGHT)+max(chest.data.unupgraded$WEIGHT)+max(hands.data.unupgraded$WEIGHT)+max(legs.data.unupgraded$WEIGHT)
        ## Armor weight at which each movement class ends (Light/Mid, Mid/Fat, Fat/Poop): its line less
        ## what the weapons weigh. The chart draws these; the segments end on the 0.1 grid armor
        ## weights are listed on, and the game's own check decides the sets right at a line.
        lines <- sapply(names(MOVEMENT.SHARES), function(movement){ movement.line(load, movement)-carried })
        step <- 0.1

        ## One segment per movement class, each from the previous line (exclusive) to its own
        ## (inclusive), clipped to [0, heaviest armor]; past the Fat line, no load limit
        segment.movements <- c(names(MOVEMENT.SHARES), "Poop")
        segment.ends <- c(floor(10*lines+1e-6)/10, Inf)
        segments <- list()
        covered.to <- -Inf
        for(s in seq_along(segment.movements)){
            lower <- max(0, covered.to)
            upper <- min(segment.ends[s], heaviest.armor)
            if(upper >= lower){
                segment.settings <- settings
                segment.settings$movement <- segment.movements[s]
                curve <- do.call(get.armor.tradeoffs, c(list(metric = metric, weight.step = step, min.armor.weight = lower, max.armor.weight = upper), segment.settings))$data
                ## A limit on the previous line belongs to the faster class
                curve <- curve[ARMOR_WEIGHT_LIMIT > covered.to+1e-9]
                if(nrow(curve) > 0){
                    segments[[length(segments)+1]] <- curve[, MOVEMENT_LIMIT := segment.movements[s]]
                }
            }
            covered.to <- max(covered.to, upper)
        }
        curve <- data.table::rbindlist(segments)

        classes <- movement.class(curve$HEAD, curve$CHEST, curve$HANDS, curve$LEGS, carried, load, load.father.mask)
        curve[, MOVEMENT := ifelse(is.na(ARMOR_WEIGHT), NA_character_, ifelse(classes$by.mask.bonus, paste(classes$class, "(Mask of the Father bonus)"), classes$class))]

        tradeoff.computations(tradeoff.computations()+1)
        list(
            metric = metric, data = curve, lines = lines, selected.movement = settings$movement,
            carried = carried, equip.load = load, stat.minimum = stat.minimum,
            key = tradeoff.key(snapshot, metric)
        )
    }

    ## Where extra armor weight pays off on the chart's curve (get.tradeoff.efficiency): its jumps,
    ## lines, flats and boundary points, with as many jumps and flats as the tab's choices ask for
    ## ("some" if a client sends anything else) - when it has two points to join. Recomputed when only
    ## those choices change, without searching again.
    tradeoff.efficiency <- shiny::reactive({
        result <- tradeoffdata()
        shiny::req(result)
        level <- function(value){ if(length(value) == 1 && value %in% c("few", "some", "many")) value else "some" }
        if(sum(!is.na(result$data$BEST_VALUE)) < 2){
            return(NULL)
        }
        get.tradeoff.efficiency(result$data, jumps = level(input$tradeoff_jumps), flats = level(input$tradeoff_flats))
    })

    ## The chart shows the settings of the last successful refresh (armordata()$args), exactly like
    ## the Results table - never unsaved or newer sidebar settings - so both tabs always describe
    ## the same character. It's computed while its tab is open, whenever something it depends on has
    ## changed: a refresh with different settings, or another Maximize choice. Neither the
    ## table size nor the movement type changes the curve (it covers every movement class), so a refresh that
    ## changes only those keeps it and just moves the emphasized movement line.
    shiny::observeEvent(list(input$main_tabs, armordata(), input$tradeoff_metric), {
        shiny::req(identical(input$main_tabs, "Trade-offs"), been.refreshed(), input$tradeoff_metric)
        snapshot <- armordata()$args
        current <- tradeoffdata()
        if(!is.null(current) && identical(current$key, tradeoff.key(snapshot, input$tradeoff_metric))){
            if(!identical(current$selected.movement, snapshot$movement)){
                current$selected.movement <- snapshot$movement
                tradeoffdata(current)
            }
            return(invisible(NULL))
        }

        ## Same error/warning handling as "Refresh Armor Data" above
        tradeoff.warnings <- character(0)

        tryCatch(

        withCallingHandlers({

            shinybusy::show_modal_spinner()

            tradeoffdata(compute.tradeoffs(snapshot, input$tradeoff_metric))
            output$errormessage <- shiny::renderText("")

            for(message in unique(tradeoff.warnings)){
                shiny::showNotification(message, type = "warning")
            }

        },

        warning = function(w){
            tradeoff.warnings <<- c(tradeoff.warnings, conditionMessage(w))
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

    output$tradeoff_plot <- plotly::renderPlotly({
        result <- tradeoffdata()
        shiny::req(result)
        d <- data.table::copy(result$data)
        metric.label <- tradeoff.metric.labels[[result$metric]]
        ## The charted stat at its display precision: the score to 3 decimals, poise whole, a
        ## defense or resistance to 1 decimal
        value.digits <- if(result$metric == "SCORE") 3 else if(result$metric == "POISE") 0 else 1
        d$hover <-
            ifelse(
                is.na(d$BEST_VALUE),
                sprintf("Armor weight up to %.1f<br>No armor set fits the other settings", d$ARMOR_WEIGHT_LIMIT),
                sprintf(
                    "Armor weight up to %.1f<br>%s: %s<br>%s<br>%s<br>%s<br>%s<br>Weighs %.1f - Movement: %s<br>Score %.3f (%s)",
                    d$ARMOR_WEIGHT_LIMIT, metric.label, formatC(round(d$BEST_VALUE, value.digits), format = "f", digits = value.digits),
                    ## The Mask of the Father abbreviated, to keep the hover box narrow
                    d$HEAD, d$CHEST, d$HANDS, d$LEGS, d$ARMOR_WEIGHT, sub("Mask of the Father bonus", "MotF bonus", d$MOVEMENT, fixed = TRUE), d$SCORE_RAW, d$SCORE_QUALITY
                )
            )
        d$point <- seq_len(nrow(d))

        shapes <- list()
        annotations <- list()
        ## The value axis: the curve, and the poise breakpoints and minimum drawn across it, with a
        ## margin - set rather than left to plotly, so that the flats' hatching can fill it top to bottom
        across <- c(if(result$metric == "POISE") c(21, 31, 46, 61), if(result$stat.minimum > 0) result$stat.minimum)
        value.range <- range(c(d$BEST_VALUE[!is.na(d$BEST_VALUE)], across))
        margin <- if(diff(value.range) > 0) 0.06*diff(value.range) else 1
        value.range <- value.range+c(-1, 1)*margin

        ## Where extra weight pays off (get.tradeoff.efficiency): each line shaded behind the chart,
        ## colored by how much it buys per unit of weight against the curve's average, outlined in white
        ## (the chart's background) so neighbouring lines stay apart, translucent so the grid shows
        ## through; each flat hatched in grey; each jump's rise drawn over the curve and labelled with
        ## its gain; each boundary point marked on the curve. Each point's hover text names the piece
        ## it's in, and its box takes that piece's color (a line's tint, a flat's grey, white otherwise);
        ## a boundary point also gives the rates either side.
        pieces <- if(is.null(tradeoff.efficiency())) NULL else tradeoff.efficiency()$data
        d$hover.color <- "white"
        if(!is.null(pieces)){
            lines.of <- pieces[TYPE == "line"]
            colors <- efficiency.color(lines.of$RATIO_TO_AVERAGE)
            for(i in seq_len(nrow(lines.of))){
                shapes[[length(shapes)+1]] <- list(type = "rect", x0 = lines.of$FROM[i], x1 = lines.of$TO[i], y0 = 0, y1 = 1, yref = "paper", fillcolor = colors[i], opacity = 0.35, line = list(width = 2, color = "white"), layer = "below")
            }
            ## The pieces a point can belong to: jumps, lines, flats, and boundary points that are a
            ## single step between two flats (others share their step with the line beside them)
            between.flats <- pieces$TYPE == "boundary" & c(FALSE, head(pieces$TYPE, -1) == "flat") & c(tail(pieces$TYPE, -1) == "flat", FALSE)
            owners <- pieces[pieces$TYPE != "boundary" | between.flats]
            first.weight <- d$ARMOR_WEIGHT_LIMIT[which(!is.na(d$BEST_VALUE))[1]]
            rate <- function(v) if(v == 0) "flat" else sprintf("%.2fx average", v)
            for(j in which(!is.na(d$BEST_VALUE))){
                w <- d$ARMOR_WEIGHT_LIMIT[j]
                i <- which((owners$FROM < w-1e-9 | abs(w-first.weight) < 1e-9) & owners$TO >= w-1e-9)[1]
                if(is.na(i)){
                    next
                }
                o <- owners[i]
                ## Its first point: the next weight after the point it's measured from
                from <- if(abs(o$FROM-first.weight) < 1e-9 && i == 1) o$FROM else min(d$ARMOR_WEIGHT_LIMIT[d$ARMOR_WEIGHT_LIMIT > o$FROM+1e-9])
                span <- if(abs(from-o$TO) < 1e-9) sprintf("%.1f", o$TO) else sprintf("%.1f-%.1f", from, o$TO)
                d$hover[j] <- paste0(d$hover[j], "<br>", switch(o$TYPE,
                    line = sprintf("Line %s: %.2fx average (%+.*f %s per unit weight)", span, o$RATIO_TO_AVERAGE, value.digits+1, o$GAIN/(o$TO-o$FROM), metric.label),
                    flat = sprintf("Flat %s: no gain", span),
                    jump = sprintf("Jump at %.1f: %+.*f %s for %.1f weight", o$TO, value.digits, o$GAIN, metric.label, o$TO-o$FROM),
                    boundary = sprintf("Step at %.1f between two flats: %+.*f %s", o$TO, value.digits, o$GAIN, metric.label)
                ))
                d$hover.color[j] <- switch(o$TYPE, line = efficiency.color(o$RATIO_TO_AVERAGE, tint = 0.35), flat = "rgb(232,232,232)", "white")
                at <- which(pieces$TYPE == "boundary" & abs(pieces$TO-w) < 1e-9 & !between.flats)
                if(length(at) > 0){
                    d$hover[j] <- paste0(d$hover[j], sprintf("<br>Boundary: %s, then %s",rate(pieces$RATIO_BEFORE[at[1]]), rate(pieces$RATIO_AFTER[at[1]])))
                }
            }
            ## Jump labels, to the left of each rise, halfway up it
            jumps.of <- pieces[TYPE == "jump"]
            for(i in seq_len(nrow(jumps.of))){
                annotations[[length(annotations)+1]] <- list(
                    x = jumps.of$TO[i], y = (jumps.of$START_VALUE[i]+jumps.of$END_VALUE[i])/2, text = sprintf("%+.*f", value.digits, jumps.of$GAIN[i]),
                    showarrow = FALSE, xanchor = "right", xshift = -4, font = list(color = JUMP.COLOR), bgcolor = "rgba(255,255,255,0.8)"
                )
            }
        }
        ## Movement breakpoints that fall within the chart, the selected movement type's emphasized
        line.labels <- c(Light = "Light | Mid", Mid = "Mid | Fat", Fat = "Fat | Poop")
        for(movement in names(result$lines)){
            x <- result$lines[[movement]]
            if(x >= 0 && x <= max(d$ARMOR_WEIGHT_LIMIT)){
                selected <- movement == result$selected.movement
                shapes[[length(shapes)+1]] <- list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper", line = list(dash = if(selected) "solid" else "dash", width = if(selected) 2 else 1, color = if(selected) "black" else "gray"))
                annotations[[length(annotations)+1]] <- list(x = x, y = 1, yref = "paper", text = if(selected) paste0("<b>", line.labels[[movement]], "</b>") else line.labels[[movement]], showarrow = FALSE, xanchor = "center", yanchor = "bottom")
            }
        }
        ## Poise breakpoints players aim for (see the User Guide)
        if(result$metric == "POISE"){
            for(breakpoint in c(21, 31, 46, 61)){
                shapes[[length(shapes)+1]] <- list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = breakpoint, y1 = breakpoint, line = list(dash = "dot", color = "firebrick"))
                annotations[[length(annotations)+1]] <- list(x = 0, xref = "paper", y = breakpoint, text = paste("Poise", breakpoint), showarrow = FALSE, xanchor = "left", yanchor = "bottom")
            }
        }
        ## The user's minimum on the charted stat (ignored by the curve itself)
        if(result$stat.minimum > 0){
            shapes[[length(shapes)+1]] <- list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = result$stat.minimum, y1 = result$stat.minimum, line = list(dash = "dash", color = "steelblue"))
            annotations[[length(annotations)+1]] <- list(x = 1, xref = "paper", y = result$stat.minimum, text = paste("Your minimum:", format(result$stat.minimum)), showarrow = FALSE, xanchor = "right", yanchor = "bottom")
        }
        ## The curve from its first point with a set to its last: plotly drops points with no set at
        ## either end from the line and its hover text, but not from the hover colors, which would then
        ## be out of step. (Points with no set in between stay, as a gap, so the colors keep step.)
        shown <- which(!is.na(d$BEST_VALUE))
        if(length(shown) > 0){
            d <- d[min(shown):max(shown)]
        }
        ## The flats first (behind the curve), then the curve, then the jumps' rises and the boundary
        ## points over it - all but the curve left out of hover, so that each weight has one box
        p <- plotly::plot_ly(source = "tradeoffs")
        if(!is.null(pieces)){
            flats.of <- pieces[TYPE == "flat"]
            for(i in seq_len(nrow(flats.of))){
                p <- plotly::add_trace(
                    p, x = c(flats.of$FROM[i], flats.of$FROM[i], flats.of$TO[i], flats.of$TO[i], flats.of$FROM[i]),
                    y = value.range[c(1, 2, 2, 1, 1)], name = "flat", type = "scatter", mode = "lines", fill = "toself",
                    fillcolor = "rgba(150,150,150,0.12)", fillpattern = list(shape = "/", fgcolor = "rgb(150,150,150)", size = 8, solidity = 0.15),
                    line = list(width = 0), hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
                )
            }
        }
        p <- plotly::add_trace(
            p, data = d, x = ~ARMOR_WEIGHT_LIMIT, y = ~BEST_VALUE, customdata = ~point, text = ~hover, hoverinfo = "text",
            hoverlabel = list(bgcolor = d$hover.color, font = list(color = "black")), name = "curve",
            ## plotly's usual first color, set because the flats' traces come before it
            type = "scatter", mode = "lines+markers", line = list(shape = "hv", color = CURVE.COLOR), marker = list(color = CURVE.COLOR),
            showlegend = FALSE, inherit = FALSE
        )
        if(!is.null(pieces) && any(pieces$TYPE == "jump")){
            jumps.of <- pieces[TYPE == "jump"]
            ## Each rise as the curve draws it (along, then up), the jumps apart
            rise.x <- unlist(lapply(seq_len(nrow(jumps.of)), function(i) c(jumps.of$FROM[i], jumps.of$TO[i], jumps.of$TO[i], NA)))
            rise.y <- unlist(lapply(seq_len(nrow(jumps.of)), function(i) c(jumps.of$START_VALUE[i], jumps.of$START_VALUE[i], jumps.of$END_VALUE[i], NA)))
            p <- plotly::add_trace(
                p, x = rise.x, y = rise.y, name = "jumps", type = "scatter", mode = "lines",
                line = list(color = JUMP.COLOR, width = 5), connectgaps = FALSE, hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
            )
        }
        if(!is.null(pieces) && any(pieces$TYPE == "boundary")){
            bounds.of <- pieces[TYPE == "boundary"]
            p <- plotly::add_trace(
                p, x = bounds.of$TO, y = bounds.of$END_VALUE, name = "boundaries", type = "scatter", mode = "markers",
                marker = list(symbol = "diamond-open", size = 11, color = "rgb(40,40,40)", line = list(width = 2)),
                hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
            )
        }
        p <-
            plotly::layout(
                p,
                xaxis = list(title = "Armor weight limit"), yaxis = list(title = paste("Best", metric.label), range = value.range),
                shapes = shapes, annotations = annotations,
                ## Hover (and click) by weight alone, so the pointer needn't be on the marker
                hovermode = "x"
            )
        plotly::event_register(p, "plotly_click")
    })

    ## Clicking a point: its armor set's links. Triggered by plotly's own click input rather than by
    ## event_data(), which (via a deferred check) warns whenever it's called before the chart has
    ## been drawn and registered its click event - here it only runs after an actual click.
    shiny::observeEvent(input[["plotly_click-tradeoffs"]], {
        click <- plotly::event_data("plotly_click", source = "tradeoffs")
        point <- tradeoffdata()$data[click$customdata[1]]
        if(nrow(point) == 1 && !is.na(point$HEAD)){
            show.armor.links(point)
        }
    })

    ## The table under the chart: limit, best value (named for the chosen stat), movement class, the
    ## set's scores, then its pieces - with the score itself as the value when it's the chosen stat
    ## (its quality beside it), rather than shown twice. Each set's real armor weight is in the
    ## chart's hover.
    tradeoff.table <- function(result){
        if(result$metric == "SCORE"){
            return(result$data[, .(ARMOR_WEIGHT_LIMIT, SCORE_RAW, SCORE_QUALITY, MOVEMENT, HEAD, CHEST, HANDS, LEGS)])
        }
        table <- result$data[, .(ARMOR_WEIGHT_LIMIT, BEST_VALUE, MOVEMENT, SCORE_RAW, SCORE_QUALITY, HEAD, CHEST, HANDS, LEGS)]
        data.table::setnames(table, "BEST_VALUE", result$metric)
        return(table)
    }

    output$tradeoff_table <- DT::renderDataTable({
        result <- tradeoffdata()
        shiny::req(result)
        value.digits <- if(result$metric == "POISE") 0 else 1
        table <-
            DT::datatable(
                tradeoff.table(result),
                selection = "single",
                options = list(scrollX = TRUE, paging = FALSE, scrollY = "400px", scrollCollapse = TRUE)
            ) |>
            DT::formatCurrency("ARMOR_WEIGHT_LIMIT", currency = "", interval = 3, mark = ",", digits = 1) |>
            DT::formatCurrency("SCORE_RAW", currency = "", interval = 3, mark = ",", digits = 3)
        if(result$metric != "SCORE"){
            table <- DT::formatCurrency(table, result$metric, currency = "", interval = 3, mark = ",", digits = value.digits)
        }
        table
    })

    shiny::observeEvent(input$tradeoff_table_rows_selected, {
        point <- tradeoffdata()$data[input$tradeoff_table_rows_selected]
        if(!is.na(point$HEAD)){
            show.armor.links(point)
        }
    })


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
                paste("Minimum", minima.metrics),
                paste("Score Weight", weight.metrics)
            ),
            VALUE = c(
                format(args$max.table.size, scientific = FALSE), args$starting.class, listed(args$areas.completed), listed(args$upgrade.types),
                listed(args$head.filter), listed(args$chest.filter), listed(args$hands.filter), listed(args$legs.filter),
                args$regular.level, args$twinkling.level,
                yes.no(args$havel.ring), yes.no(args$favor.ring), yes.no(args$wolf.ring),
                args$movement, as.character(args$endurance.level), unname(args$weapons[c("right.1", "right.2", "left.1", "left.2")]),
                as.character(args$minima),
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
            tradeoffs <- tradeoffdata()
            if(is.null(tradeoffs) || !identical(tradeoffs$key, tradeoff.key(snapshot, input$tradeoff_metric))){
                tradeoffs <- compute.tradeoffs(snapshot, input$tradeoff_metric)
                tradeoffdata(tradeoffs)
            }
            writexl::write_xlsx(
                list(
                    Results = armordata()$data,
                    `Trade-offs` = tradeoff.table(tradeoffs),
                    Settings = settings.sheet(snapshot)
                ),
                file
            )
        }
    )


}
