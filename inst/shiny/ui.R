
bslib::page_sidebar(
        sidebar = 
            bslib::sidebar( 
            
                width = 360,

                shiny::fluidRow(
                    shiny::column(9, shiny::textOutput(outputId = "mode_label")),
                    shiny::column(3, bslib::input_dark_mode(id = "mode", mode = "dark"))
                ),
                
                shiny::actionButton(inputId = "guide", label = "User Guide"),
                
                shiny::hr(),

                filter.inputs.button("filters"),
                upgrade.inputs.button("upgrades"),
                ring.inputs.button("rings"),
                load.inputs.button("constraints"),
                minimum.inputs.button("minima"),
                score.inputs.button("scores"),
                
                shiny::hr(),

                shiny::actionButton(inputId = "go", label = "Refresh Armor Data"),
                shiny::downloadButton(outputId = "download", label = "Download Armor Data"),

            ),
        
        shiny::tags$head(
            shiny::tags$style(
                shiny::HTML('
                    td[data-type="factor"] input {
                        width: 100px !important;
                    }
                ')
            )
        ),

        shiny::tags$head(
            shiny::tags$style("
                #errormessage{
                    color: red;
                    font-size: 20px;
                    font-weight: bold;
                }
            ")
        ),

        shiny::tags$head(
            shiny::tags$style("
                #refreshmessage{
                    color: red;
                    font-size: 20px;
                    font-weight: bold;
                }
            ")
        ),

        shiny::textOutput("errormessage"),
        shiny::textOutput("refreshmessage"),

        ## Each tab's value (what input$main_tabs reports) is fixed, apart from its label
        bslib::navset_card_tab(
            id = "main_tabs",
            bslib::nav_panel(
                "Top Scoring Combos",
                value = "table",
                DT::dataTableOutput(outputId = "table")
            ),
            bslib::nav_panel(
                "Best Combo by Weight",
                value = "graph",
                tradeoffs.tab("tradeoffs")
            )
        )

    )
