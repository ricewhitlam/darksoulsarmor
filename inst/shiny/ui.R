
## Pages along the top, each with its own sidebar; the User Guide and the light/dark switch, which
## belong to the whole app, at the right of the bar. Each page's value (what input$pages reports) is
## fixed, apart from its label.
bslib::page_navbar(
        title = "Dark Souls Armor",
        id = "pages",

        header = shiny::tagList(
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
            )
        ),

        ## The search: the sidebar's settings, applied by 'Refresh Armor Data' to the table and the graph
        bslib::nav_panel(
            "Armor Optimization",
            value = "optimization",
            bslib::layout_sidebar(
                sidebar =
                    bslib::sidebar(

                        width = 360,

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
        ),

        bslib::nav_spacer(),
        bslib::nav_item(shiny::actionLink(inputId = "guide", label = "User Guide")),
        bslib::nav_item(bslib::input_dark_mode(id = "mode", mode = "dark"))

    )
