
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

                shiny::actionButton(inputId = "filters", label = "Filter Inputs"),
                shiny::actionButton(inputId = "upgrades", label = "Upgrade Inputs"),
                shiny::actionButton(inputId = "rings", label = "Ring Inputs"),
                shiny::actionButton(inputId = "constraints", label = "Load Inputs"),
                shiny::actionButton(inputId = "minima", label = "Minimum Inputs"),
                shiny::actionButton(inputId = "weights", label = "Score Inputs"),
                
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
                    font-style: bold;
                }
            ")
        ),

        shiny::tags$head(
            shiny::tags$style("
                #refreshmessage{
                    color: red;
                    font-size: 20px;
                    font-style: bold;
                }
            ")
        ),

        shiny::textOutput("errormessage"),
        shiny::textOutput("refreshmessage"),

        bslib::navset_card_tab(
            id = "main_tabs",
            bslib::nav_panel(
                "Results",
                DT::dataTableOutput(outputId = "table")
            ),
            bslib::nav_panel(
                "Trade-offs",
                shiny::fluidRow(
                    shiny::column(
                        4,
                        shiny::selectInput(
                            inputId = "tradeoff_metric",
                            label = "Maximize",
                            choices = c(
                                "Score" = "SCORE", "Poise" = "POISE",
                                "Physical Defense" = "PHYS_DEF", "Strike Defense" = "STRIKE_DEF", "Slash Defense" = "SLASH_DEF", "Thrust Defense" = "THRUST_DEF",
                                "Magic Defense" = "MAG_DEF", "Fire Defense" = "FIRE_DEF", "Lightning Defense" = "LITNG_DEF",
                                "Bleed Resistance" = "BLEED_RES", "Poison Resistance" = "POIS_RES", "Curse Resistance" = "CURSE_RES"
                            )
                        )
                    ),
                    shiny::column(
                        2,
                        shiny::selectInput(inputId = "tradeoff_jumps", label = "Jumps", choices = c("Few" = "few", "Some" = "some", "Many" = "many"), selected = "some")
                    ),
                    shiny::column(
                        2,
                        shiny::selectInput(inputId = "tradeoff_flats", label = "Flats", choices = c("Few" = "few", "Some" = "some", "Many" = "many"), selected = "some")
                    )
                ),
                shiny::helpText(
                    "For every armor weight, the most of the chosen stat any armor set can reach, using the settings of the last refresh.",
                    "Vertical lines mark where your movement type changes (the selected movement type's line is solid). The shaded bands split the curve",
                    "into regions: green regions buy more of the stat per unit of armor weight than the curve's average, purple ones less. Hover over a point",
                    "for its set, its movement type and its region's rate; click a point or a table row for its links."
                ),
                plotly::plotlyOutput(outputId = "tradeoff_plot"),
                DT::dataTableOutput(outputId = "tradeoff_table")
            )
        )

    )
