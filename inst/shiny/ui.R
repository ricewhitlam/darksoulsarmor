
bslib::page_sidebar(
        sidebar = 
            bslib::sidebar( 
            
                width = 360,

                shiny::fluidRow(
                    shiny::column(9, shiny::textOutput(outputId = "mode")), 
                    shiny::column(3, bslib::input_dark_mode(id = "mode", mode = "dark"))
                ),
                
                shiny::actionButton(inputId = "guide", label = "User Guide"),
                
                hr(),

                shiny::actionButton(inputId = "filters", label = "Filter Inputs"),
                shiny::actionButton(inputId = "upgrades", label = "Upgrade Inputs"),
                shiny::actionButton(inputId = "rings", label = "Ring Inputs"),
                shiny::actionButton(inputId = "constraints", label = "Load Inputs"),
                actionButton(inputId = "minima", label = "Minimum Inputs"),
                shiny::actionButton(inputId = "weights", label = "Score Inputs"),
                
                hr(),

                shiny::actionButton(inputId = "go", label = "Refresh Armor Data"),
                shiny::downloadButton("download","Download Armor Data"),
            
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

        shiny::textOutput("errormessage"),
        DT::dataTableOutput(outputId = "table")
    
    )
