

#' @name armor.application
#' 
#' @title Run an R Shiny application to Produce Optimized Armor Combinations
#'
#' @description
#' Runs an R Shiny application which facilitates Dark Souls armor optimization.
#' Various UI elements allow constraints and goals to be specified.
#' A table is produced which displays optimal armor combinations.
#' Some links are included in this app. To ensure that they work, launch the app in a browser.
#' 
#' @param
#' ... Arguments are passed to \code{shiny::runApp}.
#'
#' @return
#' No value is returned - rather, a Shiny application is run.
#'
#' @examples
#' \dontrun{
#' armor.application()
#' }
#'
armor.application <- function(...){
    appDir <- system.file("shiny", package = "darksoulsarmor")
    if(appDir == ""){
        stop("Could not find shiny. Try re-installing `darksoulsarmor`.", call. = FALSE)
    }
    shiny::runApp(appDir = appDir, ...)
}

## The app's packages are listed in DESCRIPTION's Imports (so installing darksoulsarmor installs
## them) but deliberately not imported in NAMESPACE, so library(darksoulsarmor) doesn't load the
## whole Shiny stack for users who only call the search functions - they load when the app runs.
## Their only uses are in inst/shiny, which R CMD check doesn't scan, so this never-called function
## names one export of each to mark the Imports as used.
app.imports <- function(){
    list(shiny::runApp, shinyWidgets::pickerInput, bslib::page_sidebar, DT::datatable, shinybusy::show_modal_spinner, plotly::plot_ly, writexl::write_xlsx)
}

