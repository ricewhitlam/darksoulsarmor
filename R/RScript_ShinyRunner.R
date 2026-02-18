

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
#' @usage 
#' armor.application()
#' 
#' @param 
#' ... Arguments are passed to \code{shiny::runApp}.
#' 
#' @return
#' No value is returned - rather, a Shiny application is run.
#' 
armor.application <- function(...){
    appDir <- system.file("shiny", package = "darksoulsarmor")
    if(appDir == ""){
        stop("Could not find shiny. Try re-installing `darksoulsarmor`.", call. = FALSE)   
    }
    shiny::runApp(appDir = appDir, ...)
}

