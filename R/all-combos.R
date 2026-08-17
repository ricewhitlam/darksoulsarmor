

#' @name get.all.armor.combos
#' 
#' @title Create a \code{data.table} of all possible Dark Souls armor combinations
#'
#' @description
#' Produces a table of every possible armor combination in Dark Souls.
#' This table is large (>10M rows and 17 columns, nearly 2 GB).
#' Only call this function if there is enough available RAM to support this.
#' All relevant metrics are included.
#' All metrics are aggregated as would be expected with perhaps one exception: 
#' Durability is aggregated by taking the minimum across all armor pieces.
#' 
#' @param
#' regular.level A length 1 \code{character} indicating the upgrade level of armor pieces ascended via regular titanite. 
#' Options are \code{"+0"} thru \code{"+10"}. 
#' Metrics are exact for \code{"+0"} and \code{"+10"}. 
#' For the other options, metrics are approximated based on the game's default upgrade patterns.
#' These approximations should be very accurate but will differ from true values slightly.
#' Defaults to \code{"+0"}.
#' 
#' @param 
#' twinkling.level A length 1 \code{character} indicating the upgrade level of armor pieces ascended via twinkling titanite. 
#' Options are \code{"+0"} thru \code{"+5"}. 
#' Metrics are exact for \code{"+0"} and \code{"+5"}. 
#' For the other options, metrics are approximated based on the game's default upgrade patterns.
#' These approximations should be very accurate but will differ from true values slightly.
#' Defaults to \code{"+0"}.
#' 
#' @return
#' A \code{data.table} of all possible armor combinations
#'
#' @examples
#' \dontrun{
#' all.armor.combos <- get.all.armor.combos()
#' }
#'
get.all.armor.combos <- function(regular.level = c("+0", "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8", "+9", "+10")[1], twinkling.level = c("+0", "+1", "+2", "+3", "+4", "+5")[1]){

    ## Check regular.level
    if(!is.character(regular.level)){
        stop("Invalid argument 'regular.level'")
    } else if(length(regular.level) != 1){
        stop("Invalid argument 'regular.level'")
    } else if(is.na(regular.level)){
        stop("Invalid argument 'regular.level'")
    } else if(!(regular.level %in% c("+0", "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8", "+9", "+10"))){
        stop("Invalid argument 'regular.level'")
    }

    ## Check twinkling.level
    if(!is.character(twinkling.level)){
        stop("Invalid argument 'twinkling.level'")
    } else if(length(twinkling.level) != 1){
        stop("Invalid argument 'twinkling.level'")
    } else if(is.na(twinkling.level)){
        stop("Invalid argument 'twinkling.level'")
    } else if(!(twinkling.level %in% c("+0", "+1", "+2", "+3", "+4", "+5"))){
        stop("Invalid argument 'twinkling.level'")
    }

    ## Interpolate to specified upgrade levels
    working.head.data <- get.interp.data(head.data.unupgraded, head.data.fullupgrade, as.numeric(regular.level), as.numeric(twinkling.level))
    working.chest.data <- get.interp.data(chest.data.unupgraded, chest.data.fullupgrade, as.numeric(regular.level), as.numeric(twinkling.level))
    working.hands.data <- get.interp.data(hands.data.unupgraded, hands.data.fullupgrade, as.numeric(regular.level), as.numeric(twinkling.level))
    working.legs.data <- get.interp.data(legs.data.unupgraded, legs.data.fullupgrade, as.numeric(regular.level), as.numeric(twinkling.level))

    ## Call cpp function to create and return the table of all combos
    full.data <- data.table::setDT(all_armor_combinations(working.head.data, working.chest.data, working.hands.data, working.legs.data))

    rm(list = c("working.head.data", "working.chest.data", "working.hands.data", "working.legs.data"))
    gc()

    return(full.data)

}

