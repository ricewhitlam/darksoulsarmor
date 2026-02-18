

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
#' @usage 
#' all.armor.combos <- get.all.armor.combos()
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
    working.head.data <- get.interp.data(head.data_00, head.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.chest.data <- get.interp.data(chest.data_00, chest.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.hands.data <- get.interp.data(hands.data_00, hands.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.legs.data <- get.interp.data(legs.data_00, legs.data_10, as.numeric(regular.level), as.numeric(twinkling.level))

    ## Initialize output data
    N <- nrow(working.head.data)*nrow(working.chest.data)*nrow(working.hands.data)*nrow(working.legs.data)
    full.data <- 
        data.table::data.table(
            HEAD = character(N),
            CHEST = character(N),
            HANDS = character(N),
            LEGS = character(N),
            PHYS_DEF = numeric(N),
            STRIKE_DEF = numeric(N),
            SLASH_DEF = numeric(N),
            THRUST_DEF = numeric(N),
            MAG_DEF = numeric(N),
            FIRE_DEF = numeric(N),
            LITNG_DEF = numeric(N),
            POISE = numeric(N),
            BLEED_RES = numeric(N),
            POIS_RES = numeric(N),
            CURSE_RES = numeric(N),
            DURABILITY = numeric(N),
            WEIGHT = numeric(N)
        )
    
    ## Call cpp function to create all combos
    all_armor_combinations(working.head.data, working.chest.data, working.hands.data, working.legs.data, full.data)

    rm(list = c("working.head.data", "working.chest.data", "working.hands.data", "working.legs.data", "N"))
    gc()

    return(full.data)

}

