
get.reg.def.weight_10 <- approxfun(x = 0:10, y = c(0, 10/142, 20/142, 30/142, 43/142, 56/142, 69/142, 85/142, 101/142, 117/142, 1))
get.reg.res.weight_10 <- approxfun(x = 0:10, y = c(0, 0, 0, 0, 1/8, 2/8, 3/8, 4/8, 5/8, 6/8, 1))
get.twink.def.weight_05 <- approxfun(x = 0:5, y = c(0, 8/55, 19/55, 29/55, 39/55, 1))
get.twink.res.weight_05 <- approxfun(x = 0:5, y = c(0, 5/27, 9/27, 14/27, 18/27, 1))

get.interp.data <- function(data_00, data_10, reg.lvl, twink.lvl){

    if(!all(data_00$ARMOR == data_10$ARMOR)){
        data.table::setorder(data_00, ARMOR)
        data.table::setorder(data_10, ARMOR)
        if(!all(data_00$ARMOR == data_10$ARMOR)){
            stop("Armor sets are incompatible - check underlying data")
        }
    }

    def.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF")
    res.cols <- c("BLEED_RES", "POIS_RES", "CURSE_RES")
    def.res.cols <- c(def.cols, res.cols)

    reg.weights_10 <- c(rep(get.reg.def.weight_10(reg.lvl), 7), rep(get.reg.res.weight_10(reg.lvl), 3))
    twink.weights_05 <- c(rep(get.twink.def.weight_05(twink.lvl), 7), rep(get.twink.res.weight_05(twink.lvl), 3))
    
    data.final <- 
        rbind(
            data.table::copy(data_00)[
                UPGRADE_TYPE == "Regular", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, 1-reg.weights_10, SIMPLIFY = FALSE)
            ][
                UPGRADE_TYPE == "Twinkling", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, 1-twink.weights_05, SIMPLIFY = FALSE)
            ],
            data.table::copy(data_10)[UPGRADE_TYPE != "None"][, c("POISE", "DURABILITY", "WEIGHT") := 0][
                UPGRADE_TYPE == "Regular", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, reg.weights_10, SIMPLIFY = FALSE)
            ][
                UPGRADE_TYPE == "Twinkling", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, twink.weights_05, SIMPLIFY = FALSE)
            ]
        )[,
            lapply(.SD, function(x){round(sum(x), 1)}), 
            by = .(ARMOR, UPGRADE_TYPE, STARTING_CLASS, AREA_FORMULA, LINK),
            .SDcols = c(def.cols, "POISE", res.cols, "DURABILITY", "WEIGHT")
        ]

    data.table::setnames(data.final, c("ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", def.cols, "POISE", res.cols, "DURABILITY", "WEIGHT"))
    data.table::setorder(data.final, ARMOR)

    rm(list = c("def.cols", "res.cols", "def.res.cols", "reg.weights_10", "twink.weights_05"))
    gc()

    return(data.final)

}

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

    working.head.data <- get.interp.data(head.data_00, head.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.chest.data <- get.interp.data(chest.data_00, chest.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.hands.data <- get.interp.data(hands.data_00, hands.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.legs.data <- get.interp.data(legs.data_00, legs.data_10, as.numeric(regular.level), as.numeric(twinkling.level))

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
    
    all_armor_combinations(working.head.data, working.chest.data, working.hands.data, working.legs.data, full.data)

    rm(list = c("working.head.data", "working.chest.data", "working.hands.data", "working.legs.data", "N"))
    gc()

    return(full.data)

}

#' @name get.optimal.armor.combos
#' 
#' @title Create a \code{data.table} of optimized Dark Souls armor combinations
#'
#' @description
#' Produces a table of optimized armor combinations in Dark Souls.
#' All relevant metrics are included, as well as a column of scores on which the data is sorted.
#' These scores are between \code{0} and \code{1} and indicate how optimal each combination is.
#' The table can be tailored to satisfy various constraints.
#' 
#' @usage 
#' optimal.armor.combos <- get.optimal.armor.combos(upgraded = TRUE, endurance.level = 40, unarmored.weight = 12, fap.ring = TRUE, roll = "Fast")
#' 
#' @param 
#' max.table.size A length 1 \code{numeric} indicating how large the produced table should be.
#' Defaults to \code{1000}. Passed values will be clamped between 1 and 100,000,000 and cast to an integer.
#' 
#' @param 
#' starting.class A length 1 \code{character} indicating whether to consider unupgraded or fully upgraded armor.
#' Defaults to \code{"Bandit"}. The vector of available classes is stored as \code{classes}.
#' 
#' @param 
#' areas.completed A \code{character} vector indicating which areas have been completed.
#' Defaults to all areas being complete. The vector of available areas is stored as \code{areas}.
#' 
#' @param 
#' upgrade.types A \code{character} vector indicating which upgrade types to consider.
#' There are three types available: \code{"Regular"} (armor pieces that upgrade with regular titanite), 
#' \code{"Twinkling"} (armor pieces that upgrade with twinkling titanite), 
#' and \code{"None"} (armor pieces that cannot be upgraded).
#' Defaults to all types i.e. \code{c("Regular", "Twinkling", "None")}.
#' 
#' @param 
#' head.filter A \code{character} vector indicating which head armor pieces should be included. 
#' Defaults to all pieces. A vector of available pieces can be accessed via \code{head.data_00$ARMOR}.
#' If an empty vector is passed, it is assumed that no head armor will be worn.
#' 
#' @param 
#' chest.filter Same as \code{head.filter} but for chest armor pieces. 
#' A vector of available pieces can be accessed via \code{chest.data_00$ARMOR}.
#' 
#' @param 
#' hands.filter Same as \code{head.filter} but for hand armor pieces.
#' A vector of available pieces can be accessed via \code{hands.data_00$ARMOR}.
#' 
#' @param 
#' legs.filter Same as \code{head.filter} but for leg armor pieces.
#' A vector of available pieces can be accessed via \code{legs.data_00$ARMOR}.
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
#' @param 
#' roll A length 1 \code{character} indicating desired roll speed. 
#' Options are \code{"Fast"} (weight at or below 25\% of max equip load), \code{"Mid"} (weight at or below 50% of max equip load), \code{"Fat"} (weight at or below 100% of max equip load), and \code{"None"} (weight above 100% of max equip load).
#' Defaults to \code{"Fast"}.
#' 
#' @param 
#' unarmored.weight A length 1 \code{numeric} indicating the unarmored weight of the character i.e. weight of weapons.
#' Defaults to \code{10}.
#' Passed values are clamped between 0 and 999 and rounded to the nearest decimal point.
#' 
#' @param 
#' endurance.level A length 1 \code{numeric} indicating the level of the character in the Endurance stat. 
#' This stat increases maximum equip load. Defaults to \code{10}. 
#' Passed values are clamped between 0 and 99 and rounded to an integer.
#' 
#' @param 
#' havel.ring A length 1 \code{logical} indicating whether Havels Ring is equipped. 
#' This ring increases maximum equip load by 50\%. Defaults to \code{FALSE}.
#' 
#' @param 
#' fap.ring A length 1 \code{logical} indicating whether the Ring of Favor and Protection is equipped. 
#' This ring increases maximum equip load by 20\%. Defaults to \code{FALSE}.
#' 
#' @param 
#' wolf.ring A length 1 \code{logical} indicating whether the Wolf Ring is equipped. 
#' This ring increases Poise by 40. Defaults to \code{FALSE}.
#' 
#' @param 
#' minima A length 12 \code{numeric} indicating minimum allowable values for the following ordered metrics:
#' PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF, LITNG_DEF, POISE, BLEED_RES, POIS_RES, CURSE_RES, DURABILITY.
#' Defaults to \code{c(0,0,0,0,0,0,0,0,0,0,0,0)}.
#' Passed values are clamped between 0 and 999.
#' 
#' @param 
#' weights A length 10 \code{numeric} indicating weights for the following ordered metrics:
#' PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF, LITNG_DEF, BLEED_RES, POIS_RES, CURSE_RES.
#' Defaults to \code{c(0.16,0.16,0.16,0.16,0.08,0.08,0.08,0.04,0.04,0.04)}.
#' These weights are used in the calculation of a score. This score is then optimized across all possible armor combinations.
#' Increasing the weight on a metric increases its importance to the final score.
#' The weights do not need to sum to 1, but should all be nonnegative with positive sum.
#' 
#' @return
#' A \code{list} holding (1) the list of arguments which defined the table and (2) a \code{data.table} of optimal armor combinations
#' 
get.optimal.armor.combos <- function(
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
){

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

    ## Check upgrade.types
    if(is.null(upgrade.types)){
        upgrade.types <- character(0)
    } else if(!is.character(upgrade.types)){
        stop("Invalid argument 'upgrade.types'")
    } else if(any(is.na(upgrade.types))){
        stop("Invalid argument 'upgrade.types'")
    } else if(length(upgrade.types) != 0){
        if(!all(upgrade.types %in% c("Regular", "Twinkling", "None"))){
            stop("Invalid argument 'upgrade.types'")
        }
    } else{
        upgrade.types <- unique(upgrade.types)
    }

    ## Check starting.class
    if(!is.character(starting.class)){
        stop("Invalid argument 'starting.class'")
    } else if(length(starting.class) != 1){
        stop("Invalid argument 'starting.class'")
    } else if(is.na(starting.class)){
        stop("Invalid argument 'starting.class'")
    } else if(!(starting.class %in% classes)){
        stop("Invalid argument 'starting.class'")
    }

    ## Check areas.completed
    if(is.null(areas.completed)){
        areas.completed <- character(0)
    } else if(!is.character(areas.completed)){
        stop("Invalid argument 'areas.completed'")
    } else if(any(is.na(areas.completed))){
        stop("Invalid argument 'areas.completed'")
    } else if(length(areas.completed) != 0){
        if(!all(areas.completed %in% areas)){
            stop("Invalid argument 'areas.completed'")
        }
    } else{
        areas.completed <- unique(areas.completed)
    }

    ## Check head.filter
    if(is.null(head.filter)){
        head.filter <- character(0)
    } else if(!is.character(head.filter)){
        stop("Invalid argument 'head.filter'")
    } else if(any(is.na(head.filter))){
        stop("Invalid argument 'head.filter'")
    } else if(length(head.filter) != 0){
        if(!all(head.filter %in% head.data_00$ARMOR)){
            stop("Invalid argument 'head.filter'")
        }
    } else if(length(head.filter) == 0){
        head.filter <- "No Head"
    } else{
        head.filter <- unique(head.filter)
    }

    ## Check chest.filter
    if(is.null(chest.filter)){
        chest.filter <- character(0)
    } else if(!is.character(chest.filter)){
        stop("Invalid argument 'chest.filter'")
    } else if(any(is.na(chest.filter))){
        stop("Invalid argument 'chest.filter'")
    } else if(length(chest.filter) != 0){
        if(!all(chest.filter %in% chest.data_00$ARMOR)){
            stop("Invalid argument 'chest.filter'")
        }
    } else if(length(chest.filter) == 0){
        chest.filter <- "No Chest"
    } else{
        chest.filter <- unique(chest.filter)
    }
    
    ## Check hands.filter
    if(is.null(hands.filter)){
        hands.filter <- character(0)
    } else if(!is.character(hands.filter)){
        stop("Invalid argument 'hands.filter'")
    } else if(any(is.na(hands.filter))){
        stop("Invalid argument 'hands.filter'")
    } else if(length(hands.filter) != 0){
        if(!all(hands.filter %in% hands.data_00$ARMOR)){
            stop("Invalid argument 'hands.filter'")
        }
    } else if(length(hands.filter) == 0){
        hands.filter <- "No Hands"
    } else{
        hands.filter <- unique(hands.filter)
    }

    ## Check legs.filter
    if(is.null(legs.filter)){
        legs.filter <- character(0)
    } else if(!is.character(legs.filter)){
        stop("Invalid argument 'legs.filter'")
    } else if(any(is.na(legs.filter))){
        stop("Invalid argument 'legs.filter'")
    } else if(length(legs.filter) != 0){
        if(!all(legs.filter %in% legs.data_00$ARMOR)){
            stop("Invalid argument 'legs.filter'")
        }
    } else if(length(legs.filter) == 0){
        legs.filter <- "No Legs"
    } else{
        legs.filter <- unique(legs.filter)
    }

    ## Check endurance.level
    if(!is.numeric(endurance.level)){
        stop("Invalid argument 'endurance.level'")
    } else if(length(endurance.level) != 1){
        stop("Invalid argument 'endurance.level'")
    } else if(is.na(endurance.level) || is.nan(endurance.level)){
        stop("Invalid argument 'endurance.level'")
    } else{
        endurance.level <- round(median(c(0, endurance.level, 99)), 0)
    }

    ## Check unarmored.weight
    if(!is.numeric(unarmored.weight)){
        stop("Invalid argument 'unarmored.weight'")
    } else if(length(unarmored.weight) != 1){
        stop("Invalid argument 'unarmored.weight'")
    } else if(is.na(unarmored.weight) || is.nan(unarmored.weight)){
        stop("Invalid argument 'unarmored.weight'")
    } else{
        unarmored.weight <- round(median(c(0, unarmored.weight, 999)), 1)
    }

    ## Check wolf.ring
    if(!is.logical(wolf.ring)){
        stop("Invalid argument 'wolf.ring'")
    } else if(length(wolf.ring) != 1){
        stop("Invalid argument 'wolf.ring'")
    } else if(is.na(wolf.ring)){
        stop("Invalid argument 'wolf.ring'")
    }

    ## Check havel.ring
    if(!is.logical(havel.ring)){
        stop("Invalid argument 'havel.ring'")
    } else if(length(havel.ring) != 1){
        stop("Invalid argument 'havel.ring'")
    } else if(is.na(havel.ring)){
        stop("Invalid argument 'havel.ring'")
    }

    ## Check fap.ring
    if(!is.logical(fap.ring)){
        stop("Invalid argument 'fap.ring'")
    } else if(length(fap.ring) != 1){
        stop("Invalid argument 'fap.ring'")
    } else if(is.na(fap.ring)){
        stop("Invalid argument 'fap.ring'")
    }

    ## Check roll
    if(!is.character(roll)){
        stop("Invalid argument 'roll'")
    } else if(length(roll) != 1){
        stop("Invalid argument 'roll'")
    } else if(is.na(roll)){
        stop("Invalid argument 'roll'")
    } else if(!(roll %in% c("Fast", "Mid", "Fat", "None"))){
        stop("Invalid argument 'roll'")
    }
    
    ## Check minima
    if(!is.numeric(minima)){
        stop("Invalid argument 'minima'")
    } else if(length(minima) != 12){
        stop("Invalid argument 'minima'")
    } else if(any(is.na(minima) | is.nan(minima))){
        stop("Invalid argument 'minima'")
    } else{
        minima <- pmin(999, pmax(0, minima))
    }

    ## Check weights
    if(!is.numeric(weights)){
        stop("Invalid argument 'weights'")
    } else if(length(weights) != 10){
        stop("Invalid argument 'weights'")
    } else if(any(is.na(weights) | is.nan(weights) | is.infinite(weights))){
        stop("Invalid argument 'weights'")
    } else if(any(weights < 0)){
        stop("Invalid argument 'weights'")
    } else if((sum(weights) < 1e-15)){
        stop("Invalid argument 'weights'")
    } else{
        weights <- weights/sum(weights)
    }

    ## Check max.table.size
    if(!is.numeric(max.table.size)){
        stop("Invalid argument 'max.table.size'")
    } else if(length(max.table.size) != 1){
        stop("Invalid argument 'max.table.size'")
    } else if(is.na(max.table.size) || is.nan(max.table.size)){
        stop("Invalid argument 'max.table.size'")
    } else{
        max.table.size <- round(median(c(1, max.table.size, 1e8)), 0)
    }

    ## Initialize output
    out <- 
        list(
            args = 
                list(
                    max.table.size = max.table.size,
                    starting.class = starting.class,
                    areas.completed = areas.completed,
                    upgrade.types = upgrade.types,
                    head.filter = head.filter,
                    chest.filter = chest.filter,
                    hands.filter = hands.filter,
                    legs.filter = legs.filter,
                    regular.level = regular.level, 
                    twinkling.level = twinkling.level,
                    roll = roll,
                    unarmored.weight = unarmored.weight,
                    endurance.level = endurance.level,
                    havel.ring = havel.ring,
                    fap.ring = fap.ring,
                    wolf.ring = wolf.ring,
                    minima = minima,
                    weights = weights
                ),
            data = data.table::data.table()
        )

    ## Get data at specified upgrade levels
    working.head.data <- get.interp.data(head.data_00, head.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.chest.data <- get.interp.data(chest.data_00, chest.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.hands.data <- get.interp.data(hands.data_00, hands.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.legs.data <- get.interp.data(legs.data_00, legs.data_10, as.numeric(regular.level), as.numeric(twinkling.level))
    working.mean.stddev.corr <- mean.stddev.corr.list[[paste0(as.numeric(regular.level), "_", as.numeric(twinkling.level))]]

    ## Calc equip load values
    base.load <- (endurance.level+40)*ifelse(havel.ring, 1.5, 1)*ifelse(fap.ring, 1.2, 1)
    roll.mult <- c(0.25, 0.5, 1.0, 999.0)[match(roll, c("Fast", "Mid", "Fat", "None"))]
    load.threshold <- base.load*roll.mult
    load.threshold.motf <- load.threshold*1.05

    ## Filter datasets based on inputs
    working.head.data[, AREAFILTER := sapply(AREA_FORMULA, function(x){eval(parse(text = x))(areas.completed)})]
    working.head.data <- 
        working.head.data[
            (ARMOR %in% head.filter) & 
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Head") & 
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class) & 
            (WEIGHT <= data.table::fifelse(ARMOR == "Mask of the Father", -unarmored.weight+load.threshold.motf+1e-10, -unarmored.weight+load.threshold+1e-10))
        ]

    working.chest.data[, AREAFILTER := sapply(AREA_FORMULA, function(x){eval(parse(text = x))(areas.completed)})]
    working.chest.data <- 
        working.chest.data[
            (ARMOR %in% chest.filter) & 
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Chest") & 
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class) & 
            (WEIGHT <= (-unarmored.weight+max(load.threshold, load.threshold.motf-1.2)+1e-10))
        ]

    working.hands.data[, AREAFILTER := sapply(AREA_FORMULA, function(x){eval(parse(text = x))(areas.completed)})]
    working.hands.data <- 
        working.hands.data[
            (ARMOR %in% hands.filter) & 
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Hands") & 
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class) &
            (WEIGHT <= (-unarmored.weight+max(load.threshold, load.threshold.motf-1.2)+1e-10))
        ]

    working.legs.data[, AREAFILTER := sapply(AREA_FORMULA, function(x){eval(parse(text = x))(areas.completed)})]
    working.legs.data <- 
        working.legs.data[
            (ARMOR %in% legs.filter) & 
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Legs") & 
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class) &
            (WEIGHT <= (-unarmored.weight+max(load.threshold, load.threshold.motf-1.2)+1e-10))
        ]

    ## If any tables empty, return empty data
    n.head <- nrow(working.head.data)
    n.chest<- nrow(working.chest.data)
    n.hands <- nrow(working.hands.data)
    n.legs <- nrow(working.legs.data)
    if(n.head == 0 || n.chest == 0 || n.hands == 0 || n.legs == 0){
        out$data[, SCORE := numeric(0)]
        out$data[, c("HEAD", "CHEST", "HANDS", "LEGS") := character(0)]
        out$data[, 
            c(
                "PHYS_DEF",
                "STRIKE_DEF",
                "SLASH_DEF",
                "THRUST_DEF",
                "MAG_DEF",
                "FIRE_DEF",
                "LITNG_DEF",
                "BLEED_RES",
                "POIS_RES",
                "CURSE_RES",
                "DURABILITY",
                "ARMOR_POISE",
                "TOTAL_POISE",
                "ARMOR_WEIGHT",
                "TOTAL_WEIGHT",
                "EQUIP_LOAD",
                "PCT_LOAD"
            ) := numeric(0)
        ]
        return(out)
    }

    ## Remove unneeded columns
    working.head.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "AREAFILTER") := NULL]
    working.chest.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "AREAFILTER") := NULL]
    working.hands.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "AREAFILTER") := NULL]
    working.legs.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "AREAFILTER") := NULL]

    ## Calc scores for each dataset and sort on these
    score.means <- working.mean.stddev.corr$means
    score.scalars <- (weights)/(working.mean.stddev.corr$stddevs*sqrt((t(weights) %*% working.mean.stddev.corr$corrs %*% weights)[1, 1]))
    score.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
    
    working.head.data[, SCORE := 0]
    working.chest.data[, SCORE := 0]
    working.hands.data[, SCORE := 0]
    working.legs.data[, SCORE := 0]
    for(i in seq_along(score.cols)){
        working.head.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
        working.chest.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
        working.hands.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
        working.legs.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
    }
    data.table::setorder(working.head.data, -SCORE, WEIGHT)
    data.table::setorder(working.chest.data, -SCORE, WEIGHT)
    data.table::setorder(working.hands.data, -SCORE, WEIGHT)
    data.table::setorder(working.legs.data, -SCORE, WEIGHT)

    ## Sort data such that it is in "optimal chunks"
    ## Within each chunk, each entry has the best score for its weight in the remaining data
    working.head.data <- working.head.data[optimized_search_order(SCORE, WEIGHT)]
    working.chest.data <- working.chest.data[optimized_search_order(SCORE, WEIGHT)]
    working.hands.data <- working.hands.data[optimized_search_order(SCORE, WEIGHT)]
    working.legs.data <- working.legs.data[optimized_search_order(SCORE, WEIGHT)]
  
    ## Determine position of MOTF in head data to apply equip load benefit
    motf.index <- which(working.head.data == "Mask of the Father")
    if(length(motf.index) == 0){
        motf.index <- 999
    } 

    ## Identify initial looping info based on allowable weight and minima
    n.max <- max(n.head, n.chest, n.hands, n.legs)
    weight.check <- 
        cummin(c(working.chest.data$WEIGHT, rep(0, n.max-n.chest)))+
        cummin(c(working.hands.data$WEIGHT, rep(0, n.max-n.hands)))+
        cummin(c(working.legs.data$WEIGHT, rep(0, n.max-n.legs)))
    if(motf.index == 999){
        weight.check <- ((weight.check+cummin(c(working.head.data$WEIGHT, rep(0, n.max-n.head)))) <= (-unarmored.weight+load.threshold+1e-10)) 
    } else{
        weight.check <- 
            ((weight.check+cummin(c(working.head.data$WEIGHT, rep(0, n.max-n.head)))) <= (-unarmored.weight+load.threshold+1e-10)) |
            c(rep(FALSE, motf.index-1), ((weight.check[motf.index:n.max]+1.2) <= (-unarmored.weight+load.threshold.motf+1e-10)))
    }
    minima.check <- 
        pmin(
            cummax(c(working.head.data$DURABILITY, rep(0, n.max-n.head))),
            cummax(c(working.chest.data$DURABILITY, rep(0, n.max-n.chest))),
            cummax(c(working.hands.data$DURABILITY, rep(0, n.max-n.hands))),
            cummax(c(working.legs.data$DURABILITY, rep(0, n.max-n.legs)))
        ) >= (minima[12]-1e-10) 
    min.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "POISE", "BLEED_RES", "POIS_RES", "CURSE_RES")
    for(i in seq_along(min.cols)){
        minima.check <- 
            minima.check & 
            (
                (
                    cummax(c(working.head.data[[min.cols[i]]], rep(0, n.max-n.head)))+
                    cummax(c(working.chest.data[[min.cols[i]]], rep(0, n.max-n.chest)))+
                    cummax(c(working.hands.data[[min.cols[i]]], rep(0, n.max-n.hands)))+
                    cummax(c(working.legs.data[[min.cols[i]]], rep(0, n.max-n.legs)))
                ) >= (minima[i]-1e-10)
            ) 
    }
    init.size <- which(weight.check & minima.check)[1]

    ## If there are no allowable combos, return empty data
    if(is.na(init.size)){
        out$data[, SCORE := numeric(0)]
        out$data[, c("HEAD", "CHEST", "HANDS", "LEGS") := character(0)]
        out$data[, 
            c(
                "PHYS_DEF",
                "STRIKE_DEF",
                "SLASH_DEF",
                "THRUST_DEF",
                "MAG_DEF",
                "FIRE_DEF",
                "LITNG_DEF",
                "BLEED_RES",
                "POIS_RES",
                "CURSE_RES",
                "DURABILITY",
                "ARMOR_POISE",
                "TOTAL_POISE",
                "ARMOR_WEIGHT",
                "TOTAL_WEIGHT",
                "EQUIP_LOAD",
                "PCT_LOAD"
            ) := numeric(0)
        ]
        return(out)
    }

    ## Create table of optimal combos
    out$data <- 
        data.table::setDT(
            optimize_armor_combinations(
                init.size,
                max.table.size,
                unarmored.weight,
                0.1*floor(10*base.load),
                0.1*floor(10.5*base.load),
                load.threshold,
                load.threshold.motf,
                motf.index-1,
                wolf.ring,
                minima,
                working.head.data,
                working.chest.data,
                working.hands.data,
                working.legs.data
            )
        )

    rm(list = c("working.head.data", "working.chest.data", "working.hands.data", "working.legs.data", "working.mean.stddev.corr"))
    rm(list = c("base.load", "roll.mult", "load.threshold", "load.threshold.motf"))
    rm(list = c("score.means", "score.scalars", "score.cols"))
    rm(list = c("n.head", "n.chest", "n.hands", "n.legs", "n.max"))
    rm(list = c("weight.check", "minima.check", "min.cols", "init.size"))
    rm(list = c("motf.index"))
    gc()

    return(out)

}
