

## Whether a piece's area requirement is satisfied by a set of completed areas. AREA_MATCH_TYPE
## and AREA_LIST each encode one or more "|"-separated clauses that are combined with OR; a
## clause is ALWAYS (no areas required), ANY (at least one of its ";"-separated areas required),
## or ALL (every one of its areas required). This is a closed, fully data-driven grammar - no
## armor piece needs more than this - so it replaces what used to be a per-piece R expression
## string evaluated via eval(parse()) on every call.
area.requirement.met <- function(match.type, area.list, completed){
    if(match.type == "ALWAYS"){
        return(TRUE)
    }
    types <- strsplit(match.type, "|", fixed = TRUE)[[1]]
    clauses <- strsplit(area.list, "|", fixed = TRUE)[[1]]
    any(mapply(function(type, clause){
        areas <- strsplit(clause, ";", fixed = TRUE)[[1]]
        if(type == "ANY") any(areas %in% completed) else all(areas %in% completed)
    }, types, clauses))
}

## Maps a named minima/weights vector (e.g. c(POISE = 30, PHYS_DEF = 50)) onto the positional
## order get.optimal.armor.combos works in (metric.names), in any order, filling omitted metrics
## with 0. Unnamed vectors are returned unchanged, so positional calls behave exactly as before;
## anything else that's wrong with the values is caught by the regular argument checks afterward.
expand.named.metrics <- function(x, metric.names, arg.name){
    if(!is.numeric(x) || is.null(names(x))){
        return(x)
    }
    x.names <- names(x)
    if(any(is.na(x.names) | x.names == "")){
        stop(sprintf("Invalid argument '%s': name every entry or none", arg.name))
    }
    if(anyDuplicated(x.names) > 0){
        stop(sprintf("Invalid argument '%s': duplicated names: %s", arg.name, paste(unique(x.names[duplicated(x.names)]), collapse = ", ")))
    }
    unknown <- setdiff(x.names, metric.names)
    if(length(unknown) > 0){
        stop(sprintf("Invalid argument '%s': unknown names: %s. Valid names are: %s", arg.name, paste(unknown, collapse = ", "), paste(metric.names, collapse = ", ")))
    }
    out <- rep(0, length(metric.names))
    out[match(x.names, metric.names)] <- unname(x)
    return(out)
}

#' @name get.optimal.armor.combos
#' 
#' @title Create a \code{data.table} of optimized Dark Souls armor combinations
#'
#' @description
#' Produces a table of optimized armor combinations in Dark Souls.
#' All relevant metrics are included, along with two score columns: \code{SCORE_RAW}, an
#' unbounded standardized score (the one the table is sorted on - see \code{vignette("scoring")}),
#' and \code{SCORE_QUALITY}, that score's exact rarity among every possible combination at the
#' selected upgrade levels (regardless of the other filters) - e.g. \code{"Top 1 in 40"} when 1 in
#' every 40 such combinations scores at least as well, or \code{"Bottom 1 in 40"} when 1 in every
#' 40 scores at most as well.
#' Combinations with equal scores are ordered lighter first, then more poise, then more durability.
#' Stats are exact (what the game computes, in 32-bit floating point), not rounded to the one
#' decimal place the game displays. For example, Black Sorcerer Hat's strike defense is 5.1499996,
#' which the game shows as 5.1.
#' The table can be tailored to satisfy various constraints.
#' 
#' @param
#' max.table.size A length 1 \code{numeric} indicating how large the produced table should be.
#' Defaults to \code{1000}. Passed values will be clamped between 1 and 100,000,000 and cast to an integer.
#' 
#' @param 
#' starting.class A length 1 \code{character} indicating the character's starting class, whose
#' starting armor set is treated as available regardless of \code{areas.completed}.
#' Defaults to \code{"Warrior"}. The vector of available classes is stored as \code{classes}.
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
#' Defaults to all pieces. A vector of available pieces can be accessed via \code{head.data.unupgraded$ARMOR}.
#' If an empty vector is passed, it is assumed that no head armor will be worn.
#'
#' @param
#' chest.filter Same as \code{head.filter} but for chest armor pieces.
#' A vector of available pieces can be accessed via \code{chest.data.unupgraded$ARMOR}.
#'
#' @param
#' hands.filter Same as \code{head.filter} but for hand armor pieces.
#' A vector of available pieces can be accessed via \code{hands.data.unupgraded$ARMOR}.
#'
#' @param
#' legs.filter Same as \code{head.filter} but for leg armor pieces.
#' A vector of available pieces can be accessed via \code{legs.data.unupgraded$ARMOR}.
#' 
#' @param 
#' regular.level A length 1 \code{character} indicating the upgrade level of armor pieces ascended via regular titanite. 
#' Options are \code{"+0"} thru \code{"+10"}. 
#' Each defense and resistance is computed from the piece's +0 value with the game's own upgrade rates, exactly as the game computes it.
#' Defaults to \code{"+0"}.
#' 
#' @param 
#' twinkling.level A length 1 \code{character} indicating the upgrade level of armor pieces ascended via twinkling titanite. 
#' Options are \code{"+0"} thru \code{"+5"}. 
#' Each defense and resistance is computed from the piece's +0 value with the game's own upgrade rates, exactly as the game computes it.
#' Defaults to \code{"+0"}.
#' 
#' @param 
#' movement A length 1 \code{character}: the heaviest movement type to allow, by the share of max equip load carried.
#' Options are \code{"Light"} (weight at or below 25\% of max equip load: light roll), \code{"Mid"} (at or below 50\%: mid roll), \code{"Fat"} (at or below 100\%: fat roll), and \code{"Poop"} (no equip-load limit - over 100\%, the character can't roll and walks slowly).
#' Defaults to \code{"Light"}.
#' 
#' @param 
#' weapons A \code{character} vector of the weapons equipped in the four weapon slots, by name (see
#' \code{\link{weapon.data}}), or \code{"None"} for an empty slot. Either named by slot, in any
#' order, leaving out empty slots - \code{left.1}, \code{right.1}, \code{left.2} and \code{right.2}
#' for each hand's first and second slot, e.g. \code{c(left.1 = "Grass Crest Shield", right.1 = "Longsword")}
#' - or an unnamed length 4 vector in exactly that order. Rings and ammunition weigh nothing, so
#' these are everything the character carries besides armor. The game adds the four slots' weights
#' in that order, one at a time in 32-bit floating point, then the armor's, which makes the total
#' depend on which slot holds which weapon (with two or more talismans), so they're taken per slot
#' rather than as a single weight. Only their weight is used.
#' Defaults to every slot empty, as does an empty vector (\code{character(0)}).
#' 
#' @param 
#' endurance.level A length 1 \code{numeric} indicating the level of the character in the Endurance stat. 
#' This stat increases maximum equip load. Defaults to \code{10}. 
#' Passed values are clamped between 0 and 99 and rounded to an integer.
#' 
#' @param 
#' havel.ring A length 1 \code{logical} indicating whether Havel's Ring is equipped. 
#' This ring increases maximum equip load by 50\%. Defaults to \code{FALSE}.
#' 
#' @param 
#' favor.ring A length 1 \code{logical} indicating whether the Ring of Favor and Protection is equipped.
#' This ring increases maximum equip load by 20\%. Defaults to \code{FALSE}.
#' 
#' @param 
#' wolf.ring A length 1 \code{logical} indicating whether the Wolf Ring is equipped. 
#' This ring increases Poise by 40. Defaults to \code{FALSE}.
#' 
#' @param 
#' minima A \code{numeric} indicating minimum allowable values for metrics. Either a named vector
#' with any of the names PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF, LITNG_DEF,
#' POISE, BLEED_RES, POIS_RES, CURSE_RES, DURABILITY, in any order (omitted metrics have no minimum,
#' i.e. 0) - e.g. \code{c(POISE = 30, DURABILITY = 200)} - or an unnamed length 12 vector in
#' exactly that order.
#' Defaults to \code{c(0,0,0,0,0,0,0,0,0,0,0,0)}.
#' Passed values are clamped between 0 and 999.
#' 
#' @param 
#' weights A \code{numeric} indicating weights for the scored metrics. Either a named vector with any
#' of the names PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF, LITNG_DEF, BLEED_RES,
#' POIS_RES, CURSE_RES, in any order - e.g. \code{c(PHYS_DEF = 2, MAG_DEF = 1)} - or an unnamed
#' length 10 vector in exactly that order. Note that with a named vector, omitted metrics get weight
#' 0 (they don't count toward the score at all) rather than their default weights.
#' Defaults to \code{c(0.16,0.16,0.16,0.16,0.08,0.08,0.08,0.04,0.04,0.04)}.
#' These weights are used in the calculation of a score. This score is then optimized across all possible armor combinations.
#' Increasing the weight on a metric increases its importance to the final score.
#' The weights do not need to sum to 1, but should all be nonnegative with positive sum.
#' 
#' @return
#' A \code{list} holding (1) the list of arguments which defined the table and (2) a \code{data.table} of optimal armor combinations
#'
#' @examples
#' optimal.armor.combos <- get.optimal.armor.combos(endurance.level = 40, weapons = c(left.1 = "Grass Crest Shield", right.1 = "Longsword"), favor.ring = TRUE, movement = "Light")
#'
#' ## At least 30 poise, scored only on physical and magic defense (physical counting double)
#' poise.combos <- get.optimal.armor.combos(endurance.level = 40, movement = "Mid", minima = c(POISE = 30), weights = c(PHYS_DEF = 2, MAG_DEF = 1))
#'
get.optimal.armor.combos <- function(
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
        areas.completed <- unique(areas.completed)
    }

    ## Check head.filter
    if(is.null(head.filter)){
        head.filter <- character(0)
    }
    if(!is.character(head.filter)){
        stop("Invalid argument 'head.filter'")
    } else if(any(is.na(head.filter))){
        stop("Invalid argument 'head.filter'")
    } else if(length(head.filter) != 0){
        if(!all(head.filter %in% head.data.unupgraded$ARMOR)){
            stop("Invalid argument 'head.filter'")
        }
        head.filter <- unique(head.filter)
    } else{
        head.filter <- "No Head"
    }

    ## Check chest.filter
    if(is.null(chest.filter)){
        chest.filter <- character(0)
    }
    if(!is.character(chest.filter)){
        stop("Invalid argument 'chest.filter'")
    } else if(any(is.na(chest.filter))){
        stop("Invalid argument 'chest.filter'")
    } else if(length(chest.filter) != 0){
        if(!all(chest.filter %in% chest.data.unupgraded$ARMOR)){
            stop("Invalid argument 'chest.filter'")
        }
        chest.filter <- unique(chest.filter)
    } else{
        chest.filter <- "No Chest"
    }
    
    ## Check hands.filter
    if(is.null(hands.filter)){
        hands.filter <- character(0)
    }
    if(!is.character(hands.filter)){
        stop("Invalid argument 'hands.filter'")
    } else if(any(is.na(hands.filter))){
        stop("Invalid argument 'hands.filter'")
    } else if(length(hands.filter) != 0){
        if(!all(hands.filter %in% hands.data.unupgraded$ARMOR)){
            stop("Invalid argument 'hands.filter'")
        }
        hands.filter <- unique(hands.filter)
    } else{
        hands.filter <- "No Hands"
    }

    ## Check legs.filter
    if(is.null(legs.filter)){
        legs.filter <- character(0)
    }
    if(!is.character(legs.filter)){
        stop("Invalid argument 'legs.filter'")
    } else if(any(is.na(legs.filter))){
        stop("Invalid argument 'legs.filter'")
    } else if(length(legs.filter) != 0){
        if(!all(legs.filter %in% legs.data.unupgraded$ARMOR)){
            stop("Invalid argument 'legs.filter'")
        }
        legs.filter <- unique(legs.filter)
    } else{
        legs.filter <- "No Legs"
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

    ## Check weapons: one weapon name (or "None") per weapon slot, named by slot (in any order, empty
    ## slots left out) or unnamed in the game's slot order. Validated to all four slots, by name.
    if(!is.character(weapons) || anyNA(weapons)){
        stop("Invalid argument 'weapons'")
    }
    if(length(weapons) == 0){
        ## No weapons named: every slot empty
        weapons <- c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None")
    } else if(is.null(names(weapons))){
        if(length(weapons) != 4){
            stop(sprintf("Invalid argument 'weapons': name the slots, or give all four in the order %s", paste(WEAPON.SLOTS, collapse = ", ")))
        }
        names(weapons) <- WEAPON.SLOTS
    } else{
        if(any(names(weapons) == "")){
            stop("Invalid argument 'weapons': name every entry or none")
        }
        if(anyDuplicated(names(weapons)) > 0){
            stop(sprintf("Invalid argument 'weapons': duplicated slots: %s", paste(unique(names(weapons)[duplicated(names(weapons))]), collapse = ", ")))
        }
        unknown.slots <- setdiff(names(weapons), WEAPON.SLOTS)
        if(length(unknown.slots) > 0){
            stop(sprintf("Invalid argument 'weapons': unknown slots: %s. Valid slots are: %s", paste(unknown.slots, collapse = ", "), paste(WEAPON.SLOTS, collapse = ", ")))
        }
        all.slots <- c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None")
        all.slots[names(weapons)] <- weapons
        weapons <- all.slots
    }
    unknown.weapons <- setdiff(weapons, c("None", weapon.data$WEAPON))
    if(length(unknown.weapons) > 0){
        stop(sprintf("Invalid argument 'weapons': unknown weapons: %s. weapon.data$WEAPON lists every weapon", paste(unknown.weapons, collapse = ", ")))
    }
    weapons <- weapons[WEAPON.SLOTS]

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

    ## Check favor.ring
    if(!is.logical(favor.ring)){
        stop("Invalid argument 'favor.ring'")
    } else if(length(favor.ring) != 1){
        stop("Invalid argument 'favor.ring'")
    } else if(is.na(favor.ring)){
        stop("Invalid argument 'favor.ring'")
    }

    ## Check movement
    if(!is.character(movement)){
        stop("Invalid argument 'movement'")
    } else if(length(movement) != 1){
        stop("Invalid argument 'movement'")
    } else if(is.na(movement)){
        stop("Invalid argument 'movement'")
    } else if(!(movement %in% c("Light", "Mid", "Fat", "Poop"))){
        stop("Invalid argument 'movement'")
    }
    
    ## Check minima
    minima <- expand.named.metrics(minima, METRICS[order(minima.index), metric], "minima")
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
    weights <- expand.named.metrics(weights, METRICS[!is.na(weight.index)][order(weight.index), metric], "weights")
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
                    movement = movement,
                    weapons = weapons,
                    endurance.level = endurance.level,
                    havel.ring = havel.ring,
                    favor.ring = favor.ring,
                    wolf.ring = wolf.ring,
                    minima = minima,
                    weights = weights
                ),
            data = data.table::data.table()
        )

    return(find.armor.combos(out$args))

}

## The search itself, given arguments get.optimal.armor.combos has already validated (its `args`).
## Ranks combinations by score, or - for get.armor.tradeoffs - by a single summed stat
## (rank.metric: POISE or one defense/resistance), optionally under a limit on the armor's listed
## weight (armor.cap, which get.armor.tradeoffs sets at each point of its curves). Whatever it ranks
## by, the output reports each combination's real SCORE_RAW and SCORE_QUALITY - except for one point
## of a trade-off curve (curve.point = TRUE), which leaves SCORE_QUALITY and garbage collection to
## the curve as a whole. Split in two so a curve can prepare once and run once per weight limit: see
## prepare.armor.search and run.armor.search below.
find.armor.combos <- function(args, rank.metric = "SCORE", armor.cap = Inf, curve.point = FALSE){
    return(run.armor.search(prepare.armor.search(args, rank.metric), armor.cap, curve.point))
}

## Everything about a search that doesn't depend on the gear weight: each slot's pieces at the
## upgrade levels, scored (or ranked by rank.metric), filtered by piece, upgrade type, area and
## starting class, and sorted by that ranking. Also every piece's score at the level, unfiltered,
## for SCORE_QUALITY. Sorting before run.armor.search's weight filter gives the same tables in the
## same order as filtering first: the sort is stable, and dropping rows from a sorted table leaves
## the rest in order.
prepare.armor.search <- function(args, rank.metric = "SCORE"){

    starting.class <- args$starting.class
    areas.completed <- args$areas.completed
    upgrade.types <- args$upgrade.types
    head.filter <- args$head.filter
    chest.filter <- args$chest.filter
    hands.filter <- args$hands.filter
    legs.filter <- args$legs.filter
    regular.level <- args$regular.level
    twinkling.level <- args$twinkling.level
    weights <- args$weights

    ## Get data at specified upgrade levels
    working.head.data <- get.interp.data(head.data.unupgraded, as.numeric(regular.level), as.numeric(twinkling.level))
    working.chest.data <- get.interp.data(chest.data.unupgraded, as.numeric(regular.level), as.numeric(twinkling.level))
    working.hands.data <- get.interp.data(hands.data.unupgraded, as.numeric(regular.level), as.numeric(twinkling.level))
    working.legs.data <- get.interp.data(legs.data.unupgraded, as.numeric(regular.level), as.numeric(twinkling.level))

    ## Calc scores for each dataset (sorted on further below, once filtered)
    score.scalars <- (weights)/(stddevs*sqrt((t(weights) %*% corrs %*% weights)[1, 1]))
    scored.metrics <- METRICS[!is.na(weight.index)][order(weight.index)]
    metric.cols <- scored.metrics$metric

    ## Accumulated in plain vectors rather than as 40 data.table SCORE := ... calls: the same
    ## arithmetic in the same order (so bit-identical scores), without each call's overhead
    head.score <- rep(0, nrow(working.head.data))
    chest.score <- rep(0, nrow(working.chest.data))
    hands.score <- rep(0, nrow(working.hands.data))
    legs.score <- rep(0, nrow(working.legs.data))
    for(i in seq_along(metric.cols)){
        head.score <- head.score+score.scalars[i]*(working.head.data[[metric.cols[i]]]-0.25*means[i])
        chest.score <- chest.score+score.scalars[i]*(working.chest.data[[metric.cols[i]]]-0.25*means[i])
        hands.score <- hands.score+score.scalars[i]*(working.hands.data[[metric.cols[i]]]-0.25*means[i])
        legs.score <- legs.score+score.scalars[i]*(working.legs.data[[metric.cols[i]]]-0.25*means[i])
    }
    data.table::set(working.head.data, j = "SCORE", value = unname(head.score))
    data.table::set(working.chest.data, j = "SCORE", value = unname(chest.score))
    data.table::set(working.hands.data, j = "SCORE", value = unname(hands.score))
    data.table::set(working.legs.data, j = "SCORE", value = unname(legs.score))
    rm(list = c("head.score", "chest.score", "hands.score", "legs.score"))

    ## Every piece's score at this upgrade level, kept from before any filtering below:
    ## SCORE_QUALITY ranks each result against every combination at this level (see
    ## score.quality() in R/score-quality.R), not just the ones the filters allow.
    level.head.scores <- stats::setNames(working.head.data$SCORE, working.head.data$ARMOR)
    level.chest.scores <- stats::setNames(working.chest.data$SCORE, working.chest.data$ARMOR)
    level.hands.scores <- stats::setNames(working.hands.data$SCORE, working.hands.data$ARMOR)
    level.legs.scores <- stats::setNames(working.legs.data$SCORE, working.legs.data$ARMOR)

    ## Ranking by a single stat instead of the score: the search only needs each piece's value in
    ## SCORE (any stat summed across the four slots works the same way), so swap that stat in -
    ## plus the piece's score times a factor small enough never to outweigh a real difference in
    ## the stat (Poise totals differ by at least 1, defenses/resistances by at least 0.1, and a
    ## combination's score is within about +-15), so that among combinations tied on the stat, the
    ## best-scoring one wins.
    if(rank.metric != "SCORE"){
        tie.break.factor <- if(rank.metric == "POISE") 1e-4 else 1e-5
        working.head.data[, SCORE := get(rank.metric)+tie.break.factor*SCORE]
        working.chest.data[, SCORE := get(rank.metric)+tie.break.factor*SCORE]
        working.hands.data[, SCORE := get(rank.metric)+tie.break.factor*SCORE]
        working.legs.data[, SCORE := get(rank.metric)+tie.break.factor*SCORE]
    }

    ## Filter datasets based on inputs (all but weight - see run.armor.search)
    working.head.data[, AREAFILTER := mapply(area.requirement.met, AREA_MATCH_TYPE, AREA_LIST, MoreArgs = list(completed = areas.completed))]
    working.head.data <-
        working.head.data[
            (ARMOR %in% head.filter) &
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Head") &
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class)
        ]

    working.chest.data[, AREAFILTER := mapply(area.requirement.met, AREA_MATCH_TYPE, AREA_LIST, MoreArgs = list(completed = areas.completed))]
    working.chest.data <-
        working.chest.data[
            (ARMOR %in% chest.filter) &
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Chest") &
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class)
        ]

    working.hands.data[, AREAFILTER := mapply(area.requirement.met, AREA_MATCH_TYPE, AREA_LIST, MoreArgs = list(completed = areas.completed))]
    working.hands.data <-
        working.hands.data[
            (ARMOR %in% hands.filter) &
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Hands") &
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class)
        ]

    working.legs.data[, AREAFILTER := mapply(area.requirement.met, AREA_MATCH_TYPE, AREA_LIST, MoreArgs = list(completed = areas.completed))]
    working.legs.data <-
        working.legs.data[
            (ARMOR %in% legs.filter) &
            (UPGRADE_TYPE %in% upgrade.types | ARMOR == "No Legs") &
            (AREAFILTER == TRUE | STARTING_CLASS == starting.class)
        ]

    ## Remove unneeded columns
    working.head.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_MATCH_TYPE", "AREA_LIST", "AREAFILTER") := NULL]
    working.chest.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_MATCH_TYPE", "AREA_LIST", "AREAFILTER") := NULL]
    working.hands.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_MATCH_TYPE", "AREA_LIST", "AREAFILTER") := NULL]
    working.legs.data[, c("UPGRADE_TYPE", "STARTING_CLASS", "AREA_MATCH_TYPE", "AREA_LIST", "AREAFILTER") := NULL]

    ## Sort each dataset on its scores
    data.table::setorder(working.head.data, -SCORE, WEIGHT)
    data.table::setorder(working.chest.data, -SCORE, WEIGHT)
    data.table::setorder(working.hands.data, -SCORE, WEIGHT)
    data.table::setorder(working.legs.data, -SCORE, WEIGHT)

    return(
        list(
            args = args,
            rank.metric = rank.metric,
            head = working.head.data, chest = working.chest.data, hands = working.hands.data, legs = working.legs.data,
            level.scores = list(level.head.scores, level.chest.scores, level.hands.scores, level.legs.scores),
            metric.cols = metric.cols,
            scored.metrics = scored.metrics
        )
    )

}

## The part of a search that depends on weight: the per-piece weight limits, then the C++ search over
## the prepared tables (see prepare.armor.search), and the output's scores. armor.cap limits the
## armor's listed weight (the sum of its pieces' WEIGHT) - get.armor.tradeoffs' armor-weight limits.
run.armor.search <- function(prepared, armor.cap = Inf, curve.point = FALSE){

    args <- prepared$args
    max.table.size <- args$max.table.size
    movement <- args$movement
    endurance.level <- args$endurance.level
    havel.ring <- args$havel.ring
    favor.ring <- args$favor.ring
    wolf.ring <- args$wolf.ring
    minima <- args$minima
    metric.cols <- prepared$metric.cols
    scored.metrics <- prepared$scored.metrics
    level.head.scores <- prepared$level.scores[[1]]
    level.chest.scores <- prepared$level.scores[[2]]
    level.hands.scores <- prepared$level.scores[[3]]
    level.legs.scores <- prepared$level.scores[[4]]
    out <- list(args = args, data = data.table::data.table())

    ## The game's equip-load rule (R/equip-load.R): what the weapons weigh, and the equip load and the
    ## movement type's line, without and with the Mask of the Father (Poop's line is Inf)
    carried <- weapons.weight(args$weapons)
    load <- equip.load(endurance.level, havel.ring, favor.ring, father.mask = FALSE)
    load.father.mask <- equip.load(endurance.level, havel.ring, favor.ring, father.mask = TRUE)
    line <- movement.line(load, movement)
    line.father.mask <- movement.line(load.father.mask, movement)
    ## Room left for armor, for the pre-filters and the starting shell below. Those work on the listed
    ## weights, so they get a margin far larger than any 32-bit rounding: they may let through a set
    ## that doesn't fit, which the search's exact check (in the C++) then rejects, but never exclude
    ## one that does
    room <- min(line-carried, armor.cap)+1e-3
    room.father.mask <- min(line.father.mask-carried, armor.cap)+1e-3
    ## Mask of the Father's own weight, for "with the Mask on, how much is left for the other slots"
    ## below. Taken from the unfiltered table (weight doesn't change with upgrade level), so it's
    ## defined even when the Mask is filtered out - the pre-filters then just use a looser bound.
    father.mask.weight <- head.data.unupgraded[ARMOR == "Mask of the Father", WEIGHT]

    ## Pieces too heavy to fit even with every other slot empty (subsetting keeps the prepared order,
    ## and copies, so the prepared tables are untouched for the next run)
    working.head.data <- prepared$head[WEIGHT <= data.table::fifelse(ARMOR == "Mask of the Father", room.father.mask, room)]
    working.chest.data <- prepared$chest[WEIGHT <= max(room, room.father.mask-father.mask.weight)]
    working.hands.data <- prepared$hands[WEIGHT <= max(room, room.father.mask-father.mask.weight)]
    working.legs.data <- prepared$legs[WEIGHT <= max(room, room.father.mask-father.mask.weight)]

    ## If any tables empty, return empty data
    n.head <- nrow(working.head.data)
    n.chest<- nrow(working.chest.data)
    n.hands <- nrow(working.hands.data)
    n.legs <- nrow(working.legs.data)
    if(n.head == 0 || n.chest == 0 || n.hands == 0 || n.legs == 0){
        out$data[, "SCORE_RAW" := numeric(0)]
        out$data[, "SCORE_QUALITY" := character(0)]
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
                "POISE_TIMER",
                "ARMOR_WEIGHT",
                "TOTAL_WEIGHT",
                "EQUIP_LOAD",
                "PCT_LOAD"
            ) := numeric(0)
        ]
        return(out)
    }
  
    ## Determine position of the Mask of the Father in head data to apply its equip load bonus.
    ## NO_FATHER_MASK_INDEX (999) means it isn't present in the filtered head data at all.
    ## Compares only the ARMOR column, not the whole table: which(data.table == x) compares every
    ## cell and returns a linear index into the flattened, column-major layout rather than a row
    ## index - it would only coincidentally equal the row number for as long as ARMOR happens to
    ## stay column 1.
    NO_FATHER_MASK_INDEX <- 999
    father.mask.index <- which(working.head.data$ARMOR == "Mask of the Father")
    if(length(father.mask.index) == 0){
        father.mask.index <- NO_FATHER_MASK_INDEX
    }

    ## Identify initial looping info based on allowable weight and minima
    n.max <- max(n.head, n.chest, n.hands, n.legs)
    weight.check <- 
        cummin(c(working.chest.data$WEIGHT, rep(0, n.max-n.chest)))+
        cummin(c(working.hands.data$WEIGHT, rep(0, n.max-n.hands)))+
        cummin(c(working.legs.data$WEIGHT, rep(0, n.max-n.legs)))
    if(father.mask.index == NO_FATHER_MASK_INDEX){
        weight.check <- ((weight.check+cummin(c(working.head.data$WEIGHT, rep(0, n.max-n.head)))) <= room)
    } else{
        weight.check <-
            ((weight.check+cummin(c(working.head.data$WEIGHT, rep(0, n.max-n.head)))) <= room) |
            c(rep(FALSE, father.mask.index-1), ((weight.check[father.mask.index:n.max]+father.mask.weight) <= room.father.mask))
    }
    minima.check <- 
        pmin(
            cummax(c(working.head.data$DURABILITY, rep(0, n.max-n.head))),
            cummax(c(working.chest.data$DURABILITY, rep(0, n.max-n.chest))),
            cummax(c(working.hands.data$DURABILITY, rep(0, n.max-n.hands))),
            cummax(c(working.legs.data$DURABILITY, rep(0, n.max-n.legs)))
        ) >= (minima[METRICS[metric == "DURABILITY", minima.index]]-1e-10)
    minima.check <-
        minima.check &
        (
            (
                cummax(c(working.head.data$POISE, rep(0, n.max-n.head)))+
                cummax(c(working.chest.data$POISE, rep(0, n.max-n.chest)))+
                cummax(c(working.hands.data$POISE, rep(0, n.max-n.hands)))+
                cummax(c(working.legs.data$POISE, rep(0, n.max-n.legs)))
            ) >= (minima[METRICS[metric == "POISE", minima.index]]-ifelse(wolf.ring, 40, 0)-1e-10)
        )
    for(i in seq_along(metric.cols)){
        minima.check <-
            minima.check &
            (
                (
                    cummax(c(working.head.data[[metric.cols[i]]], rep(0, n.max-n.head)))+
                    cummax(c(working.chest.data[[metric.cols[i]]], rep(0, n.max-n.chest)))+
                    cummax(c(working.hands.data[[metric.cols[i]]], rep(0, n.max-n.hands)))+
                    cummax(c(working.legs.data[[metric.cols[i]]], rep(0, n.max-n.legs)))
                ) >= (minima[scored.metrics$minima.index[i]]-1e-10)
            )
    }
    init.size <- which(weight.check & minima.check)[1]

    ## If there are no allowable combos, return empty data
    if(is.na(init.size)){
        out$data[, "SCORE_RAW" := numeric(0)]
        out$data[, "SCORE_QUALITY" := character(0)]
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
                "POISE_TIMER",
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
            optimal_armor_combinations(
                init.size,
                max.table.size,
                carried,
                load,
                load.father.mask,
                line,
                line.father.mask,
                armor.cap,
                father.mask.index-1,
                wolf.ring,
                minima,
                working.head.data,
                working.chest.data,
                working.hands.data,
                working.legs.data
            )
        )

    ## When ranked by another stat, the search's SCORE_RAW holds that stat's total - report the
    ## combinations' real scores instead (each piece's score, summed in the search's own order)
    if(prepared$rank.metric != "SCORE"){
        out$data[, SCORE_RAW := unname(level.head.scores[HEAD]+level.chest.scores[CHEST]+level.hands.scores[HANDS]+level.legs.scores[LEGS])]
    }

    ## SCORE_QUALITY: each result's exact rarity among every combination at this upgrade level
    ## ("Top 1 in N" / "Bottom 1 in N") - see score.quality() in R/score-quality.R. One point of
    ## a get.armor.tradeoffs curve leaves it to the caller instead, which computes it once for all
    ## its points from the prepared per-piece scores.
    if(curve.point){
        out$data[, SCORE_QUALITY := NA_character_]
    } else{
        out$data[, SCORE_QUALITY := score.quality(SCORE_RAW, level.head.scores, level.chest.scores, level.hands.scores, level.legs.scores)]
    }
    data.table::setcolorder(out$data, c("SCORE_RAW", "SCORE_QUALITY"))

    rm(list = c("working.head.data", "working.chest.data", "working.hands.data", "working.legs.data"))
    rm(list = c("carried", "load", "load.father.mask", "line", "line.father.mask", "room", "room.father.mask", "father.mask.weight"))
    rm(list = c("scored.metrics", "metric.cols", "prepared"))
    rm(list = c("level.head.scores", "level.chest.scores", "level.hands.scores", "level.legs.scores"))
    rm(list = c("n.head", "n.chest", "n.hands", "n.legs", "n.max"))
    rm(list = c("weight.check", "minima.check", "init.size"))
    rm(list = c("father.mask.index", "NO_FATHER_MASK_INDEX"))
    ## A get.armor.tradeoffs curve runs many searches in a row and collects once at its end
    if(!curve.point){
        gc()
    }

    return(out)

}

