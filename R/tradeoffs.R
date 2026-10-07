

#' @name get.armor.tradeoffs
#'
#' @title Find the best achievable value of a stat at each armor weight
#'
#' @description
#' For a range of armor-weight limits, finds the combination with the most of one stat - the score,
#' Poise, or a single defense or resistance - that weighs no more than each limit, with every other
#' setting (filters, upgrade levels, rings, minima, ...) as given. This traces the trade-off between
#' armor weight and that stat: e.g. how much more Poise each extra unit of armor weight can buy, or
#' the lightest armor that reaches a Poise breakpoint.
#'
#' The weight limit is varied the way the game varies it, by the weight carried besides armor. So
#' the Mask of the Father's equip load bonus applies at every limit (except with
#' \code{movement = "Poop"}, which has no load limit), and a combination wearing it can weigh
#' slightly more than the limit itself. Among combinations tied on the stat, the best-scoring one is
#' returned.
#'
#' @param
#' metric A length 1 \code{character}: the stat to maximize. \code{"SCORE"} (the score built from
#' \code{weights}, as in \code{\link{get.optimal.armor.combos}}), \code{"POISE"} (including the
#' Wolf Ring's bonus if \code{wolf.ring = TRUE}), or one of \code{"PHYS_DEF"}, \code{"STRIKE_DEF"},
#' \code{"SLASH_DEF"}, \code{"THRUST_DEF"}, \code{"MAG_DEF"}, \code{"FIRE_DEF"},
#' \code{"LITNG_DEF"}, \code{"BLEED_RES"}, \code{"POIS_RES"}, \code{"CURSE_RES"}.
#' Defaults to \code{"SCORE"}.
#'
#' @param
#' weight.step A length 1 positive \code{numeric}: the spacing of the armor-weight limits. Limits
#' are the multiples of this size between \code{min.armor.weight} and \code{max.armor.weight},
#' plus those two limits themselves. Armor weights are multiples of 0.1, so steps below 0.1 add
#' points but no detail. Defaults to \code{1}.
#'
#' @param
#' min.armor.weight A length 1 non-negative \code{numeric}: the smallest armor-weight limit, no
#' larger than \code{max.armor.weight}. Defaults to \code{0}.
#'
#' @param
#' max.armor.weight A length 1 non-negative \code{numeric}, or \code{NULL}: the largest armor-weight
#' limit. \code{NULL} (the default) uses the armor weight the other settings currently allow - the
#' movement type's share of the equip load, less \code{unarmored.weight} - or, with
#' \code{movement = "Poop"}, the heaviest possible armor (with no load limit, the Mask of the Father's
#' equip load bonus doesn't apply).
#'
#' @param
#' ... Any other arguments of \code{\link{get.optimal.armor.combos}} (except \code{max.table.size}),
#' with the same meaning and defaults.
#'
#' @return
#' A \code{list} holding (1) the arguments that defined the curve and (2) a \code{data.table} with
#' one row per armor-weight limit (\code{ARMOR_WEIGHT_LIMIT}): the best achievable value of the
#' metric (\code{BEST_VALUE}), and the combination achieving it - its \code{SCORE_RAW},
#' \code{SCORE_QUALITY}, \code{ARMOR_WEIGHT}, \code{TOTAL_POISE}, and pieces. Limits at which no
#' combination satisfies the other settings have \code{NA} values. Values are exact, as in
#' \code{\link{get.optimal.armor.combos}}.
#'
#' @examples
#' poise.curve <- get.armor.tradeoffs(metric = "POISE", endurance.level = 40, movement = "Mid", unarmored.weight = 12)
#'
get.armor.tradeoffs <- function(metric = "SCORE", weight.step = 1, min.armor.weight = 0, max.armor.weight = NULL, ...){

    metric.options <- c("SCORE", "POISE", METRICS[!is.na(weight.index)][order(weight.index), metric])

    ## Check metric
    if(!is.character(metric)){
        stop("Invalid argument 'metric'")
    } else if(length(metric) != 1){
        stop("Invalid argument 'metric'")
    } else if(is.na(metric)){
        stop("Invalid argument 'metric'")
    } else if(!(metric %in% metric.options)){
        stop(sprintf("Invalid argument 'metric': must be one of %s", paste(metric.options, collapse = ", ")))
    }

    ## Check weight.step
    if(!is.numeric(weight.step)){
        stop("Invalid argument 'weight.step'")
    } else if(length(weight.step) != 1){
        stop("Invalid argument 'weight.step'")
    } else if(!is.finite(weight.step) || weight.step <= 0){
        stop("Invalid argument 'weight.step'")
    }

    ## Check max.armor.weight
    if(!is.null(max.armor.weight)){
        if(!is.numeric(max.armor.weight)){
            stop("Invalid argument 'max.armor.weight'")
        } else if(length(max.armor.weight) != 1){
            stop("Invalid argument 'max.armor.weight'")
        } else if(!is.finite(max.armor.weight) || max.armor.weight < 0){
            stop("Invalid argument 'max.armor.weight'")
        }
    }

    ## Check min.armor.weight (against max.armor.weight once that's known, below)
    if(!is.numeric(min.armor.weight)){
        stop("Invalid argument 'min.armor.weight'")
    } else if(length(min.armor.weight) != 1){
        stop("Invalid argument 'min.armor.weight'")
    } else if(!is.finite(min.armor.weight) || min.armor.weight < 0){
        stop("Invalid argument 'min.armor.weight'")
    }

    ## Every other argument is validated by get.optimal.armor.combos itself, exactly as it would be
    ## there; only one result is needed per weight limit
    search.args <- list(...)
    if("max.table.size" %in% names(search.args)){
        stop("Invalid argument 'max.table.size': get.armor.tradeoffs always finds the single best combination per weight limit")
    }
    search.args$max.table.size <- 1
    args <- do.call(get.optimal.armor.combos, search.args)$args

    ## The weight limit is varied through the weight carried besides armor: at a given armor-weight
    ## limit, that's the movement type's share of the equip load less the limit
    load.threshold <- (args$endurance.level+40)*ifelse(args$havel.ring, 1.5, 1)*ifelse(args$favor.ring, 1.2, 1)*c(0.25, 0.5, 1.0, 999.0)[match(args$movement, c("Light", "Mid", "Fat", "Poop"))]
    if(is.null(max.armor.weight)){
        heaviest.armor <- max(head.data.unupgraded$WEIGHT)+max(chest.data.unupgraded$WEIGHT)+max(hands.data.unupgraded$WEIGHT)+max(legs.data.unupgraded$WEIGHT)
        max.armor.weight <- max(0, min(load.threshold-args$unarmored.weight, heaviest.armor))
    }
    if(min.armor.weight > max.armor.weight){
        stop(sprintf("Invalid argument 'min.armor.weight': larger than max.armor.weight (%s)", format(max.armor.weight)))
    }
    ## The multiples of weight.step in range, plus both ends (a common grid, so curves over adjacent
    ## ranges line up)
    first.multiple <- ceiling(round(min.armor.weight/weight.step, 9))*weight.step
    multiples <- if(first.multiple <= max.armor.weight) seq(first.multiple, max.armor.weight, by = weight.step) else numeric(0)
    limits <- sort(unique(round(c(min.armor.weight, multiples, max.armor.weight), 9)), decreasing = TRUE)

    out <- list(args = c(list(metric = metric, weight.step = weight.step, min.armor.weight = min.armor.weight, max.armor.weight = max.armor.weight), args), data = data.table::data.table())

    ## From the largest limit down. The best combination at a limit stays the best at every lower
    ## limit it still fits under (lowering the limit only removes combinations), so it's reused down
    ## to its own weight - or, wearing the Mask of the Father, to its weight less the Mask's 5% load
    ## bonus - and only limits below that need a new search. Once nothing fits, nothing fits lower.
    father.mask.bonus <- if(args$movement == "Poop") 0 else 0.05*load.threshold
    ## Everything about the search that doesn't depend on the weight limit is done once, and only
    ## the weight-dependent part runs at each limit
    prepared <- prepare.armor.search(args, rank.metric = metric)
    level.scores <- prepared$level.scores
    rows <- list()
    best <- NULL
    reuse.down.to <- Inf
    for(limit in limits){
        if(limit < reuse.down.to-1e-9){
            best <- run.armor.search(prepared, gear.weight = load.threshold-limit, curve.point = TRUE)$data
            if(nrow(best) == 0){
                break
            }
            reuse.down.to <- best$ARMOR_WEIGHT-ifelse(best$HEAD == "Mask of the Father", father.mask.bonus, 0)
        }
        rows[[length(rows)+1]] <- data.table::data.table(ARMOR_WEIGHT_LIMIT = limit, best[, .(SCORE_RAW, SCORE_QUALITY, ARMOR_WEIGHT, TOTAL_POISE, HEAD, CHEST, HANDS, LEGS)])
    }
    ## Limits below the lightest feasible combination: no result
    missing <- setdiff(limits, vapply(rows, function(r){ r$ARMOR_WEIGHT_LIMIT }, numeric(1)))
    if(length(missing) > 0){
        rows[[length(rows)+1]] <-
            data.table::data.table(
                ARMOR_WEIGHT_LIMIT = missing, SCORE_RAW = NA_real_, SCORE_QUALITY = NA_character_, ARMOR_WEIGHT = NA_real_, TOTAL_POISE = NA_real_,
                HEAD = NA_character_, CHEST = NA_character_, HANDS = NA_character_, LEGS = NA_character_
            )
    }
    curve <- data.table::rbindlist(rows)
    ## SCORE_QUALITY for every point at once (the searches left it out)
    if(!is.null(level.scores)){
        found <- !is.na(curve$SCORE_RAW)
        curve[found, SCORE_QUALITY := score.quality(SCORE_RAW, level.scores[[1]], level.scores[[2]], level.scores[[3]], level.scores[[4]])]
    }
    if(metric == "SCORE"){
        curve[, BEST_VALUE := SCORE_RAW]
    } else if(metric == "POISE"){
        curve[, BEST_VALUE := TOTAL_POISE]
    } else{
        ## A single defense/resistance: its total for each chosen combination
        curve[, BEST_VALUE := metric.total(metric, HEAD, CHEST, HANDS, LEGS, args$regular.level, args$twinkling.level)]
    }
    data.table::setorder(curve, ARMOR_WEIGHT_LIMIT)
    data.table::setcolorder(curve, c("ARMOR_WEIGHT_LIMIT", "BEST_VALUE"))
    out$data <- curve

    rm(list = c("rows", "best", "curve", "level.scores", "prepared"))
    gc()

    return(out)

}

## A single defense/resistance's total for the given combinations (NA pieces give NA), at the given
## upgrade levels
metric.total <- function(metric, head, chest, hands, legs, regular.level, twinkling.level){
    level.value <- function(unupgraded){
        d <- get.interp.data(unupgraded, as.numeric(regular.level), as.numeric(twinkling.level))
        stats::setNames(d[[metric]], d$ARMOR)
    }
    unname(
        level.value(head.data.unupgraded)[head]+
        level.value(chest.data.unupgraded)[chest]+
        level.value(hands.data.unupgraded)[hands]+
        level.value(legs.data.unupgraded)[legs]
    )
}
