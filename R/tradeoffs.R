

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
#' A limit applies to the armor's listed weight (its pieces' weights added up). Every combination
#' must also allow the movement type, checked as the game checks it - with the weapons, rings and
#' the Mask of the Father's equip load bonus, in 32-bit floating point - so near a movement type's
#' line, a combination within the limit can still be ruled out. Among combinations tied on the
#' stat, the best-scoring one is returned.
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
#' limit. \code{NULL} (the default) uses the most armor weight the other settings allow - the
#' movement type's line, with the Mask of the Father's bonus, less what the \code{weapons} weigh,
#' rounded down to 0.1 - or, with \code{movement = "Poop"}, the heaviest possible armor.
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
#' poise.curve <- get.armor.tradeoffs(metric = "POISE", endurance.level = 40, movement = "Mid", weapons = c(right.1 = "Great Club"))
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

    ## By default, up to the most armor weight the movement type could allow: its line (the Mask of
    ## the Father's, which is higher) less what the weapons weigh, on the 0.1 grid armor weights are
    ## listed on - or, with no limit, the heaviest armor there is
    if(is.null(max.armor.weight)){
        heaviest.armor <- max(head.data.unupgraded$WEIGHT)+max(chest.data.unupgraded$WEIGHT)+max(hands.data.unupgraded$WEIGHT)+max(legs.data.unupgraded$WEIGHT)
        line.father.mask <- movement.line(equip.load(args$endurance.level, args$havel.ring, args$favor.ring, father.mask = TRUE), args$movement)
        allowance <- line.father.mask-weapons.weight(args$weapons)
        max.armor.weight <- max(0, min(floor(10*allowance+1e-6)/10, heaviest.armor))
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
    ## limit it still fits under (lowering the limit only removes combinations, and the movement
    ## type's check doesn't depend on the limit), so it's reused down to its own listed weight, and
    ## only limits below that need a new search. Once nothing fits, nothing fits lower.
    ## Everything about the search that doesn't depend on the weight limit is done once, and only
    ## the weight-dependent part runs at each limit
    prepared <- prepare.armor.search(args, rank.metric = metric)
    level.scores <- prepared$level.scores
    rows <- list()
    best <- NULL
    reuse.down.to <- Inf
    for(limit in limits){
        if(limit < reuse.down.to-1e-9){
            best <- run.armor.search(prepared, armor.cap = limit, curve.point = TRUE)$data
            if(nrow(best) == 0){
                break
            }
            reuse.down.to <-
                head.data.unupgraded$WEIGHT[match(best$HEAD, head.data.unupgraded$ARMOR)]+
                chest.data.unupgraded$WEIGHT[match(best$CHEST, chest.data.unupgraded$ARMOR)]+
                hands.data.unupgraded$WEIGHT[match(best$HANDS, hands.data.unupgraded$ARMOR)]+
                legs.data.unupgraded$WEIGHT[match(best$LEGS, legs.data.unupgraded$ARMOR)]
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
