

#' @name get.tradeoff.efficiency
#'
#' @title Find where extra armor weight buys the most of a stat
#'
#' @description
#' Simplifies a trade-off curve (from \code{\link{get.armor.tradeoffs}}) into a few straight-line
#' segments, each with a slope: how much of the stat each extra unit of armor weight buys across that
#' region. Comparing a segment's slope with the curve's average slope (its total gain divided by its
#' total weight) shows where weight is best spent: regions steeper than average give more for the
#' weight.
#'
#' The simplification is Douglas-Peucker's: starting from the straight line between the curve's first
#' and last points, the point furthest from the line (vertically) is kept if it's further than the
#' tolerance, and each side is simplified the same way, until every point is within the tolerance of
#' the simplified line. Segments join at points of the curve - each a best set at its weight limit -
#' so a slope is a rate between real sets; you can't wear the weights in between. Points with no set
#' (\code{NA}) are left out.
#'
#' @param
#' curve A curve from \code{\link{get.armor.tradeoffs}} (or its \code{data}): a table with
#' \code{ARMOR_WEIGHT_LIMIT} and \code{BEST_VALUE}. Any other columns are ignored.
#'
#' @param
#' tolerance A length 1 non-negative \code{numeric}: how far the simplified line may stray from the
#' curve, as a share of the curve's range of values. Larger values give fewer, broader segments; 0
#' keeps every bend. Defaults to \code{0.05} (5\%).
#'
#' @return
#' A \code{list} holding (1) \code{average.slope}, the curve's total gain divided by its total
#' weight, and (2) a \code{data.table} with one row per segment, in weight order: the weights it
#' spans (\code{FROM}, \code{TO}), the curve's values there (\code{START_VALUE}, \code{END_VALUE}),
#' its \code{SLOPE}, that slope as a multiple of the average (\code{RATIO_TO_AVERAGE}, \code{NA} when
#' the average is 0), and whether it's steeper than average (\code{ABOVE_AVERAGE}).
#'
#' @examples
#' score.curve <- get.armor.tradeoffs(weight.step = 0.1, endurance.level = 40, movement = "Fat")
#' efficiency <- get.tradeoff.efficiency(score.curve)
#'
get.tradeoff.efficiency <- function(curve, tolerance = 0.05){

    ## Check curve
    if(is.list(curve) && !is.data.frame(curve)){
        curve <- curve$data
    }
    if(!is.data.frame(curve) || !all(c("ARMOR_WEIGHT_LIMIT", "BEST_VALUE") %in% names(curve))){
        stop("Invalid argument 'curve': needs ARMOR_WEIGHT_LIMIT and BEST_VALUE - a get.armor.tradeoffs() result")
    }
    found <- !is.na(curve$BEST_VALUE)
    x <- curve$ARMOR_WEIGHT_LIMIT[found]
    y <- curve$BEST_VALUE[found]
    if(length(x) < 2){
        stop("Invalid argument 'curve': fewer than two points with a set")
    }
    if(is.unsorted(x, strictly = TRUE)){
        stop("Invalid argument 'curve': ARMOR_WEIGHT_LIMIT must be in increasing order")
    }

    ## Check tolerance
    if(!is.numeric(tolerance) || length(tolerance) != 1 || !is.finite(tolerance) || tolerance < 0){
        stop("Invalid argument 'tolerance'")
    }
    allowed <- tolerance*(max(y)-min(y))

    ## Simplify, then one segment between each pair of kept points
    kept <- douglas.peucker(x, y, allowed)
    from <- kept[-length(kept)]
    to <- kept[-1]
    out <- data.table::data.table(
        FROM = x[from], TO = x[to], START_VALUE = y[from], END_VALUE = y[to],
        SLOPE = (y[to]-y[from])/(x[to]-x[from])
    )

    ## Each slope against the curve's average: its total gain over its total weight
    n <- length(x)
    average.slope <- (y[n]-y[1])/(x[n]-x[1])
    out[, RATIO_TO_AVERAGE := if(average.slope != 0) SLOPE/average.slope else NA_real_]
    out[, ABOVE_AVERAGE := SLOPE > average.slope]

    return(list(average.slope = average.slope, data = out))

}

## Douglas-Peucker simplification of the points x/y (x increasing): the indices kept, so that every
## point is within `allowed` (vertically) of the straight lines joining the kept points. Works through
## a stack of ranges still to check rather than recursing.
douglas.peucker <- function(x, y, allowed){
    kept <- c(1L, length(x))
    pending <- list(c(1L, length(x)))
    while(length(pending) > 0){
        range <- pending[[length(pending)]]
        pending[[length(pending)]] <- NULL
        i <- range[1]; j <- range[2]
        if(j-i < 2){
            next
        }
        between <- (i+1):(j-1)
        distance <- abs(y[between]-(y[i]+(y[j]-y[i])*(x[between]-x[i])/(x[j]-x[i])))
        if(max(distance) > allowed){
            furthest <- between[which.max(distance)]
            kept <- c(kept, furthest)
            pending[[length(pending)+1]] <- c(i, furthest)
            pending[[length(pending)+1]] <- c(furthest, j)
        }
    }
    return(sort(unique(kept)))
}
