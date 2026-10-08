

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
#' The simplification is Douglas-Peucker's, taken worst first: starting from the straight line between
#' the curve's first and last points, the segment that strays furthest from the curve (vertically) is
#' split at the point where it strays most, again and again, until every point is within the
#' tolerance of the simplified line - or there are \code{max.segments} segments, which then hold the
#' curve's biggest bends. A segment is only split where both sides would be at least
#' \code{min.width} wide, so a sharp jump in the curve doesn't become a sliver of its own; a segment
#' that can't be split that way stays whole, even if it strays further than the tolerance. Segments
#' join at points of the curve - each a best set at its weight limit - so a slope is a rate between
#' real sets; you can't wear the weights in between. Points with no set (\code{NA}) are left out.
#'
#' @param
#' curve A curve from \code{\link{get.armor.tradeoffs}} (or its \code{data}): a table with
#' \code{ARMOR_WEIGHT_LIMIT} and \code{BEST_VALUE}. Any other columns are ignored.
#'
#' @param
#' tolerance A length 1 non-negative \code{numeric}: how far the simplified line may stray from the
#' curve, as a share of the curve's range of values. Larger values give fewer, broader segments; 0
#' keeps every bend (up to \code{max.segments}). Defaults to \code{0.05} (5\%).
#'
#' @param
#' max.segments A length 1 whole \code{numeric}, at least 1: the most segments to split the curve
#' into, whatever the tolerance. \code{Inf} for no limit. Defaults to \code{5}.
#'
#' @param
#' min.width A length 1 non-negative \code{numeric}: the narrowest a split may leave a segment, as a
#' share of the curve's range of weights. 0 for no limit. Defaults to \code{0.05} (5\%).
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
get.tradeoff.efficiency <- function(curve, tolerance = 0.05, max.segments = 5, min.width = 0.05){

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

    ## Check max.segments
    if(!is.numeric(max.segments) || length(max.segments) != 1 || is.na(max.segments) || max.segments < 1 || (is.finite(max.segments) && max.segments != round(max.segments))){
        stop("Invalid argument 'max.segments'")
    }

    ## Check min.width
    if(!is.numeric(min.width) || length(min.width) != 1 || !is.finite(min.width) || min.width < 0){
        stop("Invalid argument 'min.width'")
    }
    width <- min.width*(max(x)-min(x))

    ## Simplify, then one segment between each pair of kept points
    kept <- douglas.peucker(x, y, allowed, max.segments, width)
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

## Douglas-Peucker simplification of the points x/y (x increasing), worst first: the indices kept.
## Splits the segment that strays furthest (vertically) from its points, at the point it strays from
## most, until every point is within `allowed` of the straight lines joining the kept points or there
## are `max.segments` segments - only ever at a point leaving both sides at least `width` wide.
douglas.peucker <- function(x, y, allowed, max.segments, width){
    ## Where the segment from point i to point j would split - of the points leaving both sides at
    ## least `width` wide, the one furthest from its line - and how far that is (-1 if no point does)
    furthest <- function(i, j){
        between <- if(j-i >= 2) (i+1):(j-1) else integer(0)
        between <- between[x[between]-x[i] >= width-1e-9 & x[j]-x[between] >= width-1e-9]
        if(length(between) == 0){
            return(c(NA, -1))
        }
        distance <- abs(y[between]-(y[i]+(y[j]-y[i])*(x[between]-x[i])/(x[j]-x[i])))
        return(c(between[which.max(distance)], max(distance)))
    }
    ## The kept points, and for each segment between them, where it would split
    kept <- c(1L, length(x))
    splits <- list(furthest(1L, length(x)))
    while(length(kept)-1 < max.segments){
        distances <- vapply(splits, function(split) split[2], numeric(1))
        k <- which.max(distances)
        if(distances[k] <= allowed){
            break
        }
        at <- splits[[k]][1]
        kept <- append(kept, at, after = k)
        splits <- append(splits[-k], list(furthest(kept[k], at), furthest(at, kept[k+2])), after = k-1)
    }
    return(kept)
}
