

#' @name get.tradeoff.efficiency
#'
#' @title Describe where extra armor weight pays off
#'
#' @description
#' Describes a trade-off curve (from \code{\link{get.armor.tradeoffs}}) as a sequence of pieces, so
#' that each step of the curve belongs to exactly one of them:
#' \itemize{
#'   \item \strong{jumps}: single steps where a little more weight buys a lot;
#'   \item \strong{lines}: regions with a steady rate, compared with the curve's average rate (its
#'   total gain divided by its total weight) - above it, extra weight buys more than average;
#'   \item \strong{flats}: regions where extra weight buys nothing;
#' }
#' with a \strong{boundary point} wherever two regions (lines or flats) meet.
#'
#' Points with no set (\code{NA}) are left out. The range is the curve's highest value less its
#' lowest, and the width its last weight less its first.
#'
#' \strong{Jumps.} A step is a jump if it rises at least a share of the range, and is worth at least
#' an amount of weight at the rate around it: its rise divided by the rate of the rest of the gain
#' within 3 weight either side of it, leaving out any jumps already chosen. So a big step in a steep
#' stretch, whose neighbours rise as fast, isn't a jump; the stretch is a steep line. Jumps are
#' chosen biggest first, up to a maximum; equal steps are taken together or not at all (a group that
#' would go past the maximum is left out, along with everything smaller).
#'
#' \strong{Flats.} A run of steps with no gain at least a share of the width long, longest first, up
#' to a maximum, equal lengths taken together or not at all.
#'
#' \strong{Lines.} Each stretch between the jumps and flats is fitted with the best 1, 2, 3 or 4
#' least-squares lines, each at least 2.5 weight wide, adding a line only if it at least halves the
#' error (root mean square). A line's rate is its gain from the set before it to its last set,
#' divided by the weight between them. A line with no gain is a flat.
#'
#' \strong{Boundary points.} Between two lines, the break moves to the biggest step within 1.25 weight
#' of where the fit put it, which becomes the last step of the earlier line; from a line into a flat,
#' it's the last step before the flat; from a flat into a line, the first step after it. A single step
#' between two flats is itself the boundary point between them.
#'
#' \strong{Promoted jumps.} While fewer jumps than the maximum are in use, a boundary point's step
#' becomes a jump if it's among the curve's largest steps (as many as the maximum, equal ones together),
#' rises at least half the share of the range a jump otherwise needs, and is worth as much weight;
#' biggest first, the curve described again around each.
#'
#' @param
#' curve A curve from \code{\link{get.armor.tradeoffs}} (or its \code{data}): a table with
#' \code{ARMOR_WEIGHT_LIMIT} and \code{BEST_VALUE}. Any other columns are ignored.
#'
#' @param
#' jumps How many jumps to look for: \code{"few"} (at most 3, each rising at least 10\% of the range
#' and worth at least 1.5 weight), \code{"some"} (at most 6, 5\%, 1.25 weight) or \code{"many"} (at
#' most 8, 3\%, 0.75 weight). Defaults to \code{"some"}.
#'
#' @param
#' flats How many flats to look for: \code{"few"} (at most 2, each at least 15\% of the width),
#' \code{"some"} (at most 3, 10\%) or \code{"many"} (at most 5, 5\%). Defaults to \code{"some"}.
#'
#' @return
#' A \code{list} holding (1) \code{average.slope}, the curve's total gain divided by its total
#' weight, and (2) a \code{data.table} with one row per piece, in weight order: its \code{TYPE}
#' (\code{"jump"}, \code{"line"}, \code{"flat"} or \code{"boundary"}); the weights it runs between
#' (\code{FROM}, \code{TO}: from the set before it to its last set, so that jumps, lines and flats
#' join end to end, a jump or boundary point being one step) and the curve's values there
#' (\code{START_VALUE}, \code{END_VALUE}), with their difference (\code{GAIN}); for lines and flats,
#' the rate as a multiple of the average (\code{RATIO_TO_AVERAGE}, \code{NA} when the average is 0)
#' and whether it's above average (\code{ABOVE_AVERAGE}); and for boundary points, the rates before
#' and after (\code{RATIO_BEFORE}, \code{RATIO_AFTER}). A boundary point's step also belongs to a
#' line beside it, except for one between two flats.
#'
#' @examples
#' score.curve <- get.armor.tradeoffs(weight.step = 0.1, endurance.level = 40, movement = "Fat")
#' efficiency <- get.tradeoff.efficiency(score.curve)
#' fewer.jumps <- get.tradeoff.efficiency(score.curve, jumps = "few")
#'
get.tradeoff.efficiency <- function(curve, jumps = "some", flats = "some"){

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

    ## Check jumps and flats
    if(!is.character(jumps) || length(jumps) != 1 || !(jumps %in% names(JUMP.LEVELS))){
        stop("Invalid argument 'jumps': \"few\", \"some\" or \"many\"")
    }
    if(!is.character(flats) || length(flats) != 1 || !(flats %in% names(FLAT.LEVELS))){
        stop("Invalid argument 'flats': \"few\", \"some\" or \"many\"")
    }
    jump.level <- JUMP.LEVELS[[jumps]]
    flat.level <- FLAT.LEVELS[[flats]]

    ## The curve's measures: each step's rise (step i is from point i-1 to point i), as a share of the
    ## range too, and the average rate
    n <- length(x)
    rise <- c(0, diff(y))
    range <- max(y)-min(y)
    of.range <- if(range > 0) rise/range else rep(0, n)
    average.slope <- (y[n]-y[1])/(x[n]-x[1])

    ## How much weight step i is worth at the rate around it: the rate of the gain within the window,
    ## less step i's own and any jump's already chosen
    is.jump <- rep(FALSE, n)
    worth <- function(i){
        near <- which(abs(x-x[i]) <= WORTH.WINDOW+1e-9)
        lo <- min(near)
        hi <- max(near)
        jumps.near <- near[near > lo & near != i & is.jump[near]]
        rate <- (y[hi]-y[lo]-rise[i]-sum(rise[jumps.near]))/(x[hi]-x[lo])
        if(rate > 1e-12) rise[i]/rate else Inf
    }

    ## Jumps, biggest first: each group of equal steps (those worth enough) taken together, or not at
    ## all if it would go past the maximum
    rising <- which(rise > 1e-9)
    candidates <- rising[of.range[rising] >= jump.level$size-1e-12]
    candidates <- candidates[order(-rise[candidates])]
    at <- 1
    while(at <= length(candidates)){
        g <- at
        while(g < length(candidates) && abs(rise[candidates[g+1]]-rise[candidates[at]]) <= 1e-9*max(1, rise[candidates[at]])){
            g <- g+1
        }
        group <- candidates[at:g]
        group <- group[vapply(group, worth, numeric(1)) >= jump.level$worth-1e-9]
        if(sum(is.jump)+length(group) > jump.level$max){
            break
        }
        is.jump[group] <- TRUE
        at <- g+1
    }

    ## Flats: of the runs of steps with no gain (their first and last points), the longest
    flat.end <- rep(NA_integer_, n)
    no.gain <- c(FALSE, abs(rise[-1]) < 1e-9)
    runs <- rle(no.gain)
    run.last <- cumsum(runs$lengths)
    run.first <- run.last-runs$lengths+1
    run.first <- run.first[runs$values]
    run.last <- run.last[runs$values]
    run.length <- round(x[run.last]-x[run.first-1], 6)
    long <- which(run.length >= flat.level$width*(x[n]-x[1])-1e-9)
    chosen <- long[largest(run.length[long], flat.level$max)]
    flat.end[run.first[chosen]] <- run.last[chosen]

    ## Describe the curve, then promote boundary points while there's room for more jumps
    pieces <- describe.curve(x, y, rise, is.jump, flat.end)
    top <- rising[largest(rise[rising], jump.level$max)]
    while(sum(is.jump) < jump.level$max){
        promotable <- vapply(pieces, function(p){
            p$type == "boundary" && p$at %in% top && !is.jump[p$at] &&
                of.range[p$at] >= jump.level$size/2-1e-12 && worth(p$at) >= jump.level$worth-1e-9
        }, logical(1))
        if(!any(promotable)){
            break
        }
        steps <- vapply(pieces[promotable], function(p) rise[p$at], numeric(1))
        is.jump[pieces[promotable][[which.max(steps)]]$at] <- TRUE
        pieces <- describe.curve(x, y, rise, is.jump, flat.end)
    }

    ## One row per piece; rates as multiples of the average
    ratio <- function(p){
        if(p$type == "flat") return(0)
        if(average.slope == 0) return(NA_real_)
        (y[p$last]-y[p$base])/(x[p$last]-x[p$base])/average.slope
    }
    out <- data.table::data.table(
        TYPE = vapply(pieces, function(p) p$type, character(1)),
        FROM = vapply(pieces, function(p) x[p$base], numeric(1)),
        TO = vapply(pieces, function(p) x[p$last], numeric(1)),
        START_VALUE = vapply(pieces, function(p) y[p$base], numeric(1)),
        END_VALUE = vapply(pieces, function(p) y[p$last], numeric(1)),
        GAIN = vapply(pieces, function(p) y[p$last]-y[p$base], numeric(1)),
        RATIO_TO_AVERAGE = vapply(pieces, function(p) if(p$type %in% c("line", "flat")) ratio(p) else NA_real_, numeric(1)),
        ABOVE_AVERAGE = vapply(pieces, function(p) if(p$type %in% c("line", "flat")) ratio(p) > 1 else NA, logical(1)),
        RATIO_BEFORE = vapply(pieces, function(p) if(p$type == "boundary") ratio(p$before) else NA_real_, numeric(1)),
        RATIO_AFTER = vapply(pieces, function(p) if(p$type == "boundary") ratio(p$after) else NA_real_, numeric(1))
    )

    return(list(average.slope = average.slope, data = out))

}

## The levels of get.tradeoff.efficiency()'s jumps and flats arguments: at most how many, how big
## (a jump's rise as a share of the range, a flat's length as a share of the width), and how much
## weight a jump must be worth at the rate around it
JUMP.LEVELS <- list(
    few = list(max = 3, size = 0.10, worth = 1.5),
    some = list(max = 6, size = 0.05, worth = 1.25),
    many = list(max = 8, size = 0.03, worth = 0.75)
)
FLAT.LEVELS <- list(
    few = list(max = 2, width = 0.15),
    some = list(max = 3, width = 0.10),
    many = list(max = 5, width = 0.05)
)

## The fixed rules: the weight either side of a step its worth is judged over; at most how many lines
## a stretch is fitted with, how much each added line must cut the error (to at most this share), and
## how narrow a line can be; and how far a boundary point between two lines may move to a big step
WORTH.WINDOW <- 3
MAX.LINES <- 4
LINE.CUT <- 0.5
LINE.WIDTH <- 2.5
BOUNDARY.WINDOW <- 1.25

## The indices of the k biggest values, in the order taken: equal values (within rounding) together,
## and a group that would go past k left out, along with everything smaller
largest <- function(values, k){
    o <- order(-values)
    out <- integer(0)
    at <- 1
    while(at <= length(o)){
        g <- at
        while(g < length(o) && abs(values[o[g+1]]-values[o[at]]) <= 1e-9*max(1, abs(values[o[at]]))){
            g <- g+1
        }
        if(length(out)+(g-at+1) > k){
            break
        }
        out <- c(out, o[at:g])
        at <- g+1
    }
    return(out)
}

## The pieces of the curve x/y (rise: each point's step from the one before) around the given jumps
## (is.jump: at each point, whether its step is a jump) and flats (flat.end: at a flat's first point,
## its last), in weight order. Each piece is a list: its type, its first and last points, and its base
## (the point before its first, where its gain is measured from); a boundary point also has the point
## its step ends at and the pieces before and after it.
describe.curve <- function(x, y, rise, is.jump, flat.end){

    n <- length(x)

    ## Jumps, flats, and lines fitted to each stretch between them
    pieces <- list()
    i <- 1
    while(i <= n){
        j <- i
        if(is.jump[i]){
            pieces[[length(pieces)+1]] <- list(type = "jump", first = i, last = i, base = i-1)
        } else if(!is.na(flat.end[i])){
            j <- flat.end[i]
            pieces[[length(pieces)+1]] <- list(type = "flat", first = i, last = j, base = i-1)
        } else {
            while(j < n && !is.jump[j+1] && is.na(flat.end[j+1])){
                j <- j+1
            }
            a <- if(i == 1) 1 else i-1
            kept <- c(a, j)
            if(j-a >= 2){
                kept <- fewest.lines(x[a:j], y[a:j])+a-1
            }
            for(s in seq_len(length(kept)-1)){
                f <- kept[s]
                t <- kept[s+1]
                if(t == f){
                    next
                }
                first <- if(s == 1) i else f+1
                type <- if(abs(y[t]-y[f]) < 1e-9) "flat" else "line"
                pieces[[length(pieces)+1]] <- list(type = type, first = first, last = t, base = f)
            }
        }
        i <- j+1
    }

    ## Neighbouring flats as one
    merged <- list()
    for(p in pieces){
        k <- length(merged)
        if(k > 0 && merged[[k]]$type == "flat" && p$type == "flat"){
            merged[[k]]$last <- p$last
        } else {
            merged[[k+1]] <- p
        }
    }

    ## Boundary points between neighbouring regions; a single step between two flats is one itself
    lone <- function(k){
        k >= 2 && k < length(merged) && merged[[k]]$type == "line" && merged[[k]]$first == merged[[k]]$last &&
            merged[[k-1]]$type == "flat" && merged[[k+1]]$type == "flat"
    }
    refit <- function(p){
        p$type <- if(abs(y[p$last]-y[p$base]) < 1e-9) "flat" else "line"
        return(p)
    }
    out <- list()
    bounds <- list()
    for(k in seq_along(merged)){
        if(lone(k)){
            p <- merged[[k]]
            out[[length(out)+1]] <- list(type = "boundary", first = p$first, last = p$last, base = p$base, at = p$last)
            bounds[[length(bounds)+1]] <- c(length(out), k-1, k+1)
            next
        }
        q.k <- k+1
        if(q.k <= length(merged) && merged[[k]]$type != "jump" && merged[[q.k]]$type != "jump" && !lone(q.k)){
            p <- merged[[k]]
            q <- merged[[q.k]]
            if(p$type == "line" && q$type == "line"){
                ## The biggest step within the window of the break, as the earlier line's last
                near <- if(p$first+1 <= q$last-1) (p$first+1):(q$last-1) else integer(0)
                near <- near[abs(x[near]-x[p$last]) <= BOUNDARY.WINDOW+1e-9 & rise[near] > 1e-9]
                if(length(near) > 0){
                    best <- near[which.max(rise[near])]
                    if(best != p$last){
                        p$last <- best
                        q$first <- best+1
                        q$base <- best
                        merged[[k]] <- refit(p)
                        merged[[q.k]] <- refit(q)
                    }
                }
                at <- merged[[k]]$last
            } else {
                at <- if(q$type == "line") q$first else p$last
            }
            out[[length(out)+1]] <- merged[[k]]
            out[[length(out)+1]] <- list(type = "boundary", first = at, last = at, base = at-1, at = at)
            bounds[[length(bounds)+1]] <- c(length(out), k, q.k)
        } else {
            out[[length(out)+1]] <- merged[[k]]
        }
    }
    ## Each boundary point's pieces either side, as they finally are
    for(b in bounds){
        out[[b[1]]]$before <- merged[[b[2]]]
        out[[b[1]]]$after <- merged[[b[3]]]
    }

    return(out)

}

## The fewest least-squares lines (up to MAX.LINES, each at least LINE.WIDTH wide) for the points
## x/y, where each extra line must bring the error (root mean square) down to at most LINE.CUT of what
## it was: the points kept, the first and last and the last point of each line but the last
fewest.lines <- function(x, y){
    m <- length(x)
    sx <- c(0, cumsum(x))
    sy <- c(0, cumsum(y))
    sxx <- c(0, cumsum(x*x))
    sxy <- c(0, cumsum(x*y))
    syy <- c(0, cumsum(y*y))
    ## The squared error of the least-squares line through points i..j (i a vector)
    cost <- function(i, j){
        k <- j-i+1
        X <- sx[j+1]-sx[i]
        Y <- sy[j+1]-sy[i]
        vx <- sxx[j+1]-sxx[i]-X*X/k
        vy <- syy[j+1]-syy[i]-Y*Y/k
        cxy <- sxy[j+1]-sxy[i]-X*Y/k
        pmax(0, ifelse(vx > 1e-12, vy-cxy*cxy/pmax(vx, 1e-12), vy))
    }
    ## F[k, j]: the least squared error of k lines over points 1..j; B[k, j]: where the last starts
    F <- matrix(Inf, MAX.LINES, m)
    B <- matrix(NA_integer_, MAX.LINES, m)
    for(j in 1:m){
        if(x[j]-x[1] >= LINE.WIDTH-1e-9 || j == m){
            F[1, j] <- cost(1, j)
        }
    }
    for(k in seq_len(MAX.LINES)[-1]){
        for(j in seq_len(m)[-1]){
            i <- 2:j
            ok <- is.finite(F[k-1, i-1]) & x[j]-x[i] >= LINE.WIDTH-1e-9
            if(!any(ok)){
                next
            }
            i <- i[ok]
            v <- F[k-1, i-1]+cost(i, j)
            F[k, j] <- min(v)
            B[k, j] <- i[which.min(v)]
        }
    }
    ## As many lines as each halve the error (none more once it's nil)
    k <- 1
    nil <- 1e-12*(1+(max(y)-min(y))^2)*m
    while(k < MAX.LINES && F[k, m] > nil && is.finite(F[k+1, m]) && sqrt(F[k+1, m]) <= LINE.CUT*sqrt(F[k, m])){
        k <- k+1
    }
    kept <- m
    j <- m
    while(k > 1){
        i <- B[k, j]
        kept <- c(i-1, kept)
        j <- i-1
        k <- k-1
    }
    kept <- c(1L, kept)
    return(kept[c(TRUE, diff(kept) != 0)])
}
