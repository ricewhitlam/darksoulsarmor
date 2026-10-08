## get.tradeoff.efficiency simplifies a trade-off curve into straight-line segments
## (Douglas-Peucker, within a tolerance) and compares each slope with the curve's average.

## The simplified line, as a function of weight, for checking how far the curve strays from it
simplified.at <- function(e, x){
    stats::approx(c(e$data$FROM, e$data$TO[nrow(e$data)]), c(e$data$START_VALUE, e$data$END_VALUE[nrow(e$data)]), xout = x)$y
}

## A curve made of two straight lines - slope 2 up to weight 4, then 0.5 - comes back as exactly those
## two segments, against an average slope of 11 / 10 = 1.1
test_that("a curve of two straight lines is recovered exactly", {
    x <- round(seq(0, 10, by = 0.1), 1)
    curve <- data.table::data.table(ARMOR_WEIGHT_LIMIT = x, BEST_VALUE = ifelse(x <= 4, 2*x, 8 + 0.5*(x - 4)))
    e <- get.tradeoff.efficiency(curve)
    expect_equal(e$average.slope, 1.1)
    expect_equal(e$data$FROM, c(0, 4))
    expect_equal(e$data$TO, c(4, 10))
    expect_equal(e$data$START_VALUE, c(0, 8))
    expect_equal(e$data$END_VALUE, c(8, 11))
    expect_equal(e$data$SLOPE, c(2, 0.5))
    expect_equal(e$data$RATIO_TO_AVERAGE, c(2, 0.5)/1.1)
    expect_equal(e$data$ABOVE_AVERAGE, c(TRUE, FALSE))
    ## The same curve given as a get.armor.tradeoffs() result
    expect_identical(get.tradeoff.efficiency(list(data = curve)), e)
})

## On real curves, at several tolerances (with no limit on segments or their width): every point
## stays within the tolerance of the simplified line, segments join up at curve points in weight
## order, and a larger tolerance never gives more segments
test_that("the simplified line stays within the tolerance, more loosely as it grows", {
    for(metric in c("SCORE", "POISE", "MAG_DEF")){
        curve <- get.armor.tradeoffs(metric = metric, weight.step = 0.1, endurance.level = 40, movement = "Fat")$data
        found <- curve[!is.na(BEST_VALUE)]
        range <- diff(range(found$BEST_VALUE))
        counts <- integer(0)
        for(tolerance in c(0, 0.01, 0.02, 0.05, 0.1, 0.25)){
            e <- get.tradeoff.efficiency(curve, tolerance = tolerance, max.segments = Inf, min.width = 0)
            info <- paste(metric, tolerance)
            expect_true(all(abs(simplified.at(e, found$ARMOR_WEIGHT_LIMIT) - found$BEST_VALUE) <= tolerance*range + 1e-9), info = info)
            expect_equal(e$data$FROM[-1], e$data$TO[-nrow(e$data)], info = info)
            expect_true(all(e$data$START_VALUE == found$BEST_VALUE[match(e$data$FROM, found$ARMOR_WEIGHT_LIMIT)]), info = info)
            expect_equal(e$data$ABOVE_AVERAGE, e$data$SLOPE > e$average.slope, info = info)
            counts <- c(counts, nrow(e$data))
        }
        expect_false(is.unsorted(rev(counts)), info = metric)
        expect_equal(e$average.slope, (found$BEST_VALUE[nrow(found)] - found$BEST_VALUE[1])/(found$ARMOR_WEIGHT_LIMIT[nrow(found)] - found$ARMOR_WEIGHT_LIMIT[1]))
    }
})

## Movement types (as in the app's stitched curves) don't split the curve: a straight stretch across
## a movement line is one segment. Points with no set are left out, and a flat region has slope 0.
test_that("segments ignore movement types", {
    curve <- data.table::data.table(
        ARMOR_WEIGHT_LIMIT = round(seq(0, 6, by = 0.1), 1),
        MOVEMENT_LIMIT = rep(c("Light", "Mid"), c(11, 50))
    )
    curve[, BEST_VALUE := ifelse(ARMOR_WEIGHT_LIMIT < 0.5, NA_real_, ifelse(ARMOR_WEIGHT_LIMIT <= 2, 3*ARMOR_WEIGHT_LIMIT, 6))]
    e <- get.tradeoff.efficiency(curve)
    expect_named(e$data, c("FROM", "TO", "START_VALUE", "END_VALUE", "SLOPE", "RATIO_TO_AVERAGE", "ABOVE_AVERAGE"))
    expect_equal(e$data$FROM, c(0.5, 2))
    expect_equal(e$data$TO, c(2, 6))
    expect_equal(e$data$SLOPE, c(3, 0))
    expect_equal(e$average.slope, (6 - 1.5)/(6 - 0.5))
})

## Capped, the curve is split into at most max.segments segments - the first ones it splits off at
## any tolerance, so a larger cap keeps every break of a smaller one - and a cap the tolerance
## doesn't reach changes nothing
test_that("max.segments caps the segments, keeping the biggest bends", {
    curve <- get.armor.tradeoffs(metric = "MAG_DEF", weight.step = 0.1, endurance.level = 40, movement = "Fat")$data
    uncapped <- get.tradeoff.efficiency(curve, max.segments = Inf, min.width = 0)
    expect_gt(nrow(uncapped$data), 6)
    breaks <- list()
    for(cap in 1:6){
        e <- get.tradeoff.efficiency(curve, max.segments = cap, min.width = 0)
        expect_equal(nrow(e$data), cap)
        breaks[[cap]] <- e$data$FROM[-1]
        expect_true(all(breaks[[cap]] %in% uncapped$data$FROM))
        if(cap > 1){
            expect_true(all(breaks[[cap - 1]] %in% breaks[[cap]]))
        }
    }
    expect_identical(get.tradeoff.efficiency(curve, max.segments = nrow(uncapped$data) + 1, min.width = 0), uncapped)
})

## No split leaves a segment narrower than min.width (a share of the curve's weights): a sharp
## jump in a curve, which unlimited becomes a sliver of its own, is folded into a wider segment
test_that("min.width keeps segments from being slivers", {
    x <- round(seq(0, 10, by = 0.1), 1)
    curve <- data.table::data.table(ARMOR_WEIGHT_LIMIT = x, BEST_VALUE = x + ifelse(x >= 5.1, 20, 0))
    sliver <- get.tradeoff.efficiency(curve, min.width = 0)
    expect_true(any(sliver$data$TO - sliver$data$FROM < 0.5 - 1e-9))
    e <- get.tradeoff.efficiency(curve)
    expect_true(all(e$data$TO - e$data$FROM >= 0.05*10 - 1e-9))
    ## On real curves, at several widths
    for(metric in c("SCORE", "MAG_DEF", "FIRE_DEF")){
        curve <- get.armor.tradeoffs(metric = metric, weight.step = 0.1, endurance.level = 40, movement = "Light")$data
        weights <- diff(range(curve$ARMOR_WEIGHT_LIMIT[!is.na(curve$BEST_VALUE)]))
        for(width in c(0.025, 0.05, 0.1)){
            e <- get.tradeoff.efficiency(curve, max.segments = Inf, min.width = width)
            expect_true(all(e$data$TO - e$data$FROM >= width*weights - 1e-9), info = paste(metric, width))
        }
    }
})

test_that("get.tradeoff.efficiency validates its arguments", {
    expect_error(get.tradeoff.efficiency(data.frame(x = 1:3)), "curve")
    expect_error(get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = c(2, 1, 3), BEST_VALUE = 1:3)), "increasing")
    expect_error(get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = 1, BEST_VALUE = 1)), "two points")
    curve <- data.table::data.table(ARMOR_WEIGHT_LIMIT = 1:6, BEST_VALUE = c(1, 2, 3, 3, 4, 6))
    expect_error(get.tradeoff.efficiency(curve, tolerance = -0.1), "tolerance")
    expect_error(get.tradeoff.efficiency(curve, tolerance = c(0.1, 0.2)), "tolerance")
    expect_error(get.tradeoff.efficiency(curve, tolerance = NA_real_), "tolerance")
    expect_error(get.tradeoff.efficiency(curve, max.segments = 0), "max.segments")
    expect_error(get.tradeoff.efficiency(curve, max.segments = 2.5), "max.segments")
    expect_error(get.tradeoff.efficiency(curve, max.segments = c(2, 3)), "max.segments")
    expect_error(get.tradeoff.efficiency(curve, max.segments = NA_real_), "max.segments")
    expect_error(get.tradeoff.efficiency(curve, min.width = -0.1), "min.width")
    expect_error(get.tradeoff.efficiency(curve, min.width = Inf), "min.width")
    expect_error(get.tradeoff.efficiency(curve, min.width = "a"), "min.width")
    expect_no_error(get.tradeoff.efficiency(curve, max.segments = Inf, min.width = 0))
})
