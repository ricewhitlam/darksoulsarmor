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

## On real curves, at several tolerances: every point stays within the tolerance of the simplified
## line, segments join up at curve points in weight order, and a larger tolerance never gives more
## segments
test_that("the simplified line stays within the tolerance, more loosely as it grows", {
    for(metric in c("SCORE", "POISE", "MAG_DEF")){
        curve <- get.armor.tradeoffs(metric = metric, weight.step = 0.1, endurance.level = 40, movement = "Fat")$data
        found <- curve[!is.na(BEST_VALUE)]
        range <- diff(range(found$BEST_VALUE))
        counts <- integer(0)
        for(tolerance in c(0, 0.01, 0.02, 0.05, 0.1, 0.25)){
            e <- get.tradeoff.efficiency(curve, tolerance = tolerance)
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

## With movement types (as in the app's stitched curves), each is simplified on its own - no segment
## spans two - points with no set are left out, and a flat region has slope 0
test_that("segments stay within one movement type", {
    curve <- data.table::data.table(
        ARMOR_WEIGHT_LIMIT = round(seq(0, 6, by = 0.1), 1),
        MOVEMENT_LIMIT = rep(c("Light", "Mid"), c(21, 40))
    )
    curve[, BEST_VALUE := ifelse(ARMOR_WEIGHT_LIMIT < 0.5, NA_real_, ifelse(ARMOR_WEIGHT_LIMIT <= 2, 3*ARMOR_WEIGHT_LIMIT, 6))]
    e <- get.tradeoff.efficiency(curve)
    expect_equal(e$data$MOVEMENT_LIMIT, c("Light", "Mid"))
    expect_equal(e$data$FROM, c(0.5, 2.1))
    expect_equal(e$data$TO, c(2, 6))
    expect_equal(e$data$SLOPE, c(3, 0))
    expect_equal(e$average.slope, (6 - 1.5)/(6 - 0.5))
    ## A movement type with a single point gets no segment
    curve[ARMOR_WEIGHT_LIMIT > 2, MOVEMENT_LIMIT := ifelse(ARMOR_WEIGHT_LIMIT == 6, "Fat", "Mid")]
    expect_equal(get.tradeoff.efficiency(curve)$data$MOVEMENT_LIMIT, c("Light", "Mid"))
})

test_that("get.tradeoff.efficiency validates its arguments", {
    expect_error(get.tradeoff.efficiency(data.frame(x = 1:3)), "curve")
    expect_error(get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = c(2, 1, 3), BEST_VALUE = 1:3)), "increasing")
    expect_error(get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = 1, BEST_VALUE = 1)), "two points")
    expect_error(get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = 1:4, BEST_VALUE = 1:4, MOVEMENT_LIMIT = c("Light", "Mid", "Light", "Mid"))), "one range")
    curve <- data.table::data.table(ARMOR_WEIGHT_LIMIT = 1:6, BEST_VALUE = c(1, 2, 3, 3, 4, 6))
    expect_error(get.tradeoff.efficiency(curve, tolerance = -0.1), "tolerance")
    expect_error(get.tradeoff.efficiency(curve, tolerance = c(0.1, 0.2)), "tolerance")
    expect_error(get.tradeoff.efficiency(curve, tolerance = NA_real_), "tolerance")
})
