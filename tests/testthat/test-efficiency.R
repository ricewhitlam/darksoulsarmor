## get.tradeoff.efficiency describes a trade-off curve as jumps, lines and flats, with boundary points
## between regions.

## The pieces in brief: each jump and boundary point where its step ends, each line and flat where it
## ends, with lines' and boundary points' rates as multiples of the average
described <- function(e){
    d <- e$data
    rate <- function(v) if(v == 0) "flat" else sprintf("%.2fx", v)
    paste(vapply(seq_len(nrow(d)), function(k){
        switch(d$TYPE[k],
            jump = sprintf("jump %.1f", d$TO[k]),
            boundary = sprintf("boundary %.1f %s>%s", d$TO[k], rate(d$RATIO_BEFORE[k]), rate(d$RATIO_AFTER[k])),
            flat = sprintf("flat to %.1f", d$TO[k]),
            line = sprintf("line to %.1f %.2fx", d$TO[k], d$RATIO_TO_AVERAGE[k]))
    }, character(1)), collapse = " | ")
}

## Curves over every armor weight (no movement limit), every 0.1
curve.over.all.weights <- function(...){
    get.armor.tradeoffs(..., weight.step = 0.1, movement = "Poop")
}
weights.16.12 <- c(PHYS_DEF = 16, STRIKE_DEF = 16, SLASH_DEF = 16, THRUST_DEF = 16, MAG_DEF = 12, FIRE_DEF = 12, LITNG_DEF = 12)

## Score with physical defenses at 16% and elemental at 12%, armor fully upgraded: with a poise
## minimum of 61, four jumps (one at 25.5, beside the bigger 25.1, as it stands out once 25.1 is set
## aside), lines between them, and the flat end; with none, a steep line, a shallow one and the flat
## end, with no jumps; with a minimum of 31, the jump at 21.0 promoted from the boundary between a
## steeper line and a shallower one, being among the curve's biggest steps
test_that("score curves are described with their jumps, lines and flats", {
    poise.61 <- curve.over.all.weights(metric = "SCORE", weights = weights.16.12, minima = c(POISE = 61), regular.level = "+10", twinkling.level = "+5")
    expect_equal(described(get.tradeoff.efficiency(poise.61)), paste(
        "line to 25.0 1.89x | jump 25.1 | line to 25.4 2.59x | jump 25.5 | line to 27.6 1.89x | jump 27.7 |",
        "line to 31.5 1.47x | jump 31.6 | line to 42.1 0.43x | boundary 42.1 0.43x>flat | flat to 52.5"))
    no.minimum <- curve.over.all.weights(metric = "SCORE", weights = weights.16.12, regular.level = "+10", twinkling.level = "+5")
    expect_equal(described(get.tradeoff.efficiency(no.minimum)),
        "line to 7.1 4.94x | boundary 7.1 4.94x>0.50x | line to 42.1 0.50x | boundary 42.1 0.50x>flat | flat to 52.5")
    poise.31 <- curve.over.all.weights(metric = "SCORE", weights = weights.16.12, minima = c(POISE = 31), regular.level = "+10", twinkling.level = "+5")
    expect_equal(described(get.tradeoff.efficiency(poise.31)), paste(
        "line to 13.4 1.18x | jump 13.5 | line to 15.0 6.55x | boundary 15.0 6.55x>1.82x | line to 20.9 1.82x | jump 21.0 |",
        "line to 31.6 0.63x | boundary 31.6 0.63x>0.31x | line to 42.1 0.31x | boundary 42.1 0.31x>flat | flat to 52.5"))
    ## Unupgraded, with the default weights: two lines and the flat end
    expect_equal(described(get.tradeoff.efficiency(curve.over.all.weights(metric = "SCORE"))),
        "line to 7.5 2.72x | boundary 7.5 2.72x>0.84x | line to 45.6 0.84x | boundary 45.6 0.84x>flat | flat to 52.5")
})

## Magic defense rises in big steps at first, but so close together that none is worth a whole unit of
## weight at the rate around it: a steep line, a less steep one, a flat, a shallow line, and the flat
## end. Fire defense (poise at least 31) climbs in steady steps of 8 or 9 to its one jump of 22 at 20.4,
## with its three long flats after; a single step between two flats is the boundary point between them.
test_that("steep staircases are lines, and single steps between flats are boundary points", {
    magic <- get.tradeoff.efficiency(curve.over.all.weights(metric = "MAG_DEF"))
    expect_equal(described(magic), paste(
        "line to 2.2 11.58x | boundary 2.2 11.58x>2.03x | line to 11.9 2.03x | boundary 11.9 2.03x>flat | flat to 17.9 |",
        "boundary 18.0 flat>0.39x | line to 36.9 0.39x | boundary 36.9 0.39x>flat | flat to 52.5"))
    fire <- get.tradeoff.efficiency(curve.over.all.weights(metric = "FIRE_DEF", minima = c(POISE = 31)))
    expect_equal(described(fire), paste(
        "line to 20.3 3.41x | jump 20.4 | line to 23.7 1.53x | boundary 23.7 1.53x>flat | flat to 32.3 |",
        "boundary 32.4 flat>flat | flat to 37.7 | boundary 37.8 flat>flat | flat to 52.5"))
    ## That boundary point owns its step (+6 from 32.3 to 32.4): the flats either side end and start there
    d <- fire$data
    k <- which(d$TYPE == "boundary" & d$TO == 32.4)
    expect_equal(c(d$FROM[k], d$TO[k], d$GAIN[k]), c(32.3, 32.4, 6))
    expect_equal(c(d$TO[k - 1], d$FROM[k + 1]), c(32.3, 32.4))
    expect_equal(c(d$RATIO_BEFORE[k], d$RATIO_AFTER[k]), c(0, 0))
})

## Lightning defense's lone +8 at 41.4 is too small a share of its range to be a jump, so 10.2 to 45.6
## is one shallow line. Curse resistance's big early steps are a steep staircase up to 5.0, and its +9
## steps after it too small to be jumps, so it's one steep line, one shallow line, and the jump of 15
## at 28.5 before the flat end.
test_that("small steps and steep staircases aren't jumps", {
    expect_equal(described(get.tradeoff.efficiency(curve.over.all.weights(metric = "LITNG_DEF"))),
        "line to 10.1 3.09x | boundary 10.1 3.09x>0.60x | line to 45.6 0.60x | boundary 45.6 0.60x>flat | flat to 52.5")
    expect_equal(described(get.tradeoff.efficiency(curve.over.all.weights(metric = "CURSE_RES"))),
        "line to 5.0 6.72x | boundary 5.0 6.72x>0.62x | line to 28.4 0.62x | jump 28.5 | flat to 52.5")
})

## Every step belongs to exactly one piece: the jumps, lines, flats and boundary points between two
## flats run end to end from the curve's first weight to its last, their gains adding up to the whole
test_that("the pieces cover the curve end to end", {
    for(metric in c("SCORE", "POISE", "MAG_DEF", "CURSE_RES")){
        curve <- curve.over.all.weights(metric = metric)$data
        found <- curve[!is.na(BEST_VALUE)]
        d <- get.tradeoff.efficiency(curve)$data
        between.flats <- d$TYPE == "boundary" & c(FALSE, d$TYPE[-nrow(d)] == "flat") & c(d$TYPE[-1] == "flat", FALSE)
        owning <- d[d$TYPE != "boundary" | between.flats]
        expect_equal(owning$FROM[1], found$ARMOR_WEIGHT_LIMIT[1], info = metric)
        expect_equal(owning$TO[nrow(owning)], found$ARMOR_WEIGHT_LIMIT[nrow(found)], info = metric)
        expect_equal(owning$FROM[-1], owning$TO[-nrow(owning)], info = metric)
        expect_equal(sum(owning$GAIN), found$BEST_VALUE[nrow(found)] - found$BEST_VALUE[1], info = metric)
        ## Lines and flats against the average; boundary points' rates are their neighbours'
        regions <- d[d$TYPE %in% c("line", "flat")]
        average <- (found$BEST_VALUE[nrow(found)] - found$BEST_VALUE[1])/(found$ARMOR_WEIGHT_LIMIT[nrow(found)] - found$ARMOR_WEIGHT_LIMIT[1])
        expect_equal(regions$RATIO_TO_AVERAGE, regions$GAIN/(regions$TO - regions$FROM)/average, info = metric)
        expect_equal(regions$ABOVE_AVERAGE, regions$RATIO_TO_AVERAGE > 1, info = metric)
        expect_true(all(d$GAIN[d$TYPE == "flat"] == 0), info = metric)
    }
})

## The levels: fewer jumps keeps only the most obvious; more jumps adds the second tier; fewer flats
## keeps only the longest, the rest folding into lines
test_that("jumps and flats can be few, some or many", {
    poise.61 <- curve.over.all.weights(metric = "SCORE", weights = weights.16.12, minima = c(POISE = 61), regular.level = "+10", twinkling.level = "+5")
    expect_equal(described(get.tradeoff.efficiency(poise.61, jumps = "few")),
        "line to 31.5 2.96x | jump 31.6 | line to 42.1 0.43x | boundary 42.1 0.43x>flat | flat to 52.5")
    expect_equal(described(get.tradeoff.efficiency(poise.61, jumps = "many")), paste(
        "line to 25.0 1.89x | jump 25.1 | line to 25.4 2.59x | jump 25.5 | line to 27.6 1.89x | jump 27.7 |",
        "line to 31.5 1.47x | jump 31.6 | line to 32.5 0.14x | jump 32.6 | line to 42.1 0.35x | boundary 42.1 0.35x>flat | flat to 52.5"))
    expect_equal(described(get.tradeoff.efficiency(curve.over.all.weights(metric = "MAG_DEF"), flats = "few")),
        "line to 4.7 7.19x | boundary 4.7 7.19x>0.58x | line to 36.9 0.58x | boundary 36.9 0.58x>flat | flat to 52.5")
})

## Equal steps are jumps together or not at all: of a +5, a +3 and five +2s, alone between flats, the
## six jumps "some" allows take the +5 and the +3, as the +2s would make seven
test_that("equal steps are jumps together or not at all", {
    x <- round(seq(0, 30, by = 0.1), 1)
    steps <- c(`3` = 5, `9` = 3, `12` = 2, `15` = 2, `18` = 2, `21` = 2, `24` = 2)
    y <- vapply(x, function(w) sum(steps[as.numeric(names(steps)) <= w + 1e-9]), numeric(1))
    d <- get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = x, BEST_VALUE = y))$data
    expect_equal(d$TO[d$TYPE == "jump"], c(3, 9))
    ## With room for eight ("many"), all seven
    d <- get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = x, BEST_VALUE = y), jumps = "many")$data
    expect_equal(d$TO[d$TYPE == "jump"], c(3, 9, 12, 15, 18, 21, 24))
})

## A stretch is described with as few lines as each halve the error: a curve of exactly two straight
## lines is two lines (not more, once the error is nil), meeting at its bend
test_that("lines are added only while they halve the error", {
    x <- round(seq(0, 30, by = 0.1), 1)
    y <- ifelse(x <= 10, 3*x, 30 + 0.5*(x - 10))
    d <- get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = x, BEST_VALUE = y))$data
    expect_equal(d$TYPE, c("line", "boundary", "line"))
    expect_equal(d$TO, c(d$TO[2], d$TO[2], 30))
    expect_true(abs(d$TO[2] - 10) <= 1.25 + 1e-9)
    ## A single straight line stays one
    d <- get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = x, BEST_VALUE = 2*x))$data
    expect_equal(d$TYPE, "line")
    expect_equal(d$RATIO_TO_AVERAGE, 1)
})

test_that("get.tradeoff.efficiency validates its arguments", {
    expect_error(get.tradeoff.efficiency(data.frame(x = 1:3)), "curve")
    expect_error(get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = c(2, 1, 3), BEST_VALUE = 1:3)), "increasing")
    expect_error(get.tradeoff.efficiency(data.table::data.table(ARMOR_WEIGHT_LIMIT = 1, BEST_VALUE = 1)), "two points")
    curve <- data.table::data.table(ARMOR_WEIGHT_LIMIT = 1:6, BEST_VALUE = c(1, 2, 3, 3, 4, 6))
    expect_error(get.tradeoff.efficiency(curve, jumps = "lots"), "jumps")
    expect_error(get.tradeoff.efficiency(curve, jumps = c("few", "many")), "jumps")
    expect_error(get.tradeoff.efficiency(curve, flats = 3), "flats")
    expect_error(get.tradeoff.efficiency(curve, flats = NA_character_), "flats")
    expect_no_error(get.tradeoff.efficiency(curve, jumps = "few", flats = "many"))
    ## A get.armor.tradeoffs() result or its data
    expect_identical(get.tradeoff.efficiency(list(data = curve)), get.tradeoff.efficiency(curve))
})
