## get.all.armor.combos itself is a thin wrapper, but its work happens in all_armor_combinations
## (src/all_combos.cpp), which unpacks and sums 13 columns per slot by hand - easy places for a
## copy-paste slip (one slot's column summed into another metric, max instead of min). Calling the
## full function would build all ~11.9 million combinations (~1.6 GB), so this checks the C++
## directly on a few small tables against a brute-force sum.
test_that("all_armor_combinations sums every combination's stats correctly", {
    set.seed(20261009)
    h <- darksoulsarmor:::get.interp.data(head.data.unupgraded, head.data.fullupgrade, 7, 3)[sample(.N, 3)]
    c <- darksoulsarmor:::get.interp.data(chest.data.unupgraded, chest.data.fullupgrade, 7, 3)[sample(.N, 4)]
    g <- darksoulsarmor:::get.interp.data(hands.data.unupgraded, hands.data.fullupgrade, 7, 3)[sample(.N, 2)]
    l <- darksoulsarmor:::get.interp.data(legs.data.unupgraded, legs.data.fullupgrade, 7, 3)[sample(.N, 5)]

    actual <- data.table::setDT(darksoulsarmor:::all_armor_combinations(h, c, g, l))

    summed.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "POISE", "BLEED_RES", "POIS_RES", "CURSE_RES")
    expect_identical(names(actual), c("HEAD", "CHEST", "HANDS", "LEGS", summed.cols, "DURABILITY", "WEIGHT"))
    expect_equal(nrow(actual), 3*4*2*5)

    ## The C++ loops head outermost and legs innermost, which is CJ's order too
    grid <- data.table::CJ(H = 1:3, C = 1:4, G = 1:2, L = 1:5)
    expect_identical(actual$HEAD, h$ARMOR[grid$H])
    expect_identical(actual$CHEST, c$ARMOR[grid$C])
    expect_identical(actual$HANDS, g$ARMOR[grid$G])
    expect_identical(actual$LEGS, l$ARMOR[grid$L])
    for(col in c(summed.cols, "WEIGHT")){
        expect_equal(actual[[col]], h[[col]][grid$H] + c[[col]][grid$C] + g[[col]][grid$G] + l[[col]][grid$L], info = col)
    }
    expect_equal(actual$DURABILITY, pmin(h$DURABILITY[grid$H], c$DURABILITY[grid$C], g$DURABILITY[grid$G], l$DURABILITY[grid$L]))
})
