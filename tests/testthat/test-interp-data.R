## get.interp.data blends the head.data.unupgraded and head.data.fullupgrade tables by upgrade
## level; at the boundary levels it should reproduce the source table exactly for the
## matching upgrade type, and leave UPGRADE_TYPE == "None" pieces unaffected by level.
test_that("get.interp.data reproduces the unupgraded table at level 0", {
    interp <- darksoulsarmor:::get.interp.data(head.data.unupgraded, head.data.fullupgrade, 0, 0)
    interp <- interp[order(ARMOR)]

    reg.interp <- interp[UPGRADE_TYPE == "Regular"]
    reg.base <- head.data.unupgraded[UPGRADE_TYPE == "Regular"][order(ARMOR)]
    expect_equal(reg.interp$PHYS_DEF, reg.base$PHYS_DEF)
    expect_equal(reg.interp$BLEED_RES, reg.base$BLEED_RES)

    twink.interp <- interp[UPGRADE_TYPE == "Twinkling"]
    twink.base <- head.data.unupgraded[UPGRADE_TYPE == "Twinkling"][order(ARMOR)]
    expect_equal(twink.interp$PHYS_DEF, twink.base$PHYS_DEF)
    expect_equal(twink.interp$BLEED_RES, twink.base$BLEED_RES)
})

test_that("get.interp.data reproduces the fully upgraded table at max level", {
    interp <- darksoulsarmor:::get.interp.data(head.data.unupgraded, head.data.fullupgrade, 10, 5)
    interp <- interp[order(ARMOR)]

    reg.interp <- interp[UPGRADE_TYPE == "Regular"]
    reg.max <- head.data.fullupgrade[UPGRADE_TYPE == "Regular"][order(ARMOR)]
    expect_equal(reg.interp$PHYS_DEF, reg.max$PHYS_DEF)
    expect_equal(reg.interp$BLEED_RES, reg.max$BLEED_RES)

    twink.interp <- interp[UPGRADE_TYPE == "Twinkling"]
    twink.max <- head.data.fullupgrade[UPGRADE_TYPE == "Twinkling"][order(ARMOR)]
    expect_equal(twink.interp$PHYS_DEF, twink.max$PHYS_DEF)
    expect_equal(twink.interp$BLEED_RES, twink.max$BLEED_RES)
})

test_that("get.interp.data leaves UPGRADE_TYPE == 'None' pieces unaffected by level", {
    at.zero <- darksoulsarmor:::get.interp.data(head.data.unupgraded, head.data.fullupgrade, 0, 0)
    at.max <- darksoulsarmor:::get.interp.data(head.data.unupgraded, head.data.fullupgrade, 10, 5)

    none.zero <- at.zero[UPGRADE_TYPE == "None"][order(ARMOR)]
    none.max <- at.max[UPGRADE_TYPE == "None"][order(ARMOR)]
    none.base <- head.data.unupgraded[UPGRADE_TYPE == "None"][order(ARMOR)]

    expect_equal(none.zero$PHYS_DEF, none.base$PHYS_DEF)
    expect_equal(none.max$PHYS_DEF, none.base$PHYS_DEF)
})
