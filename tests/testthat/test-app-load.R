## Load Inputs on its own (inst/shiny/R/load.R): its dialog has the movement type, Endurance and a
## dropdown per weapon slot, with the weapons' total weight as they're chosen; Done saves the choices
## (the weapons only when every slot has one, Endurance rounded), and unchanged says whether the
## saved choices are the last refresh's
source(system.file("shiny", "R", "load.R", package = "darksoulsarmor"), local = TRUE)

test_that("Load Inputs saves its choices on Done and compares them with the last refresh", {
    modals <- character(0)
    local_mocked_bindings(showModal = function(ui, ...){ modals <<- c(modals, as.character(ui)) }, .package = "shiny")
    last.args <- shiny::reactiveVal(NULL)
    no.weapons <- c(left.1 = "None", right.1 = "None", left.2 = "None", right.2 = "None")
    shiny::testServer(load.inputs.server, args = list(last.args = last.args), {
        ## Light, no weapons and Endurance 10 to start with, and nothing to compare before the first refresh
        expect_identical(list(values$movement, values$weapons, values$endurance.level), list("Light", no.weapons, 10))
        expect_true(session$returned$unchanged())
        last.args(list(movement = "Light", weapons = no.weapons, endurance.level = 10, minima = rep(0, 12)))
        expect_true(session$returned$unchanged())

        ## The dialog, under the module's own ids, the slots in the game's order (right hand first)
        session$setInputs(open = 1)
        expect_length(modals, 1)
        for(id in c("movement", "endurance.level", "weapon.right.1", "weapon.right.2", "weapon.left.1", "weapon.left.2", "weapon_summary")){
            expect_match(modals[1], sprintf('id="%s"', session$ns(id)), fixed = TRUE, info = id)
        }
        expect_lt(regexpr("Right Hand Weapon 2", modals[1], fixed = TRUE), regexpr("Left Hand Weapon 1", modals[1], fixed = TRUE))

        ## The total follows the dropdowns before Done
        expect_equal(output$weapon_summary, "Weapons: 0.0 in total")
        session$setInputs(weapon.right.1 = "Claymore", weapon.left.1 = "Talisman", weapon.right.2 = "None", weapon.left.2 = "None")
        expect_equal(output$weapon_summary, "Weapons: 6.3 in total")

        ## Done saves them, Endurance rounded; until the next refresh, they differ from the last one's
        session$setInputs(movement = "Mid", endurance.level = 40.4)
        session$setInputs(done = 1)
        expect_identical(list(values$movement, values$weapons, values$endurance.level), list("Mid", c(left.1 = "Talisman", right.1 = "Claymore", left.2 = "None", right.2 = "None"), 40))
        expect_false(session$returned$unchanged())
        last.args(list(movement = "Mid", weapons = c(left.1 = "Talisman", right.1 = "Claymore", left.2 = "None", right.2 = "None"), endurance.level = 40))
        expect_true(session$returned$unchanged())

        ## A slot left empty keeps every saved weapon
        session$setInputs(open = 2)
        session$setInputs(weapon.right.1 = "", weapon.left.1 = "Dagger")
        session$setInputs(done = 2)
        expect_identical(values$weapons, c(left.1 = "Talisman", right.1 = "Claymore", left.2 = "None", right.2 = "None"))
    })
})
