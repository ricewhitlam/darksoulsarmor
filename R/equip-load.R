
## The game's equip-load rule, established with in-game tests (tests/testthat/test-equip-load.R
## reproduces every one). The game works in 32-bit floating point throughout:
## - Equip load is (40 + Endurance) times the active effects' rates multiplied together first:
##   Havel's Ring 1.5, Ring of Favor and Protection 1.2, Mask of the Father 1.05.
## - Each movement type's line is the load times its share: Light 25%, Mid 50%, Fat 100%. Poop has
##   no limit.
## - The weight carried is the four weapon slots (left hand 1, right hand 1, left hand 2, right
##   hand 2), then head, chest, hands and legs, added one at a time in that order - whatever order
##   they were put on in. Rings and ammo weigh nothing.
## - A total at or below a line gives that movement type.
## Every value is rounded to 32 bits after each step (float32()), so e.g. a set weighing exactly
## 25.0% on paper can still land a hair over the Light line, as it does in the game.

EQUIP.LOAD.RATES <- c(havel.ring = 1.5, favor.ring = 1.2, father.mask = 1.05)
MOVEMENT.SHARES <- c(Light = 0.25, Mid = 0.5, Fat = 1)
WEAPON.SLOTS <- c("left.1", "right.1", "left.2", "right.2")


## The weight of the four weapon slots (a named vector of weapon names, "None" for an empty slot),
## added in the game's slot order
weapons.weight <- function(weapons){
    total <- 0
    for(slot in WEAPON.SLOTS){
        if(weapons[[slot]] != "None"){
            total <- float32(total+float32(weapon.data$WEIGHT[match(weapons[[slot]], weapon.data$WEAPON)]))
        }
    }
    return(total)
}


## The equip load with the given rings, and with or without the Mask of the Father. The rates are
## multiplied together first (in any order - three 32-bit rates give the same product in every
## order), then applied to 40 + Endurance.
equip.load <- function(endurance.level, havel.ring, favor.ring, father.mask){
    rate <- 1
    if(havel.ring){ rate <- float32(rate*float32(EQUIP.LOAD.RATES[["havel.ring"]])) }
    if(favor.ring){ rate <- float32(rate*float32(EQUIP.LOAD.RATES[["favor.ring"]])) }
    if(father.mask){ rate <- float32(rate*float32(EQUIP.LOAD.RATES[["father.mask"]])) }
    return(float32((endurance.level+40)*rate))
}


## The most a character may carry for a movement type, given the equip load
movement.line <- function(load, movement){
    if(movement == "Poop"){
        return(Inf)
    }
    return(float32(load*MOVEMENT.SHARES[[movement]]))
}


## The total weight carried: the weapons' weight (weapons.weight()), then each armor slot's weight
## in the game's order. Vectorized over the armor weights.
total.weight <- function(weapons.weight, head.weight, chest.weight, hands.weight, legs.weight){
    total <- float32(weapons.weight+float32(head.weight))
    total <- float32(total+float32(chest.weight))
    total <- float32(total+float32(hands.weight))
    total <- float32(total+float32(legs.weight))
    return(total)
}


## The movement type a total weight gives, against the equip load: the lightest whose line it is at
## or below. Vectorized over the totals and loads.
movement.type <- function(total, load){
    ifelse(total <= float32(load*MOVEMENT.SHARES[["Light"]]), "Light",
        ifelse(total <= float32(load*MOVEMENT.SHARES[["Mid"]]), "Mid",
            ifelse(total <= float32(load*MOVEMENT.SHARES[["Fat"]]), "Fat", "Poop")))
}
