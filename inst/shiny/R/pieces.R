
## Each armor slot's pieces as the app's pickers list them (Filter Inputs, the Equip Load page):
## "No <slot>" first, as "None" is for the weapons, then the rest in the data's order
HEAD.PIECES <- head.data.unupgraded[c(which(ARMOR == "No Head"), which(ARMOR != "No Head"))]
CHEST.PIECES <- chest.data.unupgraded[c(which(ARMOR == "No Chest"), which(ARMOR != "No Chest"))]
HANDS.PIECES <- hands.data.unupgraded[c(which(ARMOR == "No Hands"), which(ARMOR != "No Hands"))]
LEGS.PIECES <- legs.data.unupgraded[c(which(ARMOR == "No Legs"), which(ARMOR != "No Legs"))]
