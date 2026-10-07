

## Dark Souls computes an upgraded armor piece's stats from its +0 stats: each defense or resistance
## at an upgrade level is its +0 value times that level's rate, in 32-bit floating point. The
## rates below are the game's own (its ReinforceParamProtector table). Regular armor (upgraded
## with titanite, +0 to +10) and twinkling armor (twinkling titanite, +0 to +5) each have one set
## for defenses and another for resistances. Strike, slash and thrust defense follow the defense
## rate: the game derives them from upgraded physical defense, and applying the rate to their exact
## +0 values instead gives the same values to within the last bit of a 32-bit float.
REGULAR.DEF.RATES <- c(1, 1.1, 1.2, 1.3, 1.43, 1.56, 1.69, 1.85, 2.01, 2.17, 2.42)
REGULAR.RES.RATES <- c(1, 1, 1, 1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.4)
TWINKLING.DEF.RATES <- c(1, 1.08, 1.19, 1.29, 1.39, 1.55)
TWINKLING.RES.RATES <- c(1, 1.05, 1.09, 1.14, 1.18, 1.27)


## Rounds each value to the nearest 32-bit float (ties to even): the precision the game stores and
## computes armor stats in. writeBin() narrows each value to a C float and readBin() widens it back,
## exactly.
float32 <- function(x){
    readBin(writeBin(as.numeric(x), raw(), size = 4), "double", size = 4, n = length(x))
}


## A slot table (head.data.unupgraded etc.) at the given regular and twinkling upgrade levels.
## Values are exact, i.e. what the game computes - not rounded for display. Pieces that can't be
## upgraded (UPGRADE_TYPE == "None") keep their +0 values, as do POISE, DURABILITY, WEIGHT,
## STAM_MOD and SOUND_MOD, which never change with upgrade level.
get.interp.data <- function(data.unupgraded, reg.lvl, twink.lvl){

    def.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF")
    res.cols <- c("BLEED_RES", "POIS_RES", "CURSE_RES")

    ## Each piece's rates, by its upgrade type, as the game's 32-bit values
    is.reg <- data.unupgraded$UPGRADE_TYPE == "Regular"
    is.twink <- data.unupgraded$UPGRADE_TYPE == "Twinkling"
    def.rate <- float32(ifelse(is.reg, REGULAR.DEF.RATES[reg.lvl+1], ifelse(is.twink, TWINKLING.DEF.RATES[twink.lvl+1], 1)))
    res.rate <- float32(ifelse(is.reg, REGULAR.RES.RATES[reg.lvl+1], ifelse(is.twink, TWINKLING.RES.RATES[twink.lvl+1], 1)))

    ## The product of two 32-bit values is exact in double precision, so rounding it once to 32 bits
    ## is exactly the game's 32-bit multiplication
    data.final <- data.table::copy(data.unupgraded)
    for(col in def.cols){
        data.table::set(data.final, j = col, value = float32(data.unupgraded[[col]]*def.rate))
    }
    for(col in res.cols){
        data.table::set(data.final, j = col, value = float32(data.unupgraded[[col]]*res.rate))
    }

    ## Tidy data
    data.table::setorder(data.final, ARMOR)

    return(data.final)

}
