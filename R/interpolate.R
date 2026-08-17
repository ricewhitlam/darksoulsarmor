

## Dark Souls armor pieces have standard upgrade patterns based on armor type (Regular vs Twinkling) and on attribute type (Defense vs Resistance)
## This app has the full data for both unupgraded and fully upgraded armor pieces as well as the upgrade patterns
## Therefore, the unupgraded and fully upgraded values for an attribute can be combined via weighted average to determine the values for any upgrade level


## Function to interpolate to a dataset with specified regular upgrade level and twinkling upgrade level from the unpgraded and fully upgraded datasets
get.interp.data <- function(data_00, data_10, reg.lvl, twink.lvl){

    ## Check that the unupgraded and fully upgraded datasets are compatible 
    if(!all(data_00$ARMOR == data_10$ARMOR)){
        data.table::setorder(data_00, ARMOR)
        data.table::setorder(data_10, ARMOR)
        if(!all(data_00$ARMOR == data_10$ARMOR)){
            stop("Armor sets are incompatible - check underlying data")
        }
    } else if(ncol(data_00) != ncol(data_10)){
        stop("Armor sets are incompatible - check underlying data")
    } else if(!all(colnames(data_00) == colnames(data_10))){
        stop("Armor sets are incompatible - check underlying data")
    }

    ## Interpolation functions for upgrading
    ## Given the upgrade level of an armor piece, get the appropriate weight for the fully upgraded value in the weighted average
    get.reg.def.weight_10 <- approxfun(x = 0:10, y = c(0, 10/142, 20/142, 30/142, 43/142, 56/142, 69/142, 85/142, 101/142, 117/142, 1))
    get.reg.res.weight_10 <- approxfun(x = 0:10, y = c(0, 0, 0, 0, 1/8, 2/8, 3/8, 4/8, 5/8, 6/8, 1))
    get.twink.def.weight_05 <- approxfun(x = 0:5, y = c(0, 8/55, 19/55, 29/55, 39/55, 1))
    get.twink.res.weight_05 <- approxfun(x = 0:5, y = c(0, 5/27, 9/27, 14/27, 18/27, 1))

    ## Define column names and attribute weights
    def.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF")
    res.cols <- c("BLEED_RES", "POIS_RES", "CURSE_RES")

    ## Both tables are now row-aligned by ARMOR, so each metric's interpolated value is simply a
    ## per-row weighted average of the unupgraded and fully upgraded values: a Regular piece
    ## blends toward data_10 by its regular-upgrade weight, a Twinkling piece by its twinkling
    ## weight, and a piece that cannot be upgraded (UPGRADE_TYPE == "None") stays at weight 0,
    ## i.e. exactly its data_00 value. POISE, DURABILITY, WEIGHT, STAM_MOD, and SOUND_MOD never
    ## change with upgrade level, so they are left untouched at their data_00 values below.
    is.reg <- data_00$UPGRADE_TYPE == "Regular"
    is.twink <- data_00$UPGRADE_TYPE == "Twinkling"
    def.weight <- ifelse(is.reg, get.reg.def.weight_10(reg.lvl), ifelse(is.twink, get.twink.def.weight_05(twink.lvl), 0))
    res.weight <- ifelse(is.reg, get.reg.res.weight_10(reg.lvl), ifelse(is.twink, get.twink.res.weight_05(twink.lvl), 0))

    data.final <- data.table::copy(data_00)
    for(col in def.cols){
        data.table::set(data.final, j = col, value = round((1-def.weight)*data_00[[col]]+def.weight*data_10[[col]], 1))
    }
    for(col in res.cols){
        data.table::set(data.final, j = col, value = round((1-res.weight)*data_00[[col]]+res.weight*data_10[[col]], 1))
    }

    ## Tidy data
    data.table::setorder(data.final, ARMOR)

    return(data.final)

}
