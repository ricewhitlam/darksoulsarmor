

## Dark Souls armor pieces have standard upgrade patterns based on armor type (Regular vs Twinkling) and on attribute type (Defense vs Resistance)
## This app has the full data for both unupgraded and fully upgraded armor pieces as well as the upgrade patterns
## Therefore, the unupgraded and fully upgraded values for an attribute can be combined via weighted average to determine the values for any upgrade level


## Interpolation functions
## Given the upgrade level of an armor piece, get the appropriate weight for the fully upgraded value in the weighted average
get.reg.def.weight_10 <- approxfun(x = 0:10, y = c(0, 10/142, 20/142, 30/142, 43/142, 56/142, 69/142, 85/142, 101/142, 117/142, 1))
get.reg.res.weight_10 <- approxfun(x = 0:10, y = c(0, 0, 0, 0, 1/8, 2/8, 3/8, 4/8, 5/8, 6/8, 1))
get.twink.def.weight_05 <- approxfun(x = 0:5, y = c(0, 8/55, 19/55, 29/55, 39/55, 1))
get.twink.res.weight_05 <- approxfun(x = 0:5, y = c(0, 5/27, 9/27, 14/27, 18/27, 1))


## Interpolate to a dataset with specified regular upgrade level and twinkling upgrade level from the unpgraded and fully upgraded datasets
get.interp.data <- function(data_00, data_10, reg.lvl, twink.lvl){

    ## Check that the unupgraded and fully upgraded datasets are compatible 
    if(!all(data_00$ARMOR == data_10$ARMOR)){
        data.table::setorder(data_00, ARMOR)
        data.table::setorder(data_10, ARMOR)
        if(!all(data_00$ARMOR == data_10$ARMOR)){
            stop("Armor sets are incompatible - check underlying data")
        }
    }

    ## Define column names and attribute weights
    def.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF")
    res.cols <- c("BLEED_RES", "POIS_RES", "CURSE_RES")
    def.res.cols <- c(def.cols, res.cols)
    reg.weights_10 <- c(rep(get.reg.def.weight_10(reg.lvl), 7), rep(get.reg.res.weight_10(reg.lvl), 3))
    twink.weights_05 <- c(rep(get.twink.def.weight_05(twink.lvl), 7), rep(get.twink.res.weight_05(twink.lvl), 3))

    ## Apply weights to and combine unupgraded and fully upgraded data to get interpolated data
    data.final <- 
        rbind(
            data.table::copy(data_00)[
                UPGRADE_TYPE == "Regular", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, 1-reg.weights_10, SIMPLIFY = FALSE)
            ][
                UPGRADE_TYPE == "Twinkling", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, 1-twink.weights_05, SIMPLIFY = FALSE)
            ],
            data.table::copy(data_10)[UPGRADE_TYPE != "None"][, c("POISE", "DURABILITY", "WEIGHT") := 0][
                UPGRADE_TYPE == "Regular", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, reg.weights_10, SIMPLIFY = FALSE)
            ][
                UPGRADE_TYPE == "Twinkling", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, twink.weights_05, SIMPLIFY = FALSE)
            ]
        )[,
            lapply(.SD, function(x){round(sum(x), 1)}), 
            by = .(ARMOR, UPGRADE_TYPE, STARTING_CLASS, AREA_FORMULA, LINK),
            .SDcols = c(def.cols, "POISE", res.cols, "DURABILITY", "WEIGHT")
        ]
    
    ## Tidy data
    data.table::setnames(data.final, c("ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", def.cols, "POISE", res.cols, "DURABILITY", "WEIGHT"))
    data.table::setorder(data.final, ARMOR)

    rm(list = c("def.cols", "res.cols", "def.res.cols", "reg.weights_10", "twink.weights_05"))
    gc()

    return(data.final)

}