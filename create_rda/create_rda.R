

## Load packages and set up for parallel calcs
require("devtools")
require("dsa.rda")
require("doParallel")
registerDoParallel(cores = detectCores()-1)


## Load armor datasets
armor_metainfo <- fread("create_rda/armor_metainfo.csv")
armor_00 <- fread("create_rda/armor_00.csv")
armor_00 <- merge(armor_00, armor_metainfo[, .(ARMOR, TYPE, UPGRADE_TYPE)], by = "ARMOR")
armor_10 <- fread("create_rda/armor_10.csv")
armor_10 <- merge(armor_10, armor_metainfo[, .(ARMOR, TYPE, UPGRADE_TYPE)], by = "ARMOR")


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
    const.cols <- c("POISE", "DURABILITY", "WEIGHT", "STAM_MOD", "SOUND_MOD")
    out.cols <- c(def.cols, "POISE", res.cols, "DURABILITY", "WEIGHT", "STAM_MOD", "SOUND_MOD")
    def.res.cols <- c(def.cols, res.cols)
    info.cols <- setdiff(colnames(data_00), out.cols)
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
            data.table::copy(data_10)[UPGRADE_TYPE != "None"][, (const.cols) := 0][
                UPGRADE_TYPE == "Regular", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, reg.weights_10, SIMPLIFY = FALSE)
            ][
                UPGRADE_TYPE == "Twinkling", 
                (def.res.cols) := mapply(function(col, weight){get(col)*weight}, def.res.cols, twink.weights_05, SIMPLIFY = FALSE)
            ]
        )[,
            lapply(.SD, function(x){round(sum(x), 1)}), 
            by = info.cols,
            .SDcols = out.cols
        ]
    
    ## Tidy data
    data.table::setorder(data.final, ARMOR)

    return(data.final)

}


## Create datasets inclusive of all upgrades 
total.head.data <- armor_00[TYPE == "Head" & UPGRADE_TYPE == "None"][, c("TYPE", "UPGRADE_TYPE") := NULL]
total.chest.data <- armor_00[TYPE == "Chest" & UPGRADE_TYPE == "None"][, c("TYPE", "UPGRADE_TYPE") := NULL]
total.hands.data <- armor_00[TYPE == "Hands" & UPGRADE_TYPE == "None"][, c("TYPE", "UPGRADE_TYPE") := NULL]
total.legs.data <- armor_00[TYPE == "Legs" & UPGRADE_TYPE == "None"][, c("TYPE", "UPGRADE_TYPE") := NULL]

for(reg in 0:10){
    total.head.data <- rbind(total.head.data, get.interp.data(armor_00[TYPE == "Head" & UPGRADE_TYPE == "Regular"], armor_10[TYPE == "Head" & UPGRADE_TYPE == "Regular"], reg, 0)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", reg)])
    total.chest.data <- rbind(total.chest.data, get.interp.data(armor_00[TYPE == "Chest" & UPGRADE_TYPE == "Regular"], armor_10[TYPE == "Chest" & UPGRADE_TYPE == "Regular"], reg, 0)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", reg)])
    total.hands.data <- rbind(total.hands.data, get.interp.data(armor_00[TYPE == "Hands" & UPGRADE_TYPE == "Regular"], armor_10[TYPE == "Hands" & UPGRADE_TYPE == "Regular"], reg, 0)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", reg)])
    total.legs.data <- rbind(total.legs.data, get.interp.data(armor_00[TYPE == "Legs" & UPGRADE_TYPE == "Regular"], armor_10[TYPE == "Legs" & UPGRADE_TYPE == "Regular"], reg, 0)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", reg)])
}

for(twink in 0:5){
    total.head.data <- rbind(total.head.data, get.interp.data(armor_00[TYPE == "Head" & UPGRADE_TYPE == "Twinkling"], armor_10[TYPE == "Head" & UPGRADE_TYPE == "Twinkling"], 0, twink)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", twink)])
    total.chest.data <- rbind(total.chest.data, get.interp.data(armor_00[TYPE == "Chest" & UPGRADE_TYPE == "Twinkling"], armor_10[TYPE == "Chest" & UPGRADE_TYPE == "Twinkling"], 0, twink)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", twink)])
    total.hands.data <- rbind(total.hands.data, get.interp.data(armor_00[TYPE == "Hands" & UPGRADE_TYPE == "Twinkling"], armor_10[TYPE == "Hands" & UPGRADE_TYPE == "Twinkling"], 0, twink)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", twink)])
    total.legs.data <- rbind(total.legs.data, get.interp.data(armor_00[TYPE == "Legs" & UPGRADE_TYPE == "Twinkling"], armor_10[TYPE == "Legs" & UPGRADE_TYPE == "Twinkling"], 0, twink)[, c("TYPE", "UPGRADE_TYPE") := NULL][, ARMOR := paste0(ARMOR, " +", twink)])
}


## Calc mean, sd, and corr data
sel.head.data <- copy(total.head.data)
sel.chest.data <- copy(total.chest.data)
sel.hands.data <- copy(total.hands.data)
sel.legs.data <- copy(total.legs.data)

N_inv <- (1/nrow(sel.head.data))*(1/nrow(sel.chest.data))*(1/nrow(sel.hands.data))*(1/nrow(sel.legs.data))
metric.indices <- c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11)

means <- 
    foreach(i = 1:10, .combine = c, .packages = "dsa.rda") %dopar% {
        N_inv*dsa_get_metric_mean(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, metric.indices[i])
    }
names(means) <- names(sel.head.data)[metric.indices+1]

stddevs <- 
    foreach(i = 1:10, .combine = c, .packages = "dsa.rda") %dopar% {
        sqrt(N_inv*dsa_get_metric_var(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, metric.indices[i], means[i]))
    }
names(stddevs) <- names(sel.head.data)[metric.indices+1]

indices <- integer(0)
for(i in 1:9){
    for(j in (i+1):10){
        indices <- c(indices, as.integer(10*(i-1)+(j-1)))
    }
}
corr.vec <- 
    foreach(i = indices, .combine = c, .packages = "dsa.rda") %dopar% {
        m <- i %% 10
        n <- (i-m)/10
        (N_inv/(stddevs[m+1]*stddevs[n+1]))*dsa_get_metrics_covar(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, metric.indices[m+1], metric.indices[n+1], means[m+1], means[n+1])
    }
corrs <- diag(10)
for(i in seq_along(indices)){
    index <- indices[i]
    val <- corr.vec[i]
    m <- index %% 10
    n <- (index-m)/10
    corrs[m+1, n+1] <- val
    corrs[n+1, m+1] <- val
}
rownames(corrs) <- names(sel.head.data)[metric.indices+1]
colnames(corrs) <- names(sel.head.data)[metric.indices+1]


## Function to check behavior given set of weights - mean should always be 0 and sd should always be 1
test.meansd <- function(weights = runif(10)){
    weights <- weights/sum(weights)
    score.means <- means
    score.scalars <- (weights)/(stddevs*sqrt((t(weights) %*% corrs %*% weights)[1, 1]))
    working.head.data <- copy(total.head.data)
    working.chest.data <- copy(total.chest.data)
    working.hands.data <- copy(total.hands.data)
    working.legs.data <- copy(total.legs.data)
    working.head.data[, SCORE := 0]
    working.chest.data[, SCORE := 0]
    working.hands.data[, SCORE := 0]
    working.legs.data[, SCORE := 0]
    score.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
    for(i in seq_along(score.cols)){
        working.head.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
        working.chest.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
        working.hands.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
        working.legs.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
    }
    print(weights)
    print(N_inv*dsa_get_metric_mean(working.head.data, working.chest.data, working.hands.data, working.legs.data, which(colnames(working.head.data) == "SCORE")-1))
    print(sqrt(N_inv*dsa_get_metric_var(working.head.data, working.chest.data, working.hands.data, working.legs.data, which(colnames(working.head.data) == "SCORE")-1, 0)))
}
test.meansd()


## Below chunk is in testing
# ## Get weight mean, sd, covariances with metrics -> linear model where weight predicts score
# ## This gives a way to identify which combos have high scores relative to their weight
# weight.index <- 17
# mean.weight <- N_inv*dsa_get_metric_mean(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, weight.index)
# stddev.weight <- sqrt(N_inv*dsa_get_metric_var(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, weight.index, mean.weight))
# covars.weight <- 
#     foreach(i = seq_along(metric.indices), .combine = c, .packages = "dsa.rda") %dopar% {
#         N_inv*dsa_get_metrics_covar(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, weight.index, metric.indices[i], mean.weight, means[i])
#     }


# ## Function to check weight model
# test.weightlm <- function(weights = runif(10)){
#     weights <- weights/sum(weights)
#     score.means <- means
#     score.scalars <- (weights)/(stddevs*sqrt((t(weights) %*% corrs %*% weights)[1, 1]))
#     lm.beta <- sum(score.scalars*covars.weight)/stddev.weight^2
#     lm.alpha <- -lm.beta*mean.weight
#     lm.rsqd <- sign(lm.beta)*(lm.beta*stddev.weight)^2
#     working.head.data <- copy(total.head.data)
#     working.chest.data <- copy(total.chest.data)
#     working.hands.data <- copy(total.hands.data)
#     working.legs.data <- copy(total.legs.data)
#     working.head.data[, SCORE := 0]
#     working.chest.data[, SCORE := 0]
#     working.hands.data[, SCORE := 0]
#     working.legs.data[, SCORE := 0]
#     score.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")
#     for(i in seq_along(score.cols)){
#         working.head.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
#         working.chest.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
#         working.hands.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
#         working.legs.data[, SCORE := SCORE+score.scalars[i]*(get(score.cols[i])-0.25*score.means[i])]
#     }
#     head.sample <- sample(seq_len(nrow(working.head.data)), 10000, replace = TRUE)
#     chest.sample <- sample(seq_len(nrow(working.chest.data)), 10000, replace = TRUE)
#     hands.sample <- sample(seq_len(nrow(working.hands.data)), 10000, replace = TRUE)
#     legs.sample <- sample(seq_len(nrow(working.legs.data)), 10000, replace = TRUE)
#     out <- working.head.data[head.sample][, c("ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", "POISE", "DURABILITY") := NULL]
#     out <- out+working.chest.data[chest.sample][, c("ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", "POISE", "DURABILITY") := NULL]
#     out <- out+working.hands.data[hands.sample][, c("ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", "POISE", "DURABILITY") := NULL]
#     out <- out+working.legs.data[legs.sample][, c("ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", "POISE", "DURABILITY") := NULL]
#     out[, FITTED_SCORE := lm.alpha+lm.beta*WEIGHT]
#     print(weights)
#     print(lm.beta)
#     print(lm.alpha)
#     print(lm.rsqd)
#     return(out)
# }

# d <- test.weightlm()
# d[, plot(WEIGHT, SCORE)]
# d[, lines(WEIGHT, FITTED_SCORE)]
# d[, summary(lm(SCORE ~ WEIGHT))]
# d[, plot(WEIGHT, SCORE-FITTED_SCORE)]
# d[, qqnorm(SCORE-FITTED_SCORE)]


## Stop parallel computing
stopImplicitCluster()


## Create other data files
armor_00 <- fread("create_rda/armor_00.csv")
armor_00 <- merge(armor_00, armor_metainfo, by = "ARMOR")
armor_10 <- fread("create_rda/armor_10.csv")
armor_10 <- merge(armor_10, armor_metainfo, by = "ARMOR")

colorder <- 
    c(
        "ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", 
        "PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF",    
        "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "POISE",
        "BLEED_RES", "POIS_RES", "CURSE_RES",
        "DURABILITY",  "WEIGHT", "STAM_MOD", "SOUND_MOD"
    )

head.data_00 <- armor_00[TYPE == "Head"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(head.data_00, colorder)
chest.data_00 <- armor_00[TYPE == "Chest"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(chest.data_00, colorder)
hands.data_00 <- armor_00[TYPE == "Hands"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(hands.data_00, colorder)
legs.data_00 <- armor_00[TYPE == "Legs"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(legs.data_00, colorder)

head.data_10 <- armor_10[TYPE == "Head"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(head.data_10, colorder)
chest.data_10 <- armor_10[TYPE == "Chest"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(chest.data_10, colorder)
hands.data_10 <- armor_10[TYPE == "Hands"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(hands.data_10, colorder)
legs.data_10 <- armor_10[TYPE == "Legs"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(legs.data_10, colorder)

areas <- 
    c(
        "Undead Burg", "Undead Parish", "Lower Undead Burg", 
        "Depths", "Blighttown", "Valley of Drakes",
        "Sens Fortress", "Anor Londo", "Painted World of Ariamis",
        "Darkroot Basin", "Darkroot Garden", "New Londo Ruins",
        "Demon Ruins", "Lost Izalith", 
        "Catacombs", "Tomb of the Giants",
        "The Dukes Archives", "Artorias of the Abyss",
        "Kiln of the First Flame"
    )
classes <- c("Warrior", "Knight", "Wanderer", "Thief", "Bandit", "Hunter", "Sorcerer", "Pyromancer", "Cleric", "Deprived")

## Save out to rda files
use_data(
    head.data_00, 
    chest.data_00, 
    hands.data_00, 
    legs.data_00, 
    head.data_10, 
    chest.data_10, 
    hands.data_10, 
    legs.data_10, 
    areas, 
    classes,
    means,
    stddevs,
    corrs,
    # mean.weight,
    # stddev.weight,
    # covars.weight,
    overwrite = TRUE
)

