

## Load packages
require("devtools")

## Use the package's own get.interp.data rather than maintaining a second copy of it here.
## This is a script-time convenience only - it adds no DESCRIPTION-level dependency.
pkgload::load_all(".")


## Load armor datasets
armor_metainfo <- fread("create_rda/armor_metainfo.csv")
armor_00 <- fread("create_rda/armor_00.csv")
armor_00 <- merge(armor_00, armor_metainfo[, .(ARMOR, TYPE, UPGRADE_TYPE)], by = "ARMOR")
armor_10 <- fread("create_rda/armor_10.csv")
armor_10 <- merge(armor_10, armor_metainfo[, .(ARMOR, TYPE, UPGRADE_TYPE)], by = "ARMOR")


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

metric.cols <- c("PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF", "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "BLEED_RES", "POIS_RES", "CURSE_RES")

## Total number of possible armor combinations across every upgrade level (every regular level
## 0-10 and twinkling level 0-5 stacked as separate rows per slot in total.*.data above). Not
## currently used by any exported function (get.optimal.armor.combos' SCORE_QUALITY counts
## combinations at the selected upgrade level only - see R/score-quality.R), but kept as a
## documented, non-duplicated fact for anyone who wants it - see R/data.R. Computed
## as an exact integer product since it is itself an exact count, not a probability.
total.combo.count <- as.numeric(nrow(total.head.data)) * nrow(total.chest.data) * nrow(total.hands.data) * nrow(total.legs.data)

## Computes the population mean, sd, and correlation matrix (over the 10 scored metrics) for one
## armor population, given as a head/chest/hands/legs table set. Used both for the population
## pooled across every upgrade level (means/stddevs/corrs below) and, separately, for the
## population within each individual (regular.level, twinkling.level) pair (mean.stddev.corr.list
## below) - the only difference between the two is which tables are passed in.
##
## The population is every head x chest x hands x legs combination, each counted once, so the
## piece chosen in each slot is independent of the other slots' pieces. A combination's metric is
## the sum of its four pieces' values, and for a sum of independent parts the mean is the sum of
## the parts' means and the covariance matrix is the sum of the parts' covariance matrices. So the
## statistics follow exactly from each slot table's own (population, divide-by-n) means and
## covariances, without visiting the combinations themselves (~21.9 billion for the pooled
## population) - this reproduces the brute-force enumeration this script previously did to within
## floating-point noise (~1e-14).
compute.mean.sd.corr <- function(head.data, chest.data, hands.data, legs.data, metric.cols){
    slot.means <- function(d){ colMeans(as.matrix(d[, ..metric.cols])) }
    slot.covar <- function(d){
        m <- as.matrix(d[, ..metric.cols])
        crossprod(sweep(m, 2, colMeans(m)))/nrow(m)
    }

    metric.means <- slot.means(head.data)+slot.means(chest.data)+slot.means(hands.data)+slot.means(legs.data)
    metric.covar <- slot.covar(head.data)+slot.covar(chest.data)+slot.covar(hands.data)+slot.covar(legs.data)
    metric.stddevs <- sqrt(diag(metric.covar))
    metric.corrs <- metric.covar/outer(metric.stddevs, metric.stddevs)
    diag(metric.corrs) <- 1

    list(means = metric.means, stddevs = metric.stddevs, corrs = metric.corrs)
}

pooled.stats <- compute.mean.sd.corr(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, metric.cols)
means <- pooled.stats$means
stddevs <- pooled.stats$stddevs
corrs <- pooled.stats$corrs

## mean.stddev.corr.list: mean/sd/corr WITHIN each individual (regular.level, twinkling.level)
## pair - the population where every armor slot is evaluated at that same level simultaneously
## (matching how a player thinks of their loadout: "I'm using +5 regular titanite gear"), as
## opposed to means/stddevs/corrs above, which pool across every upgrade level with each slot's
## own upgrade increment varying independently of the others. Not currently used by any exported
## function - kept for potential future use (e.g. an upgrade-level-aware scoring model). Each
## population is built with a single get.interp.data() call per slot (matching exactly how
## get.optimal.armor.combos/get.all.armor.combos build their own working data at one upgrade
## level - non-upgradeable pieces are included automatically, at their fixed base stats, since
## get.interp.data() gives UPGRADE_TYPE == "None" pieces weight 0 rather than excluding them),
## not the reg/twink-stacking loop used for total.*.data above. "overall" holds the same values
## as means/stddevs/corrs above, included here too for a single consistent access point.
mean.stddev.corr.list <- list()
for(reg in 0:10){
    for(twink in 0:5){
        level.key <- paste0(reg, "_", twink)
        level.head.data <- get.interp.data(armor_00[TYPE == "Head"], armor_10[TYPE == "Head"], reg, twink)
        level.chest.data <- get.interp.data(armor_00[TYPE == "Chest"], armor_10[TYPE == "Chest"], reg, twink)
        level.hands.data <- get.interp.data(armor_00[TYPE == "Hands"], armor_10[TYPE == "Hands"], reg, twink)
        level.legs.data <- get.interp.data(armor_00[TYPE == "Legs"], armor_10[TYPE == "Legs"], reg, twink)
        mean.stddev.corr.list[[level.key]] <- compute.mean.sd.corr(level.head.data, level.chest.data, level.hands.data, level.legs.data, metric.cols)
    }
}
mean.stddev.corr.list[["overall"]] <- pooled.stats
rm(list = c("level.key", "level.head.data", "level.chest.data", "level.hands.data", "level.legs.data"))


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
    ## Same closed form as compute.mean.sd.corr(): the score is a sum over slots, so its population
    ## mean and variance are the sums of each slot's own
    slot.score.mean <- function(d){ mean(d$SCORE) }
    slot.score.var <- function(d){ mean((d$SCORE-mean(d$SCORE))^2) }
    print(weights)
    print(slot.score.mean(working.head.data)+slot.score.mean(working.chest.data)+slot.score.mean(working.hands.data)+slot.score.mean(working.legs.data))
    print(sqrt(slot.score.var(working.head.data)+slot.score.var(working.chest.data)+slot.score.var(working.hands.data)+slot.score.var(working.legs.data)))
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


## Create other data files
armor_00 <- fread("create_rda/armor_00.csv")
armor_00 <- merge(armor_00, armor_metainfo, by = "ARMOR")
armor_10 <- fread("create_rda/armor_10.csv")
armor_10 <- merge(armor_10, armor_metainfo, by = "ARMOR")

colorder <- 
    c(
        "ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_MATCH_TYPE", "AREA_LIST", "LINK",
        "PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF",    
        "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "POISE",
        "BLEED_RES", "POIS_RES", "CURSE_RES",
        "DURABILITY",  "WEIGHT", "STAM_MOD", "SOUND_MOD"
    )

head.data.unupgraded <- armor_00[TYPE == "Head"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(head.data.unupgraded, colorder)
chest.data.unupgraded <- armor_00[TYPE == "Chest"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(chest.data.unupgraded, colorder)
hands.data.unupgraded <- armor_00[TYPE == "Hands"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(hands.data.unupgraded, colorder)
legs.data.unupgraded <- armor_00[TYPE == "Legs"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(legs.data.unupgraded, colorder)

head.data.fullupgrade <- armor_10[TYPE == "Head"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(head.data.fullupgrade, colorder)
chest.data.fullupgrade <- armor_10[TYPE == "Chest"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(chest.data.fullupgrade, colorder)
hands.data.fullupgrade <- armor_10[TYPE == "Hands"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(hands.data.fullupgrade, colorder)
legs.data.fullupgrade <- armor_10[TYPE == "Legs"][, c("INDEX", "TYPE", "SET") := NULL]
setcolorder(legs.data.fullupgrade, colorder)

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

## Save out to rda files. head.data.unupgraded etc., areas, and classes are the package's public,
## documented data (data/*.rda). means/stddevs/corrs exist only to normalize the score formula
## in get.optimal.armor.combos and are not meant to be used directly, so they are saved
## separately as internal data (R/sysdata.rda) rather than exported alongside the public data.
use_data(
    head.data.unupgraded,
    chest.data.unupgraded,
    hands.data.unupgraded,
    legs.data.unupgraded,
    head.data.fullupgrade,
    chest.data.fullupgrade,
    hands.data.fullupgrade,
    legs.data.fullupgrade,
    areas,
    classes,
    overwrite = TRUE
)
use_data(
    means,
    stddevs,
    corrs,
    total.combo.count,
    mean.stddev.corr.list,
    internal = TRUE,
    overwrite = TRUE
)

