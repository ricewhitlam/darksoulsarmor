

## Load packages and set up for parallel calcs
require("devtools")
require("dsa.rda")
require("doParallel")
registerDoParallel(cores = detectCores()-1)

## Use the package's own get.interp.data rather than maintaining a second copy of it here.
## This is a script-time convenience only - it adds no DESCRIPTION-level dependency in either
## direction between darksoulsarmor and create_rda/dsa.rda.
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

N_inv <- (1/nrow(sel.head.data))*(1/nrow(sel.chest.data))*(1/nrow(sel.hands.data))*(1/nrow(sel.legs.data))
metric.indices <- c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11)

## Total number of possible armor combinations across every upgrade level (every regular level
## 0-10 and twinkling level 0-5 stacked as separate rows per slot in total.*.data above). Not
## currently used by any exported function (get.optimal.armor.combos' SCORE_QUALITY works
## entirely from SCORE_RAW's normal-tail probability, independent of the true population size),
## but kept as a documented, non-duplicated fact for anyone who wants it - see R/data.R. Computed
## as an exact integer product (not via 1/N_inv, which would round-trip through floating-point
## division first) since it is itself an exact count, not a probability.
total.combo.count <- as.numeric(nrow(total.head.data)) * nrow(total.chest.data) * nrow(total.hands.data) * nrow(total.legs.data)

## Every distinct (i, j) metric pair, i < j, encoded as a single integer 10*(i-1)+(j-1) so a
## single foreach can iterate all 45 pairs. Fixed regardless of which population
## compute.mean.sd.corr() below is run on.
indices <- integer(0)
for(i in 1:9){
    for(j in (i+1):10){
        indices <- c(indices, as.integer(10*(i-1)+(j-1)))
    }
}

## Computes the population mean, sd, and correlation matrix (over the 10 scored metrics) for one
## armor population, given as a head/chest/hands/legs table set. Used both for the population
## pooled across every upgrade level (means/stddevs/corrs below) and, separately, for the
## population within each individual (regular.level, twinkling.level) pair (mean.stddev.corr.list
## below) - the only difference between the two is which tables are passed in.
##
## metric.indices/pair.indices are passed explicitly (not referenced via lexical scoping from the
## enclosing script) because foreach's %dopar% auto-export only inspects this function's own
## execution frame for variables to ship to parallel workers, not the frame it was lexically
## defined in - a variable that's otherwise perfectly visible by normal R scoping rules can still
## come back "object not found" on the workers if it's only reachable by walking further up.
compute.mean.sd.corr <- function(head.data, chest.data, hands.data, legs.data, metric.indices, pair.indices){
    n.inv <- (1/nrow(head.data))*(1/nrow(chest.data))*(1/nrow(hands.data))*(1/nrow(legs.data))

    metric.means <-
        foreach(i = 1:10, .combine = c, .packages = "dsa.rda") %dopar% {
            n.inv*dsa_get_metric_mean(head.data, chest.data, hands.data, legs.data, metric.indices[i])
        }
    names(metric.means) <- names(head.data)[metric.indices+1]

    metric.stddevs <-
        foreach(i = 1:10, .combine = c, .packages = "dsa.rda") %dopar% {
            sqrt(n.inv*dsa_get_metric_var(head.data, chest.data, hands.data, legs.data, metric.indices[i], metric.means[i]))
        }
    names(metric.stddevs) <- names(head.data)[metric.indices+1]

    corr.vec <-
        foreach(i = pair.indices, .combine = c, .packages = "dsa.rda") %dopar% {
            m <- i %% 10
            n <- (i-m)/10
            (n.inv/(metric.stddevs[m+1]*metric.stddevs[n+1]))*dsa_get_metrics_covar(head.data, chest.data, hands.data, legs.data, metric.indices[m+1], metric.indices[n+1], metric.means[m+1], metric.means[n+1])
        }
    metric.corrs <- diag(10)
    for(i in seq_along(pair.indices)){
        index <- pair.indices[i]
        val <- corr.vec[i]
        m <- index %% 10
        n <- (index-m)/10
        metric.corrs[m+1, n+1] <- val
        metric.corrs[n+1, m+1] <- val
    }
    rownames(metric.corrs) <- names(metric.means)
    colnames(metric.corrs) <- names(metric.means)

    list(means = metric.means, stddevs = metric.stddevs, corrs = metric.corrs)
}

pooled.stats <- compute.mean.sd.corr(sel.head.data, sel.chest.data, sel.hands.data, sel.legs.data, metric.indices, indices)
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
        mean.stddev.corr.list[[level.key]] <- compute.mean.sd.corr(level.head.data, level.chest.data, level.hands.data, level.legs.data, metric.indices, indices)
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

