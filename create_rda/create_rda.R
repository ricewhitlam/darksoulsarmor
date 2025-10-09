
## Load packages and set up for parallel calcs
devtools::load_all()
require("dsa.rda")
require("doParallel")
registerDoParallel(cores = detectCores()-1)

## Create datasets inclusive of all upgrades 
total.head.data <- head.data_00[UPGRADE_TYPE == "None"]
total.chest.data <- chest.data_00[UPGRADE_TYPE == "None"]
total.hands.data <- hands.data_00[UPGRADE_TYPE == "None"]
total.legs.data <- legs.data_00[UPGRADE_TYPE == "None"]

for(reg in 0:10){
    total.head.data <- rbind(total.head.data, get.interp.data(head.data_00[UPGRADE_TYPE == "Regular"], head.data_10[UPGRADE_TYPE == "Regular"], reg, 0)[, ARMOR := paste0(ARMOR, " +", reg)])
    total.chest.data <- rbind(total.chest.data, get.interp.data(chest.data_00[UPGRADE_TYPE == "Regular"], chest.data_10[UPGRADE_TYPE == "Regular"], reg, 0)[, ARMOR := paste0(ARMOR, " +", reg)])
    total.hands.data <- rbind(total.hands.data, get.interp.data(hands.data_00[UPGRADE_TYPE == "Regular"], hands.data_10[UPGRADE_TYPE == "Regular"], reg, 0)[, ARMOR := paste0(ARMOR, " +", reg)])
    total.legs.data <- rbind(total.legs.data, get.interp.data(legs.data_00[UPGRADE_TYPE == "Regular"], legs.data_10[UPGRADE_TYPE == "Regular"], reg, 0)[, ARMOR := paste0(ARMOR, " +", reg)])
}

for(twink in 0:5){
    total.head.data <- rbind(total.head.data, get.interp.data(head.data_00[UPGRADE_TYPE == "Twinkling"], head.data_10[UPGRADE_TYPE == "Twinkling"], 0, twink)[, ARMOR := paste0(ARMOR, " +", twink)])
    total.chest.data <- rbind(total.chest.data, get.interp.data(chest.data_00[UPGRADE_TYPE == "Twinkling"], chest.data_10[UPGRADE_TYPE == "Twinkling"], 0, twink)[, ARMOR := paste0(ARMOR, " +", twink)])
    total.hands.data <- rbind(total.hands.data, get.interp.data(hands.data_00[UPGRADE_TYPE == "Twinkling"], hands.data_10[UPGRADE_TYPE == "Twinkling"], 0, twink)[, ARMOR := paste0(ARMOR, " +", twink)])
    total.legs.data <- rbind(total.legs.data, get.interp.data(legs.data_00[UPGRADE_TYPE == "Twinkling"], legs.data_10[UPGRADE_TYPE == "Twinkling"], 0, twink)[, ARMOR := paste0(ARMOR, " +", twink)])
}

## Calc mean, sd, and corr data
sel.head.data <- copy(total.head.data)
sel.chest.data <- copy(total.chest.data)
sel.hands.data <- copy(total.hands.data)
sel.legs.data <- copy(total.legs.data)

N_inv <- (1/nrow(sel.head.data))*(1/nrow(sel.chest.data))*(1/nrow(sel.hands.data))*(1/nrow(sel.legs.data))
metric.indices <- c(5, 6, 7, 8, 9, 10, 11, 13, 14, 15)

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

stopImplicitCluster()

## Function to check behavior given set of weights - mean should always be 0 and sd should always be 1
test.meansdcorr <- function(){
    weights <- runif(10)
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
    print(N_inv*dsa_get_metric_mean(working.head.data, working.chest.data, working.hands.data, working.legs.data, which(colnames(working.head.data) == "SCORE")-1))
    print(sqrt(N_inv*dsa_get_metric_var(working.head.data, working.chest.data, working.hands.data, working.legs.data, which(colnames(working.head.data) == "SCORE")-1, 0)))
}

test.meansdcorr()

## Misc Data
data_00 <- fread("create_rda/armor_00.csv", header = TRUE, sep = ",")
data_10 <- fread("create_rda/armor_10.csv", header = TRUE, sep = ",")
areas <- fread("create_rda/areas.csv", header = TRUE, sep = ",")$AREA
classes <- data_00[STARTING_CLASS != "N/A", sort(unique(STARTING_CLASS))]

colorder <- 
    c(
        "ARMOR", "UPGRADE_TYPE", "STARTING_CLASS", "AREA_FORMULA", "LINK", 
        "PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF",    
        "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "POISE",
        "BLEED_RES", "POIS_RES", "CURSE_RES",
        "DURABILITY",  "WEIGHT"
    )

head.data_00 <- dcast(data_00[TYPE == "Head"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
chest.data_00 <- dcast(data_00[TYPE == "Chest"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
hands.data_00 <- dcast(data_00[TYPE == "Hands"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
legs.data_00 <- dcast(data_00[TYPE == "Legs"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
setcolorder(head.data_00, colorder)
setcolorder(chest.data_00, colorder) 
setcolorder(hands.data_00, colorder)
setcolorder(legs.data_00, colorder)

head.data_10 <- dcast(data_10[TYPE == "Head"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
chest.data_10 <- dcast(data_10[TYPE == "Chest"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
hands.data_10 <- dcast(data_10[TYPE == "Hands"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
legs.data_10 <- dcast(data_10[TYPE == "Legs"][, c("INDEX", "TYPE", "SET") := NULL], ARMOR+UPGRADE_TYPE+STARTING_CLASS+AREA_FORMULA+LINK~STATISTIC, value.var = "VALUE")
setcolorder(head.data_10, colorder)
setcolorder(chest.data_10, colorder) 
setcolorder(hands.data_10, colorder)
setcolorder(legs.data_10, colorder)

## Save out to rda files
usethis::use_data(
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
    overwrite = TRUE
)
