# darksoulsarmor

An R package for finding optimized armor combinations in *Dark Souls Remastered*. It ships full stat data for every armor piece in the game (unupgraded and fully upgraded), a combinatorial search that scores every valid head/chest/hands/legs combination against a set of weighted priorities and constraints, and an interactive Shiny app built on top of that search.

## Installation

```r
devtools::install_github("ricewhitlam/darksoulsarmor")
```

## Quick start

The easiest way to use the package is the Shiny app:

```r
library(darksoulsarmor)
armor.application()
```

Adjust the filters in the sidebar (which armor pieces to consider, upgrade level, equip load constraints, minimum stats, and how much each stat should matter) and click "Refresh Armor Data" to see the top combinations. Click a row for links to the relevant armor pieces on the Dark Souls wiki. The in-app "User Guide" button explains every input in detail.

The same search is available directly as a function, for scripting or exploring results outside the app:

```r
library(darksoulsarmor)

result <- get.optimal.armor.combos(
    max.table.size = 5,
    endurance.level = 40,
    unarmored.weight = 12,
    roll = "Mid",
    minima = c(0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0)  # require at least 30 poise
)
result$data[, .(SCORE_PCT, HEAD, CHEST, HANDS, LEGS, ARMOR_POISE, PCT_LOAD)]
#>    SCORE_PCT                    HEAD          CHEST              HANDS
#>        <num>                  <char>         <char>             <char>
#> 1: 0.9968520 Crown of the Great Lord      Sage Robe Smough's Gauntlets
#> 2: 0.9965136           Smough's Helm Smough's Armor  Antiquated Gloves
#> 3: 0.9961734                 Big Hat      Sage Robe Smough's Gauntlets
#> 4: 0.9960122            Bloated Head      Sage Robe Smough's Gauntlets
#> 5: 0.9959529         Ornstein's Helm Smough's Armor       Witch Gloves
#>                        LEGS ARMOR_POISE PCT_LOAD
#>                      <char>       <num>    <num>
#> 1:       Smough's Leggings          42  0.49750
#> 2: Gold-Hemmed Black Skirt          49  0.50000
#> 3:       Smough's Leggings          42  0.49750
#> 4:       Smough's Leggings          42  0.49125
#> 5: Gold-Hemmed Black Skirt          44  0.49750
```

See `?get.optimal.armor.combos` for the full set of filters (which areas/classes make a piece available, upgrade level, ring bonuses, per-stat minimums, and per-stat weights).

## What the score means

Every combination gets a score built from ten stats (physical/strike/slash/thrust/magic/fire/lightning defense, and bleed/poison/curse resistance), each standardized to mean 0 and variance 1 so that stats on very different natural scales (a 40-point armor rating vs. a 2-point resistance) contribute comparably. The `weights` argument controls how much each stat counts toward the total. The resulting `SCORE_RAW` is itself standardized, and `SCORE_PCT` converts that into a percentile: 100% is the best score achievable for the weights given, 0% the worst, and scores are directly comparable across different filter/constraint choices as long as the weights are the same.

## Where the data comes from

The underlying armor stats live in `create_rda/*.csv` and are compiled into the package's shipped data by `create_rda/create_rda.R` — see that directory for details on the build process and its own dependencies.

## License

GPL (>= 2)
