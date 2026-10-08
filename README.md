# darksoulsarmor

An R package for finding optimized armor combinations in *Dark Souls Remastered*. It ships exact stats for every armor piece in the game, computing any upgrade level with the game's own upgrade rates, a combinatorial search that scores every valid head/chest/hands/legs combination against a set of weighted priorities and constraints, and an interactive Shiny app built on top of that search. It can also chart trade-offs: the most of a stat any combination can reach at every armor weight.

## Installation

```r
devtools::install_github("ricewhitlam/darksoulsarmor", build_vignettes = TRUE)
```

Requires R 4.3 or later.

`build_vignettes = TRUE` installs the "scoring" vignette referenced below; without it, the same content is readable on GitHub at [`vignettes/scoring.Rmd`](vignettes/scoring.Rmd).

## Quick start

The easiest way to use the package is the Shiny app:

```r
library(darksoulsarmor)
armor.application()
```

Adjust the settings in the sidebar (which armor pieces to consider, upgrade levels, rings, weapons, movement type, minimum stats, and how much each stat should matter) and click "Refresh Armor Data". The Results tab lists the top combinations. The Trade-offs tab charts the most of a chosen stat (the score, Poise, or one defense or resistance) that any combination can reach at every armor weight, with lines where the movement type changes. Click a row or point for links to the armor pieces on the Dark Souls wiki. "Download Armor Data" saves an Excel workbook of both tabs and the settings behind them. The in-app "User Guide" button explains every input in detail.

The same search is available directly as a function, for scripting or exploring results outside the app:

```r
library(darksoulsarmor)

result <- get.optimal.armor.combos(
    max.table.size = 5,
    endurance.level = 40,
    weapons = c(right.1 = "Great Club"),  # weight 12
    movement = "Mid",
    minima = c(POISE = 30)  # require at least 30 poise
)
result$data[, .(SCORE_QUALITY, HEAD, CHEST, HANDS, LEGS, ARMOR_POISE, PCT_LOAD)]
#>    SCORE_QUALITY                    HEAD          CHEST              HANDS
#>           <char>                  <char>         <char>             <char>
#> 1:  Top 1 in 193 Crown of the Great Lord      Sage Robe Smough's Gauntlets
#> 2:  Top 1 in 181           Smough's Helm Smough's Armor  Antiquated Gloves
#> 3:  Top 1 in 170                 Big Hat      Sage Robe Smough's Gauntlets
#> 4:  Top 1 in 166            Bloated Head      Sage Robe Smough's Gauntlets
#> 5:  Top 1 in 165         Ornstein's Helm Smough's Armor       Witch Gloves
#>                       LEGS ARMOR_POISE PCT_LOAD
#>                     <char>       <num>    <num>
#> 1:       Smough's Leggings          42  0.49750
#> 2: Gold-Hemmed Black Skirt          49  0.50000
#> 3:       Smough's Leggings          42  0.49750
#> 4:       Smough's Leggings          42  0.49125
#> 5: Gold-Hemmed Black Skirt          44  0.49750
```

See `?get.optimal.armor.combos` for the full set of settings (which areas/classes make a piece available, upgrade levels, movement type, ring bonuses, per-stat minimums, and per-stat weights). `get.all.armor.combos()` returns every valid combination unscored, for your own analysis.

## Trade-offs

`get.armor.tradeoffs()` finds the best value of one stat at each armor-weight limit, with the same settings as `get.optimal.armor.combos()`. For example, the lightest armor that reaches each PVE poise breakpoint with Mid movement:

```r
curve <- get.armor.tradeoffs(metric = "POISE", weight.step = 0.1, endurance.level = 40, weapons = c(right.1 = "Great Club"), movement = "Mid")
first <- sapply(c(21, 31, 46), function(poise) which(curve$data$BEST_VALUE >= poise)[1])
curve$data[first, .(ARMOR_WEIGHT_LIMIT, BEST_VALUE, HEAD, CHEST, HANDS, LEGS)]
#>    ARMOR_WEIGHT_LIMIT BEST_VALUE         HEAD    CHEST            HANDS
#>                 <num>      <num>       <char>   <char>           <char>
#> 1:                7.8         21   Giant Helm No Chest         No Hands
#> 2:               12.5         31 Havel's Helm No Chest Knight Gauntlets
#> 3:               18.7         46 Havel's Helm No Chest  Giant Gauntlets
#>                         LEGS
#>                       <char>
#> 1: Hollow Soldier Waistcloth
#> 2: Hollow Soldier Waistcloth
#> 3: Hollow Soldier Waistcloth
```

`weight.step` sets the spacing of the limits (default 1), and `min.armor.weight`/`max.armor.weight` set the range; by default it runs from 0 to what the movement type allows. Among combinations tied on the stat, the best-scoring one is returned.

`get.tradeoff.efficiency()` simplifies a curve into a few straight regions (Douglas–Peucker, by default staying within 5% of the curve's range, in at most 6 regions, none narrower than 2.5 units of armor weight) and compares each region's slope, the stat gained per unit of armor weight, with the curve's average, to show where extra weight pays off:

```r
curve <- get.armor.tradeoffs(weight.step = 0.1, endurance.level = 40, movement = "Fat")
efficiency <- get.tradeoff.efficiency(curve)
efficiency$data[, .(FROM, TO, SLOPE, RATIO_TO_AVERAGE, ABOVE_AVERAGE)]
#>     FROM    TO      SLOPE RATIO_TO_AVERAGE ABOVE_AVERAGE
#>    <num> <num>      <num>            <num>        <lgcl>
#> 1:   0.0   3.5 0.76756860        3.6273473          TRUE
#> 2:   3.5   9.3 0.39527049        1.8679547          TRUE
#> 3:   9.3  37.8 0.17870405        0.8445130         FALSE
#> 4:  37.8  52.5 0.07055733        0.3334372         FALSE
```

Here the score pays off most in the first few units of armor weight: 3.6 times the curve's average up to 3.5, falling below average past 9.3.

## What the score means

Every combination gets a score built from ten stats (physical/strike/slash/thrust/magic/fire/lightning defense, and bleed/poison/curse resistance), each standardized to mean 0 and variance 1 so that stats on very different natural scales (a 40-point armor rating vs. a 2-point resistance) contribute comparably. The `weights` argument controls how much each stat counts toward the total. The resulting `SCORE_RAW` is itself standardized, and `SCORE_QUALITY` converts that into an exact rarity among every combination at the selected upgrade levels, like `"Top 1 in 40"` or `"Bottom 1 in 40"` — see `vignette("scoring")` for exactly how that's computed. Scores are directly comparable across different filter/constraint choices as long as the weights are the same.

## Where the data comes from

The underlying armor stats live in `create_rda/*.csv` and are compiled into the package's shipped data by `create_rda/create_rda.R` — see that directory for details on the build process and its own dependencies.

## License

GPL (>= 2)
