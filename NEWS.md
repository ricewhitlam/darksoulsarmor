# darksoulsarmor 2.0

## Breaking changes

* The `roll` argument of `get.optimal.armor.combos()` is now `movement`, and
  its values `"Fast"`, `"Mid"`, `"Fat"` and `"None"` are now `"Light"`,
  `"Mid"`, `"Fat"` and `"Poop"`. Each still names the heaviest movement
  allowed; `"Poop"` (over 100% equip load, where the character can't roll and
  walks slowly) means no load limit. Calls using `roll =` now fail with
  "unused argument".
* `SCORE_QUALITY` is now an exact count among every combination at the
  selected upgrade levels, rather than a normal approximation, so its values
  change. `SCORE_RAW` is unchanged.
* Stats are now exact. Every upgrade level is computed from each piece's +0
  values with the game's own upgrade rates, in 32-bit floating point as the
  game does, and strike, slash and thrust defense hold their exact values
  (e.g. 5.1499996, which the game shows as 5.1). Values are no longer rounded
  to one decimal, and some change slightly: by up to about 0.05 per piece at
  +0, and by 0.1 at some intermediate upgrade levels, where the old
  interpolation was approximate. The app still shows one decimal; its
  download holds exact values.
* The `head.data.fullupgrade`, `chest.data.fullupgrade`,
  `hands.data.fullupgrade` and `legs.data.fullupgrade` datasets are removed.
* `unarmored.weight` is replaced by `weapons`: the weapons in the four weapon
  slots, by name. The movement type is now checked exactly as the game checks
  it: equip load and weights in 32-bit floating point, with the weapons and
  then the armor added in the game's slot order. A combination sitting
  exactly on a movement line on paper can fall on either side of it, as it
  does in-game. `EQUIP_LOAD`, `ARMOR_WEIGHT` and `TOTAL_WEIGHT` are the exact
  32-bit values.
* R 4.3 or later is required.

## New features

* `get.armor.tradeoffs()` finds the best value of one stat (the score, Poise,
  or one defense or resistance) at each armor-weight limit, with the same
  settings as `get.optimal.armor.combos()`. `weight.step`, `min.armor.weight`
  and `max.armor.weight` set which limits are computed. Among combinations
  tied on the stat, the best-scoring one is returned.
* `minima` and `weights` accept named vectors in any order, e.g.
  `minima = c(POISE = 30)` or `weights = c(PHYS_DEF = 2, MAG_DEF = 1)`; stats
  left out are 0. Unnamed vectors work as before.
* `weapon.data`: every weapon, shield, bow and catalyst, with its weight.

## The app

* A new Trade-offs tab charts the most of a chosen stat that any combination
  can reach at every armor weight, every 0.1, with lines where the movement
  type changes. Hover over or click a point, or click a table row, for its set
  and links.
* "Refresh Armor Data" updates both tabs, which always show the settings of
  the last refresh.
* "Download Armor Data" saves an Excel workbook (Results, Trade-offs and
  Settings sheets) instead of a CSV.
* Four weapon dropdowns replace "Weight without Armor".
* Fixed:
  * submitting all-zero score weights no longer ends the session;
  * a warning during a refresh no longer aborts it;
  * Max Table Size is enforced on the server.

## Other changes

* Combinations with equal scores are now ordered lighter first, then more
  poise, then more durability. They are also chosen that way at the
  `max.table.size` cutoff; previously the order was arbitrary.
* The search is faster.
* `library(darksoulsarmor)` no longer loads the Shiny packages; they load when
  `armor.application()` runs.
* The scoring vignette no longer contradicts itself about comparing scores
  across different weights, and the README's install command now builds it.

# darksoulsarmor 1.0

Initial release.

* `get.optimal.armor.combos()` searches every valid head/chest/hands/legs
  combination and returns the top-scoring ones subject to equip load, area
  availability, and per-stat minimum constraints. Scores are built from ten
  standardized stats (physical/strike/slash/thrust/magic/fire/lightning
  defense, and bleed/poison/curse resistance) weighted by the `weights`
  argument - see the "scoring" vignette for the full derivation
  (`vignette("scoring")` if installed with `build_vignettes = TRUE`, or
  `vignettes/scoring.Rmd` directly otherwise).
* `get.all.armor.combos()` returns every valid combination unscored, for
  users who want to do their own analysis.
* `armor.application()` launches an interactive Shiny app built on top of
  `get.optimal.armor.combos()`.
* Full unupgraded and fully-upgraded stat data is shipped for every head,
  chest, hands, and legs piece in the game; both search functions accept
  `regular.level`/`twinkling.level` and interpolate stats to any
  intermediate upgrade level.
