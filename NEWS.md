# darksoulsarmor 1.0

Initial release.

* `get.optimal.armor.combos()` searches every valid head/chest/hands/legs
  combination and returns the top-scoring ones subject to equip load, area
  availability, and per-stat minimum constraints. Scores are built from ten
  standardized stats (physical/strike/slash/thrust/magic/fire/lightning
  defense, and bleed/poison/curse resistance) weighted by the `weights`
  argument - see `vignette("scoring")` for the full derivation.
* `get.all.armor.combos()` returns every valid combination unscored, for
  users who want to do their own analysis.
* `armor.application()` launches an interactive Shiny app built on top of
  `get.optimal.armor.combos()`.
* Full unupgraded and fully-upgraded stat data is shipped for every head,
  chest, hands, and legs piece in the game; both search functions accept
  `regular.level`/`twinkling.level` and interpolate stats to any
  intermediate upgrade level.
