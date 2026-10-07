# create_rda

This directory is a **dev-only data-build step**, not part of the installed `darksoulsarmor`
package. It turns the raw armor CSVs into the `.rda` files under `data/` (the public datasets,
e.g. `head.data.unupgraded`) and `R/sysdata.rda` (the internal `means`/`stddevs`/`corrs` used to score
combinations - see `vignettes/scoring.Rmd`). Nothing in `R/` or `src/` reads from this directory
at runtime; it only needs to be re-run when the source CSVs change (a new armor piece, a stat
correction, etc.).

## Contents

- `armor_00.csv` - per-piece stats at +0; every other upgrade level is computed from these by
  `get.interp.data()`, with the game's own upgrade rates. Strike, slash and thrust defense are
  the game's exact 32-bit values: the game stores them as a per-piece percentage adjustment to
  physical defense and computes them in 32-bit floating point, so e.g. Black Sorcerer Hat's
  strike defense is 5 x 103% = 5.14999962, which the game displays as 5.1. They were computed
  once from the game's own armor parameters (`EquipParamProtector`), and are written to 9
  significant digits, enough to recover each 32-bit value exactly; `create_rda.R` snaps them back
  to it.
- `weapons.csv` - every weapon, shield, bow and catalyst that can fill a weapon slot, with its
  weight (from the game's own weapon parameters, `EquipParamWeapon`; infusions and upgrades don't
  change weight).
- `armor_metainfo.csv` - piece metadata: type (head/chest/hands/legs), upgrade path
  (`None`/`Regular`/`Twinkling`), and the `AREA_MATCH_TYPE`/`AREA_LIST` columns used for
  area-of-origin filtering.
- `create_rda.R` - the script that reads the CSVs above, builds every upgrade level via
  `get.interp.data()`, computes population mean/sd/correlation across all combinations, and
  writes the resulting `.rda` files with `usethis::use_data()`.

The population statistics cover every possible four-piece combination (head x chest x hands x
legs - ~21.9 billion across all upgrade levels), but are computed without visiting them: each
combination counts once, so the four slots' pieces are independent of one another, and the mean
and covariance matrix of a combination's summed metrics are just the sums of each slot table's own
means and covariance matrices. The whole script runs in seconds. `create_rda.R` calls
`pkgload::load_all(".")` to reuse the package's own `get.interp.data()` rather than maintaining
a second copy of it here; that's a script-time convenience only, not a package dependency.

## Running it

From the package root, with `devtools` installed:

```r
source("create_rda/create_rda.R")
```

This overwrites `data/*.rda` and `R/sysdata.rda` in place. Reinstall `darksoulsarmor` afterward
to pick up the changes.
