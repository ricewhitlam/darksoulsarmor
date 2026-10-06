# create_rda

This directory is a **dev-only data-build step**, not part of the installed `darksoulsarmor`
package. It turns the raw armor CSVs into the `.rda` files under `data/` (the public datasets,
e.g. `head.data.unupgraded`) and `R/sysdata.rda` (the internal `means`/`stddevs`/`corrs` used to score
combinations - see `vignettes/scoring.Rmd`). Nothing in `R/` or `src/` reads from this directory
at runtime; it only needs to be re-run when the source CSVs change (a new armor piece, a stat
correction, etc.).

## Contents

- `armor_00.csv`, `armor_10.csv` - raw per-piece stats at +0 and, where applicable, max upgrade.
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
