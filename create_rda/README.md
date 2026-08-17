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
- `dsa.rda/` - a small standalone Rcpp helper package (see below).

## Why `dsa.rda` exists

Computing `means`, `stddevs`, and `corrs` requires summing each metric over every possible
four-piece combination (head x chest x hands x legs, all upgrade levels) - tens of millions of
combinations. `create_rda.R` parallelizes this with `doParallel`/`foreach`, and each parallel
worker is a separate R process that needs the summation code available to it. `dsa.rda` packages
that C++ summation logic (`src/create_rda.cpp`: Kahan-summed mean/variance/covariance) as an
installable package purely so `foreach(..., .packages = "dsa.rda")` can load it into each worker.

`dsa.rda` is **not** a dependency of `darksoulsarmor` in either direction - it isn't listed in
`darksoulsarmor`'s `DESCRIPTION`, and it doesn't call into `darksoulsarmor` itself. The one
place the two touch is at script-time convenience: `create_rda.R` calls `pkgload::load_all(".")`
to reuse the package's own `get.interp.data()` rather than maintaining a second copy of it here.

## Running it

From the package root, with `dsa.rda` installed (`R CMD INSTALL create_rda/dsa.rda` or
`devtools::install("create_rda/dsa.rda")`):

```r
source("create_rda/create_rda.R")
```

This overwrites `data/*.rda` and `R/sysdata.rda` in place. Reinstall `darksoulsarmor` afterward
to pick up the changes.
