
## The package's help page (title and description from DESCRIPTION) and its package-wide imports,
## from which roxygen writes NAMESPACE

#' @seealso
#' \code{\link{get.optimal.armor.combos}}, \code{\link{get.armor.tradeoffs}},
#' \code{\link{get.tradeoff.efficiency}}, \code{\link{get.all.armor.combos}},
#' \code{\link{armor.application}}
#'
#' @useDynLib darksoulsarmor, .registration = TRUE
#' @import data.table
#' @import Rcpp
#' @importFrom stats median
"_PACKAGE"
