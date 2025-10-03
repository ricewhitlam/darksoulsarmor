
#' Character vector of areas in Dark Souls
#'
#' @name areas
#' @format 
#' This is not an official or complete listing of areas in the game.
#' Rather, it is the underlying list which is used to indicate 
#' completed areas to \code{\link{get.optimal.armor.combos}}. 
#' @docType data
#' @keywords data
NULL

#' Character vector of starting classes in Dark Souls
#'
#' @name classes
#' @format 
#' This is the underlying list which is used to indicate
#' starting class to \code{\link{get.optimal.armor.combos}}. 
#' @docType data
#' @keywords data
NULL

#' List of metrics for every possible upgrade combination 
#'
#' @name mean.stddev.corr.list
#' @format 
#' This is a list of length (10+1)*(5+1) = 66. Each entry in this list is itself a list. 
#' Each of these lists holds a vector of means, a vector of standard deviations, and a correlation matrix for the following ten metrics:
#' PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF, LITNG_DEF, BLEED_RES, POIS_RES, CURSE_RES.
#' The length of 66 reflects all possible upgrade combinations between regular titanite and twinkling titanite.
#' @docType data
#' @keywords data
NULL

#' Table of all unupgraded head armor pieces
#'
#' @name head.data_00
#' @format 
#' \describe{
#'   \item{ARMOR}{Name of armor piece}
#'   \item{UPGRADE_TYPE}{One of \code{"Regular"} (upgrades with regular titanite), \code{"Twinkling"} (upgrades with twinkling titanite), or \code{"None"} (cannot be upgraded).}
#'   \item{STARTING_CLASS}{If relevant, the starting class associated with the armor piece. Otherwise, \code{"N/A"}.}
#'   \item{AREA_FORMULA}{An R expression stored as a character which can be applied to a list of areas via \code{eval(parse())} to determine whether the armor piece is available.}
#'   \item{LINK}{A link to the armor piece on darksouls.wikidot.com.}
#'   \item{PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF, LITNG_DEF, BLEED_RES, POIS_RES, CURSE_RES, DURABILITY, WEIGHT}{The value of the relevant metric.}
#' }
#' @docType data
#' @keywords data
NULL

#' Table of all unupgraded chest armor pieces
#'
#' @name chest.data_00
#' @format 
#' See \code{\link{head.data_00}}
#' @docType data
#' @keywords data
NULL

#' Table of all unupgraded hand armor pieces
#'
#' @name hands.data_00
#' @format 
#' See \code{\link{head.data_00}}
#' @docType data
#' @keywords data
NULL

#' Table of all unupgraded leg armor pieces
#'
#' @name legs.data_00
#' @format 
#' See \code{\link{head.data_00}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded head armor pieces
#'
#' @name head.data_10
#' @format 
#' See \code{\link{head.data_00}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded chest armor pieces
#'
#' @name chest.data_10
#' @format 
#' See \code{\link{head.data_00}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded hand armor pieces
#'
#' @name hands.data_10
#' @format 
#' See \code{\link{head.data_00}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded leg armor pieces
#'
#' @name legs.data_10
#' @format 
#' See \code{\link{head.data_00}}
#' @docType data
#' @keywords data
NULL
