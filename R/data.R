
## data.table columns are referenced by bare name via non-standard evaluation (e.g. dt[, ARMOR]),
## and head.data.unupgraded etc. are lazy-loaded package datasets referenced by bare name in
## default argument values and function bodies - neither is visible to R CMD check's static analysis.
utils::globalVariables(c(
    "AREAFILTER", "AREA_MATCH_TYPE", "AREA_LIST", "ARMOR", "SCORE", "SCORE_RAW", "SCORE_QUALITY",
    "STARTING_CLASS", "UPGRADE_TYPE", "WEIGHT",
    "areas", "classes", "means", "stddevs", "corrs",
    "head.data.unupgraded", "head.data.fullupgrade", "chest.data.unupgraded", "chest.data.fullupgrade",
    "hands.data.unupgraded", "hands.data.fullupgrade", "legs.data.unupgraded", "legs.data.fullupgrade",
    "METRICS", "metric", "weight.index", "minima.index"
))

## Canonical identity and order of every armor metric - the single source of truth for the
## metric-order lists in get.optimal.armor.combos (score.cols/min.cols were previously two
## independently hand-typed vectors) and for the position each metric occupies in the
## `minima` (all 12 metrics) and `weights` (the 10 that are scored) arguments. Keeping these
## positions in one place, rather than as parallel hand-maintained literals, is what prevents
## them from drifting out of sync with each other - which is how the minima-indexing bug fixed
## earlier in this package's history happened.
METRICS <-
    data.table::data.table(
        metric = c(
            "PHYS_DEF", "STRIKE_DEF", "SLASH_DEF", "THRUST_DEF",
            "MAG_DEF", "FIRE_DEF", "LITNG_DEF", "POISE",
            "BLEED_RES", "POIS_RES", "CURSE_RES", "DURABILITY"
        ),
        minima.index = 1:12,
        weight.index = c(1:7, NA, 8:10, NA)
    )

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

## means, stddevs, and corrs (R/sysdata.rda) are the population mean, standard deviation, and
## correlation matrix of PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF,
## LITNG_DEF, BLEED_RES, POIS_RES, and CURSE_RES across every possible armor combination
## (including upgrades) - see create_rda/create_rda.R for how they're derived. They exist only
## to normalize the score formula in get.optimal.armor.combos and are not meant to be used
## directly, so unlike areas/classes they are internal (sysdata.rda) rather than exported data.
## total.combo.count (R/sysdata.rda) is the exact count of armor combinations that population is
## drawn from (~21.9 billion, across every upgrade level). Not currently used by any exported
## function - SCORE_QUALITY works entirely from SCORE_RAW's normal-tail probability, independent
## of the true population size - but kept here as a documented, non-duplicated fact for anyone
## who wants it (e.g. to sanity-check how large a SCORE_QUALITY denominator can meaningfully get).
## mean.stddev.corr.list (R/sysdata.rda) holds the same shape of data as means/stddevs/corrs, but
## broken out WITHIN each individual (regular.level, twinkling.level) pair rather than pooled
## across every level - i.e. the population where every armor slot is evaluated at the same
## upgrade level simultaneously, matching how a player thinks of their loadout ("I'm using +5
## regular titanite gear"), rather than each slot's own upgrade increment varying independently
## of the others (which is what means/stddevs/corrs pool over). A named list of 67 entries: one
## \code{list(means, stddevs, corrs)} per "{regular.level}_{twinkling.level}" combination (e.g.
## \code{"3_2"}), plus one keyed "overall" holding the same values as means/stddevs/corrs. Not
## currently used by any exported function - kept for potential future use (e.g. an
## upgrade-level-aware scoring model).

#' Table of all unupgraded head armor pieces
#'
#' @name head.data.unupgraded
#' @format
#' \describe{
#'   \item{ARMOR}{Name of armor piece}
#'   \item{UPGRADE_TYPE}{One of \code{"Regular"} (upgrades with regular titanite), \code{"Twinkling"} (upgrades with twinkling titanite), or \code{"None"} (cannot be upgraded).}
#'   \item{STARTING_CLASS}{If relevant, the starting class associated with the armor piece. Otherwise, \code{"N/A"}.}
#'   \item{AREA_MATCH_TYPE, AREA_LIST}{Together, encode which areas make the piece available. Each holds one or more \code{"|"}-separated clauses combined with OR. A clause in \code{AREA_MATCH_TYPE} is \code{"ALWAYS"} (always available; its \code{AREA_LIST} clause is empty), \code{"ANY"} (available once at least one of its \code{";"}-separated \code{AREA_LIST} areas is completed), or \code{"ALL"} (available once every one of its areas is completed).}
#'   \item{LINK}{A link to the armor piece on darksouls.wikidot.com.}
#'   \item{PHYS_DEF, STRIKE_DEF, SLASH_DEF, THRUST_DEF, MAG_DEF, FIRE_DEF, LITNG_DEF, POISE, BLEED_RES, POIS_RES, CURSE_RES, DURABILITY, WEIGHT}{The value of the relevant metric.}
#'   \item{STAM_MOD}{The additive modifier to the base stamina regeneration rate of 45 per second.}
#'   \item{SOUND_MOD}{The multiplicative modifier to base character loudness.}
#' }
#' @docType data
#' @keywords data
NULL

#' Table of all unupgraded chest armor pieces
#'
#' @name chest.data.unupgraded
#' @format
#' See \code{\link{head.data.unupgraded}}
#' @docType data
#' @keywords data
NULL

#' Table of all unupgraded hand armor pieces
#'
#' @name hands.data.unupgraded
#' @format
#' See \code{\link{head.data.unupgraded}}
#' @docType data
#' @keywords data
NULL

#' Table of all unupgraded leg armor pieces
#'
#' @name legs.data.unupgraded
#' @format
#' See \code{\link{head.data.unupgraded}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded head armor pieces
#'
#' @name head.data.fullupgrade
#' @format
#' See \code{\link{head.data.unupgraded}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded chest armor pieces
#'
#' @name chest.data.fullupgrade
#' @format
#' See \code{\link{head.data.unupgraded}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded hand armor pieces
#'
#' @name hands.data.fullupgrade
#' @format
#' See \code{\link{head.data.unupgraded}}
#' @docType data
#' @keywords data
NULL

#' Table of all fully upgraded leg armor pieces
#'
#' @name legs.data.fullupgrade
#' @format
#' See \code{\link{head.data.unupgraded}}
#' @docType data
#' @keywords data
NULL
