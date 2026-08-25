# =============================================================================
# visu-package.R — package-level documentation and the blanket @import
# declarations every other file in R/ relies on.
#
# Same reasoning as dtab's own dtab-package.R (see that file's header note in
# full): every file here was written with UNQUALIFIED ggplot2/dplyr/purrr/
# stringr/forcats calls (mutate(), filter(), ggplot(), geom_col(), str_wrap(),
# fct_drop(), ...), from being loaded via source() + library(tidyverse) +
# library(ggplot2) at each .qmd's own top rather than as a package. Rewriting
# every call site to @importFrom-and-qualify individually, without R
# available here to catch a missed one, would be a large, high-risk
# mechanical rewrite for very little benefit in an internal-use package - so
# this package imports the whole namespace of each component actually called
# unqualified anywhere in R/, exactly reproducing what those library() calls
# already did.
#
# rlang is the one deliberate exception, same carve-out dtab makes and for
# the same reason: @import rlang collides with purrr on several identically-
# named re-exported functions (flatten*, splice, invoke, %@%). The only bare
# rlang usage in this package is sym() and := (bar()/bar_clustered()/
# bar_stacked()'s tidy-eval column renaming), so those two are
# @importFrom-ed individually instead of importing the whole namespace.
#
# colorRampPalette() (nc_alt1.R's make_tint_ramp(), and the interpolated
# branch of scale_*_pal_sequential()) is base R's grDevices package, called
# bare - works fine at runtime since grDevices is always attached for every
# R session, but R CMD check still wants it declared, same as dtab needed
# for stats/utils (see that package's own dtab-package.R note) - hence the
# @importFrom below and grDevices in DESCRIPTION's Imports.
#
# No utils::globalVariables() call needed here, unlike dtab - every dynamic
# column reference in bar_charts.R goes through !!sym(x_var) rather than a
# bare column-name symbol, so there's nothing left for codetools' static
# analysis to flag as an apparent undefined global.
#
# NOTE: after any change to these @import/@export tags (or to any other
# roxygen block in this package), run devtools::document() to regenerate
# NAMESPACE and the Rd files - nothing here is hand-maintained.
#
#' @keywords internal
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import forcats
#' @importFrom rlang sym :=
#' @importFrom grDevices colorRampPalette
"_PACKAGE"
