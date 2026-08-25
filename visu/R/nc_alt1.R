# nc_alt1.R
#
# nc_alt1: a work-compatible alternative colour palette - five colour
# families plus grey, each a five-step tint ramp, matching the shape of a
# typical work brand palette (five colours + grey, five shades each) so it
# can be swapped in for one wherever the real palette isn't appropriate to
# use.
#
# Softened to a shared, fairly muted saturation curve (S38 down to S30
# across the five tiers) so all five families sit at a consistent visual
# weight. Hue held fixed across all five tiers of every family, so every
# shade reads as a tint of the same colour rather than drifting hue as it
# lightens.
#
# orange's hue was shifted from H12 to H30 (each shade's saturation/
# lightness left as-is) - at H12 it sat only ~15 degrees from pink's H357,
# which read fine in bars but didn't differentiate well from pink as small
# scatter points/lines, where hue is the main cue and there's no fill area
# to help. H30 (true orange, midway between red and yellow) roughly doubles
# that gap.

# make_tint_ramp(): internal - builds an n-step tint ramp from a base colour
# toward white. Not exported; nc_alt1's own five families already give every
# family/shade combination that's meant to be used directly.
make_tint_ramp <- function(base_colour, n = 5, max_lightening = 0.85) {
  ramp <- colorRampPalette(c(base_colour, "white"))(101)
  idx  <- round(seq(1, 1 + max_lightening * 100, length.out = n))
  ramp[idx]
}

#' NC alternative colour palette
#'
#' A work-compatible alternative colour palette - five colour families
#' (`blue`, `pink`, `mint`, `purple`, `orange`) plus a standalone `grey`,
#' each a five-step tint ramp from most saturated (`[1]`) to palest (`[5]`).
#' Feeds every `scale_*_pal_*()` function in this package, and
#' [resolve_pal_colour()] for picking a single shade directly by name.
#'
#' @format A named list of 6 character vectors (`blue`, `pink`, `mint`,
#'   `purple`, `orange`, `grey`), each 5 hex colour strings long, ordered
#'   most saturated to palest.
#'
#' @export
nc_alt1 <- list(
  blue   = c("#8880C6", "#9D96CF", "#B4AFD9", "#C8C5E2", "#E0DEED"),
  pink   = c("#C88487", "#D29DA0", "#DDB6B8", "#E7CFD0", "#F3E8E8"),
  mint   = c("#367278", "#4C9DA4", "#74B8BE", "#A6CFD3", "#D4E6E8"),
  purple = c("#A553B2", "#B573BF", "#C99AD0", "#D9BBDD", "#EBDEED"),
  orange = c("#B0804F", "#BF9973", "#CEB297", "#DDCCBB", "#EDE6DE"),
  grey   = make_tint_ramp("#616161")
)

#' Resolve an nc_alt1 colour name to a hex value
#'
#' Resolves a family name, optionally with a shade digit attached, to its
#' [nc_alt1] hex value - `"blue"` and `"blue1"` both give `nc_alt1$blue[1]`
#' (family name alone defaults to shade 1), `"grey3"` gives `nc_alt1$grey[3]`,
#' and so on. Centralises the family+shade naming convention in one place so
#' other functions (e.g. [bar()]) can take colours as plain strings without
#' reaching into [nc_alt1] directly.
#'
#' @param name A colour name string - one of `nc_alt1`'s family names
#'   (`"blue"`, `"pink"`, `"mint"`, `"purple"`, `"orange"`, `"grey"`),
#'   optionally with a shade digit 1-5 attached (e.g. `"blue3"`).
#'
#' @return A single hex colour string.
#'
#' @export
resolve_pal_colour <- function(name) {
  families <- names(nc_alt1)
  pattern  <- paste0("^(", paste(families, collapse = "|"), ")([1-5]?)$")

  if (!grepl(pattern, name)) {
    valid <- c(families, paste0(rep(families, each = 5), 1:5))
    stop("'", name, "' isn't a valid colour name. Valid names: ", paste(valid, collapse = ", "))
  }

  family <- sub(pattern, "\\1", name)
  shade  <- sub(pattern, "\\2", name)
  shade  <- if (shade == "") 1 else as.integer(shade)

  nc_alt1[[family]][shade]
}

# gradient_pal(): internal - low/high hex pair for a single-hue continuous
# scale, for any nc_alt1 family. Used by scale_fill_pal_gradient() and by
# scale_*_pal_sequential()'s interpolated (n > 5) branch. Not exported -
# scale_fill_pal_gradient() is the public entry point for this.
gradient_pal <- function(family) {
  list(low = nc_alt1[[family]][5], high = nc_alt1[[family]][1])
}

# .categorical_tier_colours_pal(): internal - the tier-selection logic
# give_categorical_colours_pal() builds on. Not exported or documented on
# its own; give_categorical_colours_pal() is the public entry point.
.categorical_tier_colours_pal <- function(n, primary, families) {
  k <- length(families)
  idx <- if (n <= k)        primary
         else if (n <= 2*k) c(2, 4)
         else if (n <= 3*k) c(1, 3, 5)
         else if (n <= 4*k) c(1, 2, 4, 5)
         else               c(1, 2, 3, 4, 5)

  colours <- unlist(lapply(idx, function(tier) {
    vapply(families, function(f) nc_alt1[[f]][tier], character(1))
  }), use.names = FALSE)

  colours[seq_len(n)]
}

#' Pull n colours from nc_alt1 for an unordered categorical variable
#'
#' Draws one colour per family, spreading across hues before repeating
#' within one - each level reads as visually distinct from the others, with
#' no implied sequence between them. `grey` is a valid, selectable family
#' here (unlike some house palettes), so with enough categories it can turn
#' up as one of the fill colours, not just as a neutral extra. Powers
#' [scale_fill_pal_categorical()]/[scale_colour_pal_categorical()], and
#' [bar()]'s own `"multi"`/`"<family>_seq"` fill_family options.
#'
#' @param n Number of colours to return. Default `12`.
#' @param primary Which of a family's 5 shades to use first, when `n` is
#'   small enough that only one tier per family is needed. Default `1`.
#' @param families Character vector of [nc_alt1] family names to draw from.
#'   Default all 6.
#' @param allow_spillover If `TRUE` (default), running out of the selected
#'   families' own colours tops up from the rest of [nc_alt1]. If `FALSE`,
#'   requesting more than the selected families can supply errors instead -
#'   for callers (e.g. [bar()]'s `"<family>_seq"` option) where mixing in
#'   another family would defeat the point.
#'
#' @return A character vector of `n` hex colour strings.
#'
#' @export
give_categorical_colours_pal <- function(n = 12, primary = 1,
                                          families = names(nc_alt1),
                                          allow_spillover = TRUE) {

  all_families <- names(nc_alt1)

  invalid_families <- setdiff(families, all_families)
  if (length(invalid_families) > 0) {
    stop("'", paste(invalid_families, collapse = "', '"), "' isn't a valid family. Valid families: ", paste(all_families, collapse = ", "))
  }

  if (n > length(all_families) * 5) {
    stop("n (", n, ") exceeds the maximum of ", length(all_families) * 5, " available colours.")
  }

  n_selected <- min(n, length(families) * 5)

  if (!allow_spillover && n > n_selected) {
    stop("n (", n, ") exceeds the ", n_selected, " colours available from the selected families (", paste(families, collapse = ", "), ") - spillover into other families is disabled.")
  }

  colours <- .categorical_tier_colours_pal(n_selected, primary, families)

  if (n > n_selected) {
    remaining <- setdiff(all_families, families)
    colours <- c(colours, .categorical_tier_colours_pal(n - n_selected, primary, remaining))
  }

  colours
}

#' Categorical fill/colour scales from nc_alt1
#'
#' For an unordered categorical variable - each level just needs to read as
#' visually distinct from the others, with no implied sequence between them.
#' Colours come from [give_categorical_colours_pal()]; see that function for
#' how many colours per family and in what order.
#'
#' @param n Number of levels to colour. Default `12`.
#' @param primary Which of a family's 5 shades to use first, when `n` is
#'   small enough that only one tier per family is needed. Default `1`.
#' @param families Character vector of [nc_alt1] family names to draw from.
#'   Default all 6 (`grey` included).
#'
#' @return A ggplot2 scale object (`scale_fill_manual()`/
#'   `scale_colour_manual()` under the hood).
#'
#' @export
scale_fill_pal_categorical <- function(n = 12, primary = 1,
                                        families = names(nc_alt1)) {
  ggplot2::scale_fill_manual(values = give_categorical_colours_pal(n, primary, families))
}

#' @rdname scale_fill_pal_categorical
#' @export
scale_colour_pal_categorical <- function(n = 12, primary = 1,
                                          families = names(nc_alt1)) {
  ggplot2::scale_colour_manual(values = give_categorical_colours_pal(n, primary, families))
}

#' Sequential (single-hue) fill/colour scales from nc_alt1
#'
#' For levels that all belong to *one* colour family rather than being
#' spread across several - e.g. sub-categories of a single concept, or an
#' ordered/ranked categorical variable (a Likert scale, say), where staying
#' within one hue and varying only tint signals they're related and ordered,
#' rather than distinct topics.
#'
#' Up to 5 levels, this pulls the family's 5 discrete shades (via
#' [give_categorical_colours_pal()], restricted to one family); past 5, a
#' family only has 5 real shades to give, so this switches to interpolating
#' a continuous ramp between the family's palest and most saturated shade
#' instead (via the family's own internal low/high pair), rather than spilling into another family
#' or erroring.
#'
#' @param family A single [nc_alt1] family name (e.g. `"blue"`, `"mint"`).
#' @param n Number of levels to colour. No cap - interpolates past 5.
#'
#' @return A ggplot2 scale object (`scale_fill_manual()`/
#'   `scale_colour_manual()` under the hood).
#'
#' @export
scale_fill_pal_sequential <- function(family, n) {
  values <- if (n <= 5) {
    give_categorical_colours_pal(n, primary = 1, families = family)
  } else {
    gp <- gradient_pal(family)
    colorRampPalette(c(gp$low, gp$high))(n)
  }
  ggplot2::scale_fill_manual(values = values)
}

#' @rdname scale_fill_pal_sequential
#' @export
scale_colour_pal_sequential <- function(family, n) {
  values <- if (n <= 5) {
    give_categorical_colours_pal(n, primary = 1, families = family)
  } else {
    gp <- gradient_pal(family)
    colorRampPalette(c(gp$low, gp$high))(n)
  }
  ggplot2::scale_colour_manual(values = values)
}

#' Continuous gradient fill scale from nc_alt1
#'
#' For continuous numeric data with no meaningful midpoint - a single
#' low-to-high colour ramp within one [nc_alt1] family, e.g. a raw rate or
#' count mapped onto a map or heatmap.
#'
#' @param family A single [nc_alt1] family name. Default `"pink"`.
#'
#' @return A ggplot2 `scale_fill_gradient()` object.
#'
#' @export
scale_fill_pal_gradient <- function(family = "pink") {
  gp <- gradient_pal(family)
  ggplot2::scale_fill_gradient(low = gp$low, high = gp$high)
}

#' Diverging fill scale from nc_alt1
#'
#' For continuous numeric data with a meaningful reference point (e.g. zero,
#' or a national average) - two hues diverging in opposite directions from a
#' shared midpoint, so values above and below the reference are immediately
#' distinguishable by colour as well as sign. Always pink/mint either side
#' of grey - the one scale here with no `family`/`families` argument.
#'
#' @param midpoint The reference value the two hues diverge from. Default `0`.
#'
#' @return A ggplot2 `scale_fill_gradient2()` object.
#'
#' @export
scale_fill_pal_diverging <- function(midpoint = 0) {
  ggplot2::scale_fill_gradient2(
    low = nc_alt1$pink[1], mid = nc_alt1$grey[5],
    high = nc_alt1$mint[1], midpoint = midpoint
  )
}
