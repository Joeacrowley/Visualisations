# Van Gogh-inspired categorical palette for survey data visualisation
# Stray script — not yet wired into a sourcing pipeline

vangogh_pal <- c(
  night_blue  = "#1D3557",
  star_gold   = "#E9B44C",
  sage_green  = "#8A9B85",
  plum_red    = "#8C3F52",
  lavender    = "#9683A0",
  stone_brown = "#9A8567",
  pastel_pink = "#DA8B93"
)

#' Fill scale using the Van Gogh categorical palette
#' @param ... passed to ggplot2::scale_fill_manual
scale_fill_vangogh <- function(...) {
  ggplot2::scale_fill_manual(values = vangogh_pal, ...)
}

#' Colour scale using the Van Gogh categorical palette
#' @param ... passed to ggplot2::scale_colour_manual
scale_colour_vangogh <- function(...) {
  ggplot2::scale_colour_manual(values = vangogh_pal, ...)
}

# Preview
# scales::show_col(vangogh_pal)

# "blue1", 
# "gold1"
# sage
# plum
# Orange
# lavender
# grey
