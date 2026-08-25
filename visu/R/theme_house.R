#' House ggplot2 theme
#'
#' Layers a consistent title/axis/legend/gridline treatment on top of
#' [ggplot2::theme_minimal()] - a single `+ theme_house()` call standing in
#' for a repeated `theme()` block, same idea as `bbc_style()` in the BBC's
#' bbplot package (<https://github.com/bbc/bbplot>).
#'
#' @param base_size Base font size in points. Default `11`.
#'
#' @return A ggplot2 theme object, added to a plot with `+ theme_house()`.
#'
#' @export
theme_house <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(size = base_size * 1.3, face = "bold", hjust = 0),
      plot.subtitle       = element_text(size = base_size * 1.05, colour = "grey30", hjust = 0),
      plot.caption        = element_text(size = base_size * 0.75, colour = "grey50", hjust = 0),
      axis.title          = element_text(size = base_size * 0.9),
      axis.text           = element_text(size = base_size * 0.8, colour = "black"),
      legend.position     = "bottom",
      legend.title        = element_text(size = base_size * 0.85),
      legend.text         = element_text(size = base_size * 0.75),
      strip.text          = element_text(size = base_size * 0.85, face = "bold"),
      panel.grid.minor    = element_blank()
    )
}
