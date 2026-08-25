#' Save a ggplot to file with consistent export dimensions
#'
#' Thin wrapper around [ggplot2::ggsave()] - analogous to bbplot's
#' `finalise_plot()` (<https://github.com/bbc/bbplot>), minus the logo-block
#' step, so every exported chart uses the same width/height/resolution
#' defaults rather than repeating `ggsave()` arguments at every call site.
#'
#' @param plot A ggplot object.
#' @param save_filepath Full path to save to, including extension (e.g.
#'   `"figures/chart.png"`).
#' @param width_cm Width in cm. Default `25`.
#' @param height_cm Height in cm. Default `15`.
#' @param dpi Resolution. Default `300` (print-ready).
#'
#' @return Invisibly returns `save_filepath`; called for its side effect
#'   (writes the file).
#'
#' @export
finalise_plot <- function(plot, save_filepath, width_cm = 25, height_cm = 15, dpi = 300) {
  ggsave(
    filename = save_filepath,
    plot     = plot,
    width    = width_cm,
    height   = height_cm,
    units    = "cm",
    dpi      = dpi
  )

  invisible(save_filepath)
}
