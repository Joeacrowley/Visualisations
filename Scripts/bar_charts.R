# Bar chart functions ----------------------------------------------------------
# Requires: tidyverse, ncolours (from Scripts/colours.R)


# wrap_text() -----------------------------------------------------------------
# Wraps a string to fit within a ggplot plot area.
# plot_width: estimated plot width in inches
# num_labels: number of bars sharing the axis — defaults to 1, which makes
#             this suitable for titles and axis labels as well as tick labels
# char_width: approximate width of one character in inches — adjust for font size
# margin:     proportion of plot width to treat as unusable

wrap_text <- function(text, plot_width = 7, num_labels = 1, char_width = 0.09, margin = 0.1) {
  usable_width <- plot_width * (1 - margin)
  max_chars    <- floor((usable_width / num_labels) / char_width)
  str_wrap(text, width = max_chars)
}


# resolve_y_lab() -------------------------------------------------------------
# Resolves y-axis label text and theme element based on label length.
# Returns a named list: y_lab_final and y_axis_theme.
# Short labels (<=3 chars) are horizontal, longer labels are vertical and wrapped.

resolve_y_lab <- function(y_lab, plot_height = 4, base_size = 11) {
  if (!is.null(y_lab) && nchar(y_lab) > 3) {
    list(
      y_lab_final  = wrap_text(y_lab, plot_width = plot_height),
      y_axis_theme = element_text(angle = 90, hjust = 0.5, size = base_size, face = "bold")
    )
  } else if (!is.null(y_lab)) {
    list(
      y_lab_final  = y_lab,
      y_axis_theme = element_text(angle = 0, vjust = 0.5, size = base_size, face = "bold")
    )
  } else {
    list(
      y_lab_final  = NULL,
      y_axis_theme = element_text(angle = 0, vjust = 0.5, size = base_size, face = "bold")
    )
  }
}


# bar_width_from_n() ----------------------------------------------------------
# Scales bar width to the number of x-axis labels.
# Fewer bars get narrower widths to avoid them looking too fat.

bar_width_from_n <- function(num_labels) {
  case_when(
    num_labels <= 3 ~ 0.4,
    num_labels <= 5 ~ 0.6,
    num_labels <= 8 ~ 0.75,
    TRUE            ~ 0.9
  )
}


# bar() -----------------------------------------------------------------------
# Bar chart for a single categorical x numeric variable.
# x:           unquoted name of the categorical column (ideally a factor) for the x-axis
# y:           unquoted name of the numeric column for the y-axis
# x_lab:       optional character string for the x-axis title
# y_lab:       optional character string for the y-axis title — if longer than 3
#              characters the label is rotated vertical and wrapped to plot height
# title:       optional — left empty if not supplied
# fill:        bar colour — any hex colour or ncolours entry, default ncolours$blue1
# base_size:   base font size in pt — scales all text, default 11
# title_size:  title font size in pt — if NULL defaults to base_size + 2
# plot_width:  estimated plot width in inches — used for label wrapping, default 7
# plot_height: estimated plot height in inches — used for y-axis label wrapping
#              when the label is long, default 4

bar <- function(dat, x, y, x_lab = NULL, y_lab = NULL, title = NULL,
                fill = ncolours$blue1, base_size = 11, title_size = NULL, plot_width = 7, plot_height = 4) {

  fill_colour <- fill

  x_var <- as.character(substitute(x))
  y_var <- as.character(substitute(y))

  x_lab_wrap <- if (!is.null(x_lab)) wrap_text(x_lab, plot_width = plot_width) else NULL
  title_wrap <- if (!is.null(title)) wrap_text(title, plot_width = plot_width) else NULL

  y_lab_resolved <- resolve_y_lab(y_lab, plot_height, base_size)
  y_lab_final    <- y_lab_resolved$y_lab_final
  y_axis_theme   <- y_lab_resolved$y_axis_theme

  num_labels <- length(levels(fct_drop(as.factor(dat[[x_var]]))))
  bar_width  <- bar_width_from_n(num_labels)

  buffer <- mean(dat[[y_var]], na.rm = TRUE) / 4
  ylim   <- c(0, ceiling(max(dat[[y_var]], na.rm = TRUE) + buffer))

  dat %>%
    filter(!is.na(!!sym(x_var))) %>%
    mutate(
      !!sym(x_var) := map_chr(
        as.character(!!sym(x_var)),
        wrap_text,
        num_labels = num_labels,
        char_width = 0.08
      ),
      !!sym(x_var) := factor(!!sym(x_var), levels = unique(!!sym(x_var)))
    ) %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_col(fill = fill_colour, width = bar_width) +
    geom_text(aes(label = round(!!sym(y_var), 1)), vjust = -1.5, size = base_size, size.unit = "pt") +
    theme_minimal(base_size = base_size) +
    labs(
      title = title_wrap,
      x     = x_lab_wrap,
      y     = y_lab_final
    ) +
    coord_cartesian(ylim = ylim) +
    theme(
      plot.margin  = margin(1, 1, 1, 1),
      plot.title   = element_text(size = if (is.null(title_size)) base_size + 2 else title_size, face = "bold"),
      axis.title   = element_text(size = base_size, face = "bold"),
      axis.title.y = y_axis_theme,
      axis.text.x  = element_text(margin = margin(b = 10), size = base_size),
      axis.text.y  = element_text(margin = margin(l = 10), size = base_size)
    )

}


# bar_clustered() -------------------------------------------------------------
# Clustered bar chart for a categorical x numeric variable, grouped by a third
# factor variable displayed as fill colour.
# x:           unquoted name of the categorical column for the x-axis
# y:           unquoted name of the numeric column for the y-axis
# fill:        unquoted name of the grouping variable mapped to fill colour
# x_lab:       optional character string for the x-axis title
# y_lab:       optional character string for the y-axis title
# fill_lab:    optional character string for the legend title
# title:       optional — left empty if not supplied
# palette:     colour vector for fill, default ncolours$colours_pairs
# base_size:   base font size in pt — scales all text, default 11
# title_size:  title font size in pt — if NULL defaults to base_size + 2
# plot_width:  estimated plot width in inches — used for label wrapping, default 7
# plot_height: estimated plot height in inches — used for y-axis label wrapping
#              when the label is long, default 4

bar_clustered <- function(dat, x, y, fill, x_lab = NULL, y_lab = NULL,
                          fill_lab = NULL, title = NULL,
                          palette = ncolours$colours_pairs, base_size = 11, title_size = NULL,
                          plot_width = 7, plot_height = 4) {

  x_var    <- as.character(substitute(x))
  y_var    <- as.character(substitute(y))
  fill_var <- as.character(substitute(fill))

  x_lab_wrap <- if (!is.null(x_lab)) wrap_text(x_lab, plot_width = plot_width) else NULL
  title_wrap <- if (!is.null(title)) wrap_text(title, plot_width = plot_width) else NULL

  y_lab_resolved <- resolve_y_lab(y_lab, plot_height, base_size)
  y_lab_final    <- y_lab_resolved$y_lab_final
  y_axis_theme   <- y_lab_resolved$y_axis_theme

  num_labels      <- length(levels(fct_drop(as.factor(dat[[x_var]]))))
  num_fill_levels <- length(levels(fct_drop(as.factor(dat[[fill_var]]))))

  buffer <- mean(dat[[y_var]], na.rm = TRUE) / 4
  ylim   <- c(0, ceiling(max(dat[[y_var]], na.rm = TRUE) + buffer))

  dat %>%
    filter(!is.na(!!sym(x_var))) %>%
    mutate(
      !!sym(x_var) := map_chr(
        as.character(!!sym(x_var)),
        wrap_text,
        num_labels = num_labels,
        char_width = 0.08
      ),
      !!sym(x_var)    := factor(!!sym(x_var), levels = unique(!!sym(x_var))),
      !!sym(fill_var) := map_chr(
        as.character(!!sym(fill_var)),
        wrap_text,
        plot_width = plot_width,
        num_labels = num_fill_levels
      ),
      !!sym(fill_var) := as.factor(!!sym(fill_var))
    ) %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_col(position = "dodge2") +
    geom_text(
      aes(label = round(!!sym(y_var), 1)),
      position = position_dodge2(0.9),
      vjust = -1.5,
      size = base_size,
      size.unit = "pt"
    ) +
    scale_fill_manual(values = palette) +
    theme_minimal(base_size = base_size) +
    labs(
      title = title_wrap,
      x     = x_lab_wrap,
      y     = y_lab_final,
      fill  = fill_lab
    ) +
    coord_cartesian(ylim = ylim) +
    theme(
      plot.margin  = margin(1, 1, 1, 1),
      plot.title   = element_text(size = if (is.null(title_size)) base_size + 2 else title_size, face = "bold"),
      axis.title   = element_text(size = base_size, face = "bold"),
      axis.title.y = y_axis_theme,
      axis.text.x  = element_text(margin = margin(b = 10), size = base_size),
      axis.text.y  = element_text(margin = margin(l = 10), size = base_size),
      legend.title    = element_text(size = base_size, face = "bold"),
      legend.text     = element_text(size = base_size),
      legend.position = "bottom"
    )

}


# bar_stacked() ---------------------------------------------------------------
# Stacked horizontal bar chart for a categorical x numeric variable,
# grouped by a third factor variable displayed as fill colour.
# x:           unquoted name of the categorical column for the x-axis
# y:           unquoted name of the numeric column for the y-axis
# fill:        unquoted name of the grouping variable mapped to fill colour
# x_lab:       optional character string for the x-axis title
# y_lab:       optional character string for the y-axis title
# fill_lab:    optional character string for the legend title
# title:       optional — left empty if not supplied
# palette:         colour vector for fill, default ncolours$colours10
# base_size:       base font size in pt — scales all text, default 11
# title_size:      title font size in pt — if NULL defaults to base_size + 2
# legend_position: "bottom" (default) or "right" — use "right" for long fill labels
# plot_width:      estimated plot width in inches — used for label wrapping, default 7
# plot_height:     estimated plot height in inches — used for y-axis label wrapping,
#                  default 4

bar_stacked <- function(dat, x, y, fill, x_lab = NULL, y_lab = NULL,
                        fill_lab = NULL, title = NULL,
                        palette = ncolours$colours10, base_size = 11, title_size = NULL,
                        legend_position = "bottom", plot_width = 7, plot_height = 4) {

  x_var    <- as.character(substitute(x))
  y_var    <- as.character(substitute(y))
  fill_var <- as.character(substitute(fill))

  x_lab_wrap <- if (!is.null(x_lab)) wrap_text(x_lab, plot_width = plot_width * 0.2) else NULL
  y_lab_wrap <- if (!is.null(y_lab)) wrap_text(y_lab, plot_width = plot_width) else NULL
  title_wrap <- if (!is.null(title)) wrap_text(title, plot_width = plot_width) else NULL

  num_labels      <- length(levels(fct_drop(as.factor(dat[[x_var]]))))
  num_fill_levels <- length(levels(fct_drop(as.factor(dat[[fill_var]]))))
  bar_width       <- bar_width_from_n(num_labels)

  buffer <- mean(dat[[y_var]], na.rm = TRUE) / 4
  ylim   <- c(0, ceiling(max(dat[[y_var]], na.rm = TRUE) + buffer))

  dat %>%
    filter(!is.na(!!sym(x_var))) %>%
    mutate(
      !!sym(x_var) := map_chr(
        as.character(!!sym(x_var)),
        wrap_text,
        num_labels = num_labels,
        char_width = 0.08
      ),
      !!sym(x_var)    := factor(!!sym(x_var), levels = unique(!!sym(x_var))),
      !!sym(fill_var) := map_chr(
        as.character(!!sym(fill_var)),
        wrap_text,
        plot_width = plot_width,
        num_labels = num_fill_levels
      ),
      !!sym(fill_var) := as.factor(!!sym(fill_var))
    ) %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_col(position = "stack", width = bar_width) +
    geom_text(
      aes(label = round(!!sym(y_var), 1)),
      position = position_stack(vjust = 0.5),
      size = base_size,
      size.unit = "pt"
    ) +
    scale_fill_manual(values = palette) +
    coord_flip() +
    theme_minimal(base_size = base_size) +
    labs(
      title = title_wrap,
      x     = x_lab_wrap,
      y     = y_lab_wrap,
      fill  = fill_lab
    ) +
    theme(
      plot.margin  = margin(1, 1, 1, 1),
      plot.title   = element_text(size = if (is.null(title_size)) base_size + 2 else title_size, face = "bold"),
      axis.title   = element_text(size = base_size, face = "bold"),
      axis.title.y = element_text(angle = 0, vjust = 0.5, size = base_size, face = "bold"),
      axis.text.x  = element_text(margin = margin(b = 10), size = base_size),
      axis.text.y  = element_text(margin = margin(l = 10), size = base_size),
      legend.title     = element_text(size = base_size, face = "bold"),
      legend.text      = element_text(size = base_size),
      legend.position  = legend_position,
      legend.direction = if (legend_position == "bottom") "horizontal" else "vertical"
    )

}
