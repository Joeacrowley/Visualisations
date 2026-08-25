# Bar chart functions ----------------------------------------------------------

# wrap_text(): internal - wraps a string to fit within a ggplot plot area.
# plot_width: estimated plot width in inches
# num_labels: number of bars sharing the axis — defaults to 1, which makes
#             this suitable for titles and axis titles as well as the
#             individual category labels themselves
# char_width: approximate width of one character in inches — adjust for font size
# margin:     proportion of plot width to treat as unusable
# Not exported or documented on its own - an implementation detail of
# bar()/bar_clustered()/bar_stacked()'s own label handling.
wrap_text <- function(text, plot_width = 7, num_labels = 1, char_width = 0.09, margin = 0.1) {
  usable_width <- plot_width * (1 - margin)
  max_chars    <- floor((usable_width / num_labels) / char_width)
  str_wrap(text, width = max_chars)
}

# resolve_y_lab(): internal - resolves y-axis label text and theme element
# based on label length. Returns a named list: y_lab_final and y_axis_theme.
# Short labels (<=3 chars) are horizontal, longer labels are vertical and
# wrapped. Only sets angle/hjust/vjust — size/face are left to inherit from
# theme_house()'s own axis.title styling rather than overridden here. Not
# exported or documented on its own.
resolve_y_lab <- function(y_lab, plot_height = 4) {
  if (!is.null(y_lab) && nchar(y_lab) > 3) {
    list(
      y_lab_final  = wrap_text(y_lab, plot_width = plot_height),
      y_axis_theme = element_text(angle = 90, hjust = 0.5)
    )
  } else if (!is.null(y_lab)) {
    list(
      y_lab_final  = y_lab,
      y_axis_theme = element_text(angle = 0, vjust = 0.5)
    )
  } else {
    list(
      y_lab_final  = NULL,
      y_axis_theme = element_text(angle = 0, vjust = 0.5)
    )
  }
}

# bar_width_from_n(): internal - scales bar width to the number of x-axis
# labels. Fewer bars get narrower widths to avoid them looking too fat. Not
# exported or documented on its own.
bar_width_from_n <- function(num_labels) {
  case_when(
    num_labels <= 3 ~ 0.4,
    num_labels <= 5 ~ 0.6,
    num_labels <= 8 ~ 0.75,
    TRUE            ~ 0.9
  )
}

#' Bar chart for a single categorical x numeric variable
#'
#' Plots statistics already calculated and sitting in a data frame - one row
#' per bar, an x category paired with an already-computed y value
#' ([ggplot2::geom_col()]'s job) - rather than counting raw observations
#' itself ([ggplot2::geom_bar()]'s job). `dat` needs to already be aggregated
#' (e.g. via `count()`/`summarise()`, or [dtab::calc_stats()]) before it's
#' passed in.
#'
#' `horiz` is implemented by swapping which column maps to the x vs y
#' aesthetic (with `geom_col(orientation = )`'s help), not `coord_flip()` -
#' so `axis.title.x`/`axis.text.x` etc. always just mean whichever column
#' actually ends up mapped to x, nothing to translate. Flipping swaps which
#' axis sits on the narrow left margin vs the wide bottom, so whichever of
#' `x_lab`/`y_lab` is now on the left gets the "rotate vertical + wrap if
#' long" treatment instead of it always being `y_lab`.
#'
#' @param dat A data frame, one row per bar.
#' @param x Unquoted column name for the x-axis category - ideally a factor.
#'   Past 8 levels, when `horiz = FALSE`, the category labels switch from
#'   wrapped/horizontal to a single angled line (45 degrees) instead of
#'   combining the two. Skipped entirely when `horiz = TRUE` - a flipped
#'   chart doesn't have the same crowding problem.
#' @param y Unquoted column name for the already-computed value each bar shows.
#' @param x_lab Optional character string for the x-axis title.
#' @param y_lab Optional character string for the y-axis title.
#' @param title Optional plot title - left empty if not supplied.
#' @param fill_family One of:
#'   - an [nc_alt1] colour name (e.g. `"pink"`, `"blue3"`, `"grey5"` - family
#'     name alone defaults to shade 1, see [resolve_pal_colour()]) for a
#'     single colour applied to every bar
#'   - `"multi"` to colour each bar differently, spread across all nc_alt1
#'     families, via [give_categorical_colours_pal()]
#'   - `"<family>_seq"` (e.g. `"blue_seq"`) to colour each bar a different
#'     shade within that one family's tint ramp instead - only meaningful
#'     for an ordered x variable, and capped at 5 bars (one family only has
#'     5 shades)
#'
#'   Default `"pink"`. Named to match [bar_clustered()]/[bar_stacked()]'s own
#'   `fill_family` argument - `bar()` has no separate grouping column for
#'   `fill` to mean "which column maps to colour" the way it does on those
#'   two, so this argument has always meant "colour selection", just kept
#'   under a consistent name.
#' @param base_size Base font size in pt - scales all text via
#'   [theme_house()]. Default `11`.
#' @param horiz `FALSE` (default) for upright bars, `TRUE` for horizontal.
#' @param label_digits Decimal places shown on each bar's value label.
#'   Default `1`. Set to `0` for whole-number data (e.g. counts) where a
#'   trailing ".0" just wastes space.
#' @param label_size Font size in pt for the value labels. Default `NULL`
#'   (falls back to `base_size`). Independent of `base_size` so the labels
#'   can be shrunk without shrinking the theme's title/axis/legend text too -
#'   useful once many bars/dodge groups crowd the labels together.
#' @param label_style `"normal"` (default) - labels placed as usual.
#'   `"suppress"` - no value labels at all. `"diag"` - value labels rotated
#'   45 degrees, meant for the upright case (`horiz = FALSE`); still just a
#'   text angle, so it won't error if used with `horiz = TRUE`, but isn't
#'   really intended for it.
#' @param plot_width Estimated plot width in inches - feeds the
#'   label-wrapping maths only, default `7`. Doesn't set the actual rendered
#'   size - that's controlled separately, by the chunk's `fig-width`/
#'   `fig-height` when knitting, or by [finalise_plot()]'s `ggsave()`
#'   dimensions when exporting. Keep this in sync with whichever of those
#'   applies, or the wrapping will assume a different size than the plot
#'   actually renders at.
#' @param plot_height Estimated plot height in inches - feeds the same
#'   wrapping maths for whichever label ends up on the left margin. Same
#'   caveat as `plot_width`.
#' @param fill Does nothing - present only so `bar()` accepts (and ignores)
#'   the same `fill` argument [bar_clustered()]/[bar_stacked()] use for their
#'   grouping column, for cross-function compatibility when a caller
#'   dispatches between the three (e.g. `rlang::inject()`-built calls that
#'   splice in `fill` unconditionally, whichever of the three ends up called).
#'
#' @return A ggplot object.
#'
#' @export
bar <- function(dat, x, y, x_lab = NULL, y_lab = NULL, title = NULL,
                fill_family = "pink", base_size = 11, horiz = FALSE,
                label_digits = 1, label_size = NULL, label_style = "normal",
                plot_width = 7, plot_height = 4,
                fill = NULL) {

  x_var <- as.character(substitute(x))
  y_var <- as.character(substitute(y))

  num_labels <- length(levels(fct_drop(as.factor(dat[[x_var]]))))
  bar_width  <- bar_width_from_n(num_labels)

  # ---- Everything below is resolved once, up front, into plain values ----
  # ---- fed straight into the ggplot call at the bottom, unconditionally. ----

  # horiz is implemented by swapping which column maps to the x vs y
  # aesthetic (geom_col()'s orientation argument tells it which one is the
  # categorical axis), not coord_flip() - so axis.title.x/axis.text.x etc.
  # below always just mean whichever column actually ends up mapped to x,
  # with no separate "physical position" translation needed.
  aes_x_col      <- if (horiz) y_var else x_var
  aes_y_col      <- if (horiz) x_var else y_var
  bar_orientation <- if (horiz) "y" else "x"

  # Category labels only get wrapped/rotated when upright - flipping
  # already solves the crowding problem wrapping/rotation exist to fix (each
  # category gets its own row instead of competing for horizontal space), so
  # there's nothing to do for them once horiz = TRUE.
  rotate_category_labels <- !horiz && num_labels > 8
  wrap_category_labels    <- !horiz && !rotate_category_labels
  category_label_theme <- if (rotate_category_labels) {
    element_text(angle = 45, hjust = 1)
  } else {
    element_text(angle = 0, hjust = 0.5)
  }

  # x_lab always describes the category axis and y_lab the value axis,
  # regardless of horiz - but whichever one lands on the narrow left margin
  # gets the "rotate vertical + wrap if long" treatment (resolve_y_lab());
  # whichever lands on the wide bottom just wraps plain. Left is value when
  # upright, category once flipped.
  category_lab_resolved <- if (horiz) {
    resolve_y_lab(x_lab, plot_height)
  } else {
    list(
      y_lab_final  = if (!is.null(x_lab)) wrap_text(x_lab, plot_width = plot_width) else NULL,
      y_axis_theme = element_text(angle = 0, hjust = 0.5)
    )
  }
  value_lab_resolved <- if (horiz) {
    list(
      y_lab_final  = if (!is.null(y_lab)) wrap_text(y_lab, plot_width = plot_width) else NULL,
      y_axis_theme = element_text(angle = 0, hjust = 0.5)
    )
  } else {
    resolve_y_lab(y_lab, plot_height)
  }

  # Final text/theme for whichever column is actually mapped to x vs y.
  x_lab_final   <- if (horiz) value_lab_resolved$y_lab_final else category_lab_resolved$y_lab_final
  y_lab_final   <- if (horiz) category_lab_resolved$y_lab_final else value_lab_resolved$y_lab_final
  x_title_theme <- if (horiz) value_lab_resolved$y_axis_theme else category_lab_resolved$y_axis_theme
  y_title_theme <- if (horiz) category_lab_resolved$y_axis_theme else value_lab_resolved$y_axis_theme
  x_text_theme  <- if (horiz) element_text(angle = 0, hjust = 0.5) else category_label_theme
  y_text_theme  <- if (horiz) category_label_theme else element_text(angle = 0, hjust = 0.5)

  title_wrap <- if (!is.null(title)) wrap_text(title, plot_width = plot_width) else NULL

  # Value labels nudge past the top of an upright bar (vjust) vs past the end
  # of a sideways one once flipped (hjust). hjust/vjust set the anchor point
  # in the text's own unrotated box, and angle rotates around that anchor
  # afterwards - so the same nudge values used for angle = 0 send a rotated
  # label off at an angle instead of straight past the bar. Centred/bottom-
  # anchored (0.5, 0) rotates a purely-vertical box 45 degrees straight into
  # the upper-left, which is why it read as "too far left" - moving the
  # anchor to the box's left edge instead (0, 0.2) puts the bulk of the box
  # to the right and slightly above the anchor pre-rotation, which the same
  # 45-degree turn swings up-and-right rather than up-and-left.
  value_label_hjust <- if (label_style == "diag") 0   else if (horiz) -0.3 else 0.5
  value_label_vjust <- if (label_style == "diag") 0   else if (horiz) 0.5  else -1.5

  valid_label_styles <- c("normal", "suppress", "diag")
  if (!label_style %in% valid_label_styles) {
    stop("'", label_style, "' isn't a valid label_style. Valid options: ", paste(valid_label_styles, collapse = ", "))
  }

  label_size_final <- if (is.null(label_size)) base_size else label_size
  label_angle      <- if (label_style == "diag") 45 else 0

  # Resolved to a whole layer (or NULL for "suppress") here, rather than an
  # if/else inside the plot-building chain below - ggplot2 silently ignores
  # a NULL added with `+`, so this can just be spliced straight in either way.
  label_layer <- if (label_style == "suppress") {
    NULL
  } else {
    geom_text(
      aes(label = round(!!sym(y_var), label_digits)),
      hjust = value_label_hjust, vjust = value_label_vjust, angle = label_angle,
      size = label_size_final, size.unit = "pt"
    )
  }

  fill_colour <- if (fill_family == "multi") {
    give_categorical_colours_pal(n = num_labels)
  } else if (grepl("_seq$", fill_family)) {
    family <- sub("_seq$", "", fill_family)
    give_categorical_colours_pal(n = num_labels, families = family, allow_spillover = FALSE)
  } else {
    resolve_pal_colour(fill_family)
  }

  buffer    <- mean(dat[[y_var]], na.rm = TRUE) / 4
  value_lim <- c(0, ceiling(max(dat[[y_var]], na.rm = TRUE) + buffer))

  # No coord_flip() needed - horiz is handled by aes_x_col/aes_y_col above,
  # so the value's buffer just goes on whichever axis it ends up mapped to.
  coord_layer <- if (horiz) coord_cartesian(xlim = value_lim) else coord_cartesian(ylim = value_lim)

  dat %>%
    filter(!is.na(!!sym(x_var))) %>%
    mutate(
      !!sym(x_var) := if (wrap_category_labels) {
        map_chr(as.character(!!sym(x_var)), wrap_text, num_labels = num_labels, char_width = 0.08)
      } else {
        as.character(!!sym(x_var))
      },
      !!sym(x_var) := factor(!!sym(x_var), levels = unique(!!sym(x_var)))
    ) %>%
    ggplot(aes(x = !!sym(aes_x_col), y = !!sym(aes_y_col))) +
    geom_col(fill = fill_colour, width = bar_width, orientation = bar_orientation, color = "grey", linewidth = 0.2) +
    label_layer +
    theme_house(base_size = base_size) +
    labs(
      title = title_wrap,
      x     = x_lab_final,
      y     = y_lab_final
    ) +
    coord_layer +
    theme(
      axis.title.x = x_title_theme,
      axis.title.y = y_title_theme,
      axis.text.x  = x_text_theme,
      axis.text.y  = y_text_theme
    )

}

#' Clustered (dodged) bar chart for a categorical x numeric variable, grouped
#' by a third factor
#'
#' Same idea as [bar()], but a third factor variable is mapped to fill colour
#' and dodged side by side within each x category, rather than one bar per x
#' category alone.
#'
#' @param dat A data frame, one row per x/fill combination.
#' @param x Unquoted column name for the x-axis category. Past 8 levels,
#'   when `horiz = FALSE`, the category labels switch from wrapped/
#'   horizontal to a single angled line (45 degrees) instead of combining
#'   the two. Skipped entirely when `horiz = TRUE`.
#' @param y Unquoted column name for the already-computed value each bar shows.
#' @param fill Unquoted column name for the grouping variable mapped to fill
#'   colour.
#' @param x_lab Optional character string for the x-axis title.
#' @param y_lab Optional character string for the y-axis title.
#' @param fill_lab Optional character string for the legend title.
#' @param title Optional plot title - left empty if not supplied.
#' @param base_size Base font size in pt - scales all text via
#'   [theme_house()]. Default `11`.
#' @param horiz `FALSE` (default) for upright bars, `TRUE` for horizontal -
#'   done by swapping which column maps to the x vs y aesthetic, not
#'   `coord_flip()` - see [bar()]'s own documentation for the full explanation.
#' @param label_digits Decimal places shown on each bar's value label.
#'   Default `1`.
#' @param label_size Font size in pt for the value labels. Default `NULL`
#'   (falls back to `base_size`), independent of `base_size`.
#' @param label_style `"normal"` (default), `"suppress"` (no value labels),
#'   or `"diag"` (labels rotated 45 degrees) - see [bar()]'s own documentation.
#' @param fill_family `NULL` (default) colours `fill`'s levels categorically,
#'   spread across nc_alt1 families ([scale_fill_pal_categorical()]) - the
#'   original, always-on behaviour. Set to a family name (e.g. `"mint"`) to
#'   colour them sequentially within that one family instead
#'   ([scale_fill_pal_sequential()]) - suited to an ordered `fill` variable,
#'   where staying in one hue lets the tint ramp itself carry the ordering.
#'   No cap on levels either way - `scale_fill_pal_sequential()` interpolates
#'   past 5.
#' @param plot_width Estimated plot width in inches - feeds label-wrapping
#'   only, not the actual rendered size (see [bar()]'s own documentation) -
#'   keep in sync with the chunk's `fig-width` or [finalise_plot()]'s
#'   `ggsave()` width. Default `7`.
#' @param plot_height Estimated plot height in inches - feeds the same
#'   wrapping maths for whichever label ends up on the left margin. Default
#'   `4`. Same caveat as `plot_width`.
#'
#' @return A ggplot object. Fill colours always come from [nc_alt1], never
#'   `house_colours`.
#'
#' @export
bar_clustered <- function(dat, x, y, fill, x_lab = NULL, y_lab = NULL,
                          fill_lab = NULL, title = NULL,
                          base_size = 11, horiz = FALSE,
                          label_digits = 1, label_size = NULL, label_style = "normal",
                          fill_family = NULL,
                          plot_width = 7, plot_height = 4) {

  x_var    <- as.character(substitute(x))
  y_var    <- as.character(substitute(y))
  fill_var <- as.character(substitute(fill))

  num_labels      <- length(levels(fct_drop(as.factor(dat[[x_var]]))))
  num_fill_levels <- length(levels(fct_drop(as.factor(dat[[fill_var]]))))

  # ---- Everything below is resolved once, up front, into plain values ----
  # ---- fed straight into the ggplot call at the bottom, unconditionally. ----

  # fill's existing factor level order (Fair < Good < ... for diamonds$cut,
  # say) has to be captured before wrapping and re-applied afterwards - the
  # mutate() below used to just call as.factor() on the wrapped strings with
  # no levels argument, which silently re-sorts alphabetically instead of
  # keeping whatever order the original factor actually had.
  fill_levels_wrapped <- wrap_text(
    levels(fct_drop(as.factor(dat[[fill_var]]))),
    plot_width = plot_width, num_labels = num_fill_levels
  )

  # Resolved to a whole scale here (rather than a branch in the plot-building
  # chain below) - family-name validation, if fill_family is invalid, comes
  # from scale_fill_pal_sequential()/give_categorical_colours_pal() itself.
  fill_scale <- if (is.null(fill_family)) {
    scale_fill_pal_categorical(n = num_fill_levels)
  } else {
    scale_fill_pal_sequential(family = fill_family, n = num_fill_levels)
  }

  aes_x_col       <- if (horiz) y_var else x_var
  aes_y_col       <- if (horiz) x_var else y_var
  bar_orientation <- if (horiz) "y" else "x"

  rotate_category_labels <- !horiz && num_labels > 8
  wrap_category_labels    <- !horiz && !rotate_category_labels
  category_label_theme <- if (rotate_category_labels) {
    element_text(angle = 45, hjust = 1)
  } else {
    element_text(angle = 0, hjust = 0.5)
  }

  category_lab_resolved <- if (horiz) {
    resolve_y_lab(x_lab, plot_height)
  } else {
    list(
      y_lab_final  = if (!is.null(x_lab)) wrap_text(x_lab, plot_width = plot_width) else NULL,
      y_axis_theme = element_text(angle = 0, hjust = 0.5)
    )
  }
  value_lab_resolved <- if (horiz) {
    list(
      y_lab_final  = if (!is.null(y_lab)) wrap_text(y_lab, plot_width = plot_width) else NULL,
      y_axis_theme = element_text(angle = 0, hjust = 0.5)
    )
  } else {
    resolve_y_lab(y_lab, plot_height)
  }

  x_lab_final   <- if (horiz) value_lab_resolved$y_lab_final else category_lab_resolved$y_lab_final
  y_lab_final   <- if (horiz) category_lab_resolved$y_lab_final else value_lab_resolved$y_lab_final
  x_title_theme <- if (horiz) value_lab_resolved$y_axis_theme else category_lab_resolved$y_axis_theme
  y_title_theme <- if (horiz) category_lab_resolved$y_axis_theme else value_lab_resolved$y_axis_theme
  x_text_theme  <- if (horiz) element_text(angle = 0, hjust = 0.5) else category_label_theme
  y_text_theme  <- if (horiz) category_label_theme else element_text(angle = 0, hjust = 0.5)

  title_wrap <- if (!is.null(title)) wrap_text(title, plot_width = plot_width) else NULL

  # hjust/vjust set the anchor point in the text's own unrotated box, and
  # angle rotates around that anchor afterwards - so the same nudge values
  # used for angle = 0 would send a rotated label off at an angle instead of
  # straight past the bar (see bar()'s own comment, same fix applied here).
  value_label_hjust <- if (label_style == "diag") 0   else if (horiz) -0.3 else 0.5
  value_label_vjust <- if (label_style == "diag") 0   else if (horiz) 0.5  else -1.5

  valid_label_styles <- c("normal", "suppress", "diag")
  if (!label_style %in% valid_label_styles) {
    stop("'", label_style, "' isn't a valid label_style. Valid options: ", paste(valid_label_styles, collapse = ", "))
  }

  label_size_final <- if (is.null(label_size)) base_size else label_size
  label_angle      <- if (label_style == "diag") 45 else 0

  label_layer <- if (label_style == "suppress") {
    NULL
  } else {
    geom_text(
      aes(label = round(!!sym(y_var), label_digits)),
      position = position_dodge2(width = 0.9),
      hjust = value_label_hjust, vjust = value_label_vjust, angle = label_angle,
      size = label_size_final, size.unit = "pt"
    )
  }

  buffer    <- mean(dat[[y_var]], na.rm = TRUE) / 4
  value_lim <- c(0, ceiling(max(dat[[y_var]], na.rm = TRUE) + buffer))
  coord_layer <- if (horiz) coord_cartesian(xlim = value_lim) else coord_cartesian(ylim = value_lim)

  dat %>%
    filter(!is.na(!!sym(x_var))) %>%
    mutate(
      !!sym(x_var) := if (wrap_category_labels) {
        map_chr(as.character(!!sym(x_var)), wrap_text, num_labels = num_labels, char_width = 0.08)
      } else {
        as.character(!!sym(x_var))
      },
      !!sym(x_var)    := factor(!!sym(x_var), levels = unique(!!sym(x_var))),
      !!sym(fill_var) := map_chr(
        as.character(!!sym(fill_var)),
        wrap_text,
        plot_width = plot_width,
        num_labels = num_fill_levels
      ),
      !!sym(fill_var) := factor(!!sym(fill_var), levels = fill_levels_wrapped)
    ) %>%
    ggplot(aes(x = !!sym(aes_x_col), y = !!sym(aes_y_col), fill = !!sym(fill_var))) +
    geom_col(position = "dodge2", orientation = bar_orientation, color = "grey", linewidth = 0.2) +
    label_layer +
    fill_scale +
    theme_house(base_size = base_size) +
    labs(
      title = title_wrap,
      x     = x_lab_final,
      y     = y_lab_final,
      fill  = fill_lab
    ) +
    coord_layer +
    theme(
      axis.title.x = x_title_theme,
      axis.title.y = y_title_theme,
      axis.text.x  = x_text_theme,
      axis.text.y  = y_text_theme
    )

}

#' Stacked bar chart for a categorical x numeric variable, grouped by a third
#' factor
#'
#' Same idea as [bar_clustered()], but stacks segments into one bar per x
#' category (`position = "stack"`) instead of placing them side by side -
#' suited to more x categories, or where the total across `fill` levels
#' matters as much as each individual segment.
#'
#' @param dat A data frame, one row per x/fill combination.
#' @param x Unquoted column name for the x-axis category. Past 8 levels,
#'   when `horiz = FALSE`, the category labels switch from wrapped/
#'   horizontal to a single angled line (45 degrees) instead of combining
#'   the two. Skipped entirely when `horiz = TRUE` (the default here) - a
#'   flipped chart doesn't have the same crowding problem.
#' @param y Unquoted column name for the already-computed value each bar shows.
#' @param fill Unquoted column name for the grouping variable mapped to fill
#'   colour.
#' @param x_lab Optional character string for the x-axis title.
#' @param y_lab Optional character string for the y-axis title.
#' @param fill_lab Optional character string for the legend title.
#' @param title Optional plot title - left empty if not supplied.
#' @param base_size Base font size in pt - scales all text via
#'   [theme_house()]. Default `11`.
#' @param horiz `TRUE` (default, matching this function's original
#'   always-flipped behaviour) for horizontal bars, `FALSE` for upright -
#'   done by swapping which column maps to the x vs y aesthetic, not
#'   `coord_flip()` - see [bar()]'s own documentation for the full explanation.
#' @param label_digits Decimal places shown on each segment's value label.
#'   Default `1`.
#' @param label_size Font size in pt for the value labels. Default `NULL`
#'   (falls back to `base_size`), independent of `base_size`.
#' @param label_style `"normal"` (default), `"suppress"` (no value labels),
#'   or `"diag"` (labels rotated 45 degrees) - see [bar()]'s own documentation.
#' @param fill_family `NULL` (default) colours `fill`'s levels categorically,
#'   spread across nc_alt1 families ([scale_fill_pal_categorical()]) - the
#'   original, always-on behaviour. Set to a family name (e.g. `"mint"`) to
#'   colour them sequentially within that one family instead
#'   ([scale_fill_pal_sequential()]) - suited to an ordered `fill` variable,
#'   where staying in one hue lets the tint ramp itself carry the ordering.
#'   No cap on levels either way - `scale_fill_pal_sequential()` interpolates
#'   past 5.
#' @param legend_position `"bottom"` (default) or `"right"` - use `"right"`
#'   for long fill labels.
#' @param plot_width Estimated plot width in inches - feeds label-wrapping
#'   only, not the actual rendered size (see [bar()]'s own documentation) -
#'   keep in sync with the chunk's `fig-width` or [finalise_plot()]'s
#'   `ggsave()` width. Default `7`.
#' @param plot_height Estimated plot height in inches - feeds the same
#'   wrapping maths for whichever label ends up on the left margin. Default
#'   `4`. Same caveat as `plot_width`.
#'
#' @return A ggplot object. Fill colours always come from [nc_alt1], never
#'   `house_colours`.
#'
#' @export
bar_stacked <- function(dat, x, y, fill, x_lab = NULL, y_lab = NULL,
                        fill_lab = NULL, title = NULL,
                        base_size = 11, horiz = TRUE,
                        label_digits = 1, label_size = NULL, label_style = "normal",
                        fill_family = NULL,
                        legend_position = "bottom",
                        plot_width = 7, plot_height = 4) {

  x_var    <- as.character(substitute(x))
  y_var    <- as.character(substitute(y))
  fill_var <- as.character(substitute(fill))

  num_labels      <- length(levels(fct_drop(as.factor(dat[[x_var]]))))
  num_fill_levels <- length(levels(fct_drop(as.factor(dat[[fill_var]]))))
  bar_width       <- bar_width_from_n(num_labels)

  # ---- Everything below is resolved once, up front, into plain values ----
  # ---- fed straight into the ggplot call at the bottom, unconditionally. ----

  # fill's existing factor level order (Fair < Good < ... for diamonds$cut,
  # say) has to be captured before wrapping and re-applied afterwards - the
  # mutate() below used to just call as.factor() on the wrapped strings with
  # no levels argument, which silently re-sorts alphabetically instead of
  # keeping whatever order the original factor actually had.
  fill_levels_wrapped <- wrap_text(
    levels(fct_drop(as.factor(dat[[fill_var]]))),
    plot_width = plot_width, num_labels = num_fill_levels
  )

  # Resolved to a whole scale here (rather than a branch in the plot-building
  # chain below) - family-name validation, if fill_family is invalid, comes
  # from scale_fill_pal_sequential()/give_categorical_colours_pal() itself.
  fill_scale <- if (is.null(fill_family)) {
    scale_fill_pal_categorical(n = num_fill_levels)
  } else {
    scale_fill_pal_sequential(family = fill_family, n = num_fill_levels)
  }

  aes_x_col       <- if (horiz) y_var else x_var
  aes_y_col       <- if (horiz) x_var else y_var
  bar_orientation <- if (horiz) "y" else "x"

  rotate_category_labels <- !horiz && num_labels > 8
  wrap_category_labels    <- !horiz && !rotate_category_labels
  category_label_theme <- if (rotate_category_labels) {
    element_text(angle = 45, hjust = 1)
  } else {
    element_text(angle = 0, hjust = 0.5)
  }

  # This replaces the previous plot_width * 0.2 heuristic for x_lab with the
  # same resolve_y_lab()-based treatment bar()/bar_clustered() use - a real
  # improvement, not just parity, since that 0.2 figure was a guess at how
  # much of plot_width the flipped-to-the-left category title would get,
  # rather than actually being based on plot_height the way resolve_y_lab()
  # is designed to work.
  category_lab_resolved <- if (horiz) {
    resolve_y_lab(x_lab, plot_height)
  } else {
    list(
      y_lab_final  = if (!is.null(x_lab)) wrap_text(x_lab, plot_width = plot_width) else NULL,
      y_axis_theme = element_text(angle = 0, hjust = 0.5)
    )
  }
  value_lab_resolved <- if (horiz) {
    list(
      y_lab_final  = if (!is.null(y_lab)) wrap_text(y_lab, plot_width = plot_width) else NULL,
      y_axis_theme = element_text(angle = 0, hjust = 0.5)
    )
  } else {
    resolve_y_lab(y_lab, plot_height)
  }

  x_lab_final   <- if (horiz) value_lab_resolved$y_lab_final else category_lab_resolved$y_lab_final
  y_lab_final   <- if (horiz) category_lab_resolved$y_lab_final else value_lab_resolved$y_lab_final
  x_title_theme <- if (horiz) value_lab_resolved$y_axis_theme else category_lab_resolved$y_axis_theme
  y_title_theme <- if (horiz) category_lab_resolved$y_axis_theme else value_lab_resolved$y_axis_theme
  x_text_theme  <- if (horiz) element_text(angle = 0, hjust = 0.5) else category_label_theme
  y_text_theme  <- if (horiz) category_label_theme else element_text(angle = 0, hjust = 0.5)

  title_wrap <- if (!is.null(title)) wrap_text(title, plot_width = plot_width) else NULL

  valid_label_styles <- c("normal", "suppress", "diag")
  if (!label_style %in% valid_label_styles) {
    stop("'", label_style, "' isn't a valid label_style. Valid options: ", paste(valid_label_styles, collapse = ", "))
  }

  label_size_final <- if (is.null(label_size)) base_size else label_size
  label_angle      <- if (label_style == "diag") 45 else 0

  # Stacked segment labels centre within their own segment (position_stack())
  # rather than nudging past a bar's end, so unlike bar()/bar_clustered()
  # there's no separate hjust/vjust to resolve here. reverse = TRUE on both
  # this and geom_col()'s own position_stack() below cancels out
  # position_stack()'s documented default behaviour of reversing fill order
  # (so the first factor level ends up at the top of the stack) - without
  # it, fct_rev()-ing fill beforehand to control stacking order doesn't
  # visibly do anything, since the two reversals cancel out.
  label_layer <- if (label_style == "suppress") {
    NULL
  } else {
    geom_text(
      aes(label = round(!!sym(y_var), label_digits)),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      angle = label_angle,
      size = label_size_final, size.unit = "pt"
    )
  }

  dat %>%
    filter(!is.na(!!sym(x_var))) %>%
    mutate(
      !!sym(x_var) := if (wrap_category_labels) {
        map_chr(as.character(!!sym(x_var)), wrap_text, num_labels = num_labels, char_width = 0.08)
      } else {
        as.character(!!sym(x_var))
      },
      !!sym(x_var)    := factor(!!sym(x_var), levels = unique(!!sym(x_var))),
      !!sym(fill_var) := map_chr(
        as.character(!!sym(fill_var)),
        wrap_text,
        plot_width = plot_width,
        num_labels = num_fill_levels
      ),
      !!sym(fill_var) := factor(!!sym(fill_var), levels = fill_levels_wrapped)
    ) %>%
    ggplot(aes(x = !!sym(aes_x_col), y = !!sym(aes_y_col), fill = !!sym(fill_var))) +
    geom_col(position = position_stack(reverse = TRUE), width = bar_width, orientation = bar_orientation, color = "grey", linewidth = 0.2) +
    label_layer +
    fill_scale +
    theme_house(base_size = base_size) +
    labs(
      title = title_wrap,
      x     = x_lab_final,
      y     = y_lab_final,
      fill  = fill_lab
    ) +
    theme(
      axis.title.x     = x_title_theme,
      axis.title.y     = y_title_theme,
      axis.text.x      = x_text_theme,
      axis.text.y      = y_text_theme,
      legend.position  = legend_position,
      legend.direction = if (legend_position == "bottom") "horizontal" else "vertical"
    )

}
