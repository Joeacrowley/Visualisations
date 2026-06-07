# MMC Housing Study - Outcomes by year of property construction


# Setup ------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(openxlsx)
library(labelled)
library(gtsummary)
library(srvyr)
library(officer)

final_data <- readRDS("Y:/P20505-01/Analysis/Data/Clean_data.Rds")


# Config -----------------------------------------------------------------------

colours10 <- c(
  "#8f99de", "#f57da8", "#33bd9e", "#bf75b5", "#ff9c33",
  "#e3e5f5", "#fcdee8", "#ccede8", "#f0deed", "#ffe5cc"
)

predictors <- c(
  "build_type",       # MMC cat 1, MMC cat 2, traditional homes
  "heatsys2",         # Type of central heating
  "tenure_dv2",       # Property tenure
  "completion_year2", # Year home was built/completed
  "howlong_dv",       # How long residents have lived in their home
  "age4_dv",          # Age in 4 bands
  "nbdrms2",          # Number of bedrooms
  "numhh_dv",         # Number of adults/children
  "gor_num",          # Government office region
  "property_type",    # Detached, semi-detached, terrace, flat
  "firstoccupant2",   # Whether first occupant (yes/no)
  "epc_dv"            # EPC rating (large numbers who do not know)
)

# Categorical outcomes (proportion stacked bar)
outcomes_cat <- c(
  "overheating",
  "heating2",
  "tempsatis2",
  "satisg2",
  "satisq2",
  "acoustics1_2"
)

# Likert outcomes, full detail (proportion stacked bar)
outcomes_likert <- c(
  "tempsatis",
  "satisg",
  "satisq",
  "heating"
)

# Numeric outcomes (median bar)
outcomes_num <- c(
  "tempsatis_num",
  "satisg_num",
  "satisq_num"
)


# Survey design ----------------------------------------------------------------

svy_df <- final_data |>
  as_survey_design(weights = wt_all)


# Helper functions -------------------------------------------------------------

# Wraps a string to fit within a ggplot plot area.
# plot_width:  estimated plot width in inches
# num_labels:  number of bars sharing the axis
# char_width:  approximate width of one character in inches
# margin:      proportion of plot width to treat as unusable
wrap_text <- function(text, plot_width = 7, num_labels = 1,
                      char_width = 0.09, margin = 0.1) {
  usable_width <- plot_width * (1 - margin)
  max_chars    <- floor((usable_width / num_labels) / char_width)
  str_wrap(text, width = max_chars)
}

# Returns a formatted string of unweighted Ns per predictor group,
# excluding cases missing on the outcome.
# e.g. "Group A: n=120 | Group B: n=95"
get_base_sizes <- function(data, predictor, outcome) {
  data |>
    filter(!is.na(.data[[outcome]])) |>
    mutate(pred_factor = haven::as_factor(.data[[predictor]])) |>
    count(pred_factor) |>
    mutate(label = paste0(pred_factor, ": n=", n)) |>
    pull(label) |>
    paste(collapse = " | ")
}

# Shared ggplot theme for all chart types
base_plot_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      text           = element_text(colour = "black"),
      axis.text      = element_text(colour = "black"),
      plot.title     = element_text(face = "bold"),
      legend.key.size  = unit(0.4, "cm"),
      legend.text      = element_text(size = 10),
      legend.spacing.y = unit(0.1, "cm"),
      legend.margin    = margin(0, 0, 0, 0),
      legend.location  = "plot"
    )
}


# Plot functions ---------------------------------------------------------------

# Proportional stacked bar chart (categorical/Likert outcomes)
plot_function <- function(data, outcome, crosstab, title = NULL) {
  
  if (is.null(title)) {
    title <- paste(
      var_label(data[["variables"]][[outcome]]),
      "by",
      var_label(data[["variables"]][[crosstab]])
    )
  }
  
  plot_data <- data |>
    group_by(.data[[crosstab]], .data[[outcome]]) |>
    filter(!is.na(.data[[outcome]])) |>
    summarise(Percent = survey_prop(vartype = NULL) * 100) |>
    ungroup() |>
    mutate_if(is.labelled, to_factor)
  
  ggplot(plot_data, aes(x = .data[[crosstab]], y = Percent, fill = .data[[outcome]])) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(
      aes(label = paste0(round(Percent, 1), "%")),
      position = position_stack(vjust = 0.5),
      size = 3.5, colour = "black"
    ) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(0, 101),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      values = colours10,
      labels = \(x) wrap_text(x, plot_width = 7,
                              num_labels = length(unique(plot_data[[outcome]])))
    ) +
    labs(
      title = wrap_text(title, plot_width = 7),
      x     = NULL,
      y     = "Percentage",
      fill  = NULL
    ) +
    base_plot_theme() +
    theme(panel.grid.major.x = element_blank(),
          legend.position    = "bottom") +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1))
}

# Median bar chart (numeric outcomes)
median_plot_function <- function(data, outcome, crosstab, title = NULL) {
  
  if (is.null(title)) {
    title <- paste(
      var_label(data[["variables"]][[outcome]]),
      "by",
      var_label(data[["variables"]][[crosstab]])
    )
  }
  
  plot_data <- data |>
    group_by(.data[[crosstab]]) |>
    summarise(
      Median = survey_median(!!sym(outcome), vartype = NULL, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate_if(is.labelled, to_factor)
  
  ggplot(plot_data, aes(x = .data[[crosstab]], y = Median)) +
    geom_col(fill = colours10[1], width = 0.6) +
    geom_text(
      aes(label = round(Median, 1)),
      hjust = -0.4, size = 3.5, colour = "black"
    ) +
    scale_y_continuous(expand = expansion(add = c(0, 2))) +
    labs(
      title = wrap_text(title, plot_width = 7),
      x     = NULL,
      y     = "Median"
    ) +
    base_plot_theme() +
    theme(panel.grid.major.y = element_blank()) +
    coord_flip()
}


# Export function --------------------------------------------------------------

export_plots_to_pptx <- function(plot_list, titles = NULL, base_sizes = NULL,
                                 filename = "output.pptx",
                                 slide_width = 10, slide_height = 7.5,
                                 dpi = 300) {
  
  prs       <- read_pptx()
  tmp_files <- c()
  
  for (i in seq_along(plot_list)) {
    
    tmp <- tempfile(fileext = ".png")
    ggsave(tmp, plot = plot_list[[i]], width = slide_width - 1,
           height = slide_height - 1.5, dpi = dpi, bg = "white")
    tmp_files <- c(tmp_files, tmp)
    
    top        <- if (!is.null(titles)) 1.0 else 0.5
    img_height <- if (!is.null(titles)) slide_height - 1.5 else slide_height - 1
    
    prs <- prs |>
      add_slide(layout = "Blank", master = "Office Theme") |>
      ph_with(
        value    = external_img(tmp, width = slide_width - 1, height = img_height),
        location = ph_location(left = 0.5, top = top,
                               width = slide_width - 1, height = img_height)
      )
    
    if (!is.null(titles) && !is.na(titles[[i]])) {
      prs <- prs |>
        ph_with(
          value    = titles[[i]],
          location = ph_location(left = 0.5, top = 0.2,
                                 width = slide_width - 1, height = 0.7)
        )
    }
    
    if (!is.null(base_sizes) && !is.na(base_sizes[[i]])) {
      prs <- prs |>
        ph_with(
          value = fpar(
            ftext("Unweighted sample sizes: ",
                  fp_text(font.size = 8, color = "#666666", bold = TRUE)),
            ftext(base_sizes[[i]],
                  fp_text(font.size = 8, color = "#666666"))
          ),
          location = ph_location(left = 0.5, top = top + img_height + 0.1,
                                 width = slide_width - 1, height = 0.25)
        )
    }
  }
  
  print(prs, target = filename)
  unlink(tmp_files)
  message("Saved: ", filename)
}


# Build plots: completion year -------------------------------------------------

crosstab <- "completion_year2"

plots_cat    <- map(outcomes_cat,    \(x) plot_function(svy_df, outcome = x, crosstab = crosstab))
plots_num    <- map(outcomes_num,    \(x) median_plot_function(svy_df, outcome = x, crosstab = crosstab))
plots_likert <- map(outcomes_likert, \(x) plot_function(svy_df, outcome = x, crosstab = crosstab))

bases <- map(
  c(outcomes_cat, outcomes_num, outcomes_likert),
  \(x) get_base_sizes(svy_df[["variables"]], crosstab, x)
)


# Export -----------------------------------------------------------------------

export_plots_to_pptx(
  plot_list  = c(plots_cat, plots_num, plots_likert),
  base_sizes = bases,
  filename   = "Y:/P20505-01/Analysis/Outputs/Outcomes by year of construction.pptx"
)
