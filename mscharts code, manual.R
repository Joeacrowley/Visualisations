# install.packages("mschart")   # if not already installed
# install.packages("officer")    # often used together with mscharts

library(tidyverse)
library(mschart)
library(officer)

fldr <- "Y:/P20100-01/09 HO data/SECURE HO Data/R code/mscharts test"
setwd(fldr)

# Load power point functions.
ppt_functions <- c("I:/Workdocs/Analysis team/Code Standardisation/Code library/Joe/Powerpoint/functions")
functions <- list.files(ppt_functions)
map(functions, ~source(paste0(ppt_functions, "/", .x)))
ppt <- create_empty_ppt()

# Test data... 

# Example dataset
df <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(10, 23, 15, 8)
)

df

# Example dataset
dfclust <- data.frame(
  meta_cat = c("Two", "One", "Three", "Two", "One", "Three", "Two", "One", "Three", "Two", "One", "Three"),
  category = c("A", "B", "C", "D","A", "B", "C", "D","A", "B", "C", "D"),
  value = c(10, 23, 15, 8, 9, 10, 11, 17, 3, 6, 25, 23)
)

dfclust$meta_cat <- factor(dfclust$meta_cat, levels = c("One", "Two", "Three"))

dfclust

# ------------------------------------------------------------------------------
# Bar chart in default mschart style

# Create a bar chart
chart <- ms_barchart(
  data = df,
  x = "category",
  y = "value", 
)

full_width_chart_slide(
  slides = ppt, 
  title = "Test chart 1", 
  chart = chart)



# ------------------------------------------------------------------------------
# Standard bar chart in more NatCen style

# Create a bar chart
chart <- ms_barchart(
  data = df,
  x = "category",
  y = "value", 
  labels = "value") %>% 
  chart_data_labels(position = "outEnd") %>%
  # chart_labels_text(fp_text(bold = T)) %>% # removes other sensible formatting choices so not worth it
  chart_ax_x(major_tick_mark = "out") %>% 
  chart_ax_y(major_tick_mark = "out") %>% 
  chart_labels(
    title = "title", 
    xlab = "xlabel", 
    ylab = "ylabel"
  ) %>% 
  chart_data_fill("#00ab85") %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
  chart_data_stroke("transparent") %>% 
  chart_settings(dir = "vertical") %>% 
  chart_theme(
    grid_major_line_x = fp_border(width = 0),
    grid_minor_line_x = fp_border(width = 0),
    grid_minor_line_y = fp_border(width = 0), 
    grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
    legend_position = "n"
  )

full_width_chart_slide(
  slides = ppt, 
  title = "Standard NatCen bar chart", 
  chart = chart)


# Switch to horizontal
chart <- chart %>%
  chart_settings(dir = "horizontal") %>% 
  chart_theme(title_y_rot = 0)

full_width_chart_slide(
  slides = ppt, 
  title = "tandard NatCen bar chart - horizontal", 
  chart = chart)


# ------------------------------------------------------------------------------
# Bar chart with multiple colours

green <- c("#00ab85", "#33bd9e", "#66ccb5","#99decf","#ccede8")
purple <- c("#b053a1", "#bf75b5", "#d199c7", "#debad9", "#f0deed")
pink <- c("#f25c91", "#f57da8", "#f79ebf", "#fabfd4", "#fcdee8")
blue <- c("#7082d4", "#8f99de", "#abb5e5", "#c7cced", "#e3e5f5")
orange <- c("#ff8200", "#ff9c33", "#ffb566", "#ffcc99", "#ffe5cc")

many_colors <- matrix(c(blue, pink, green, purple, orange), nrow = 5, ncol = 5)
cat_colors_10 <- c(many_colors[2,], many_colors[4,])
cat_colors_10

cat_colors_5 <- many_colors[2,]
cat_colors_5

vec_of_colors <- green[1:4]
names(vec_of_colors) <- df$category

# Create a bar chart
chart <- ms_barchart(
  data = df %>% mutate(category_proxy = category),
  x = "category",
  y = "value", 
  group = "category_proxy",
  labels = "value") %>% 
  chart_settings(
    vary_colors = TRUE,
    grouping = "standard",
    overlap = 90, gap_width = 150, 
    dir = "vertical"
    ) %>%
  # as_bar_stack() %>%
  chart_data_labels(position = "outEnd") %>%
  # chart_labels_text(fp_text(bold = T)) %>% # removes other sensible formatting choices so not worth it
  chart_ax_x(major_tick_mark = "out") %>% 
  chart_ax_y(major_tick_mark = "out") %>% 
  chart_labels(
    title = "title", 
    xlab = "xlabel", 
    ylab = "ylabel"
  ) %>% 
  chart_data_fill(vec_of_colors) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
  chart_data_stroke("transparent") %>% 
  chart_theme(
    grid_major_line_x = fp_border(width = 0),
    grid_minor_line_x = fp_border(width = 0),
    grid_minor_line_y = fp_border(width = 0), 
    grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
    legend_position = "n"
  )

full_width_chart_slide(
  slides = ppt, 
  title = "Bar chart with multiple colours", 
  chart = chart)

# Switch to horizontal
chart <- chart %>%
  chart_settings(dir = "horizontal") %>% 
  chart_theme(title_y_rot = 0)

full_width_chart_slide(
  slides = ppt, 
  title = "Bar chart with multiple colours - horizontal", 
  chart = chart)



# ------------------------------------------------------------------------------
# Clustered bar charts

vec_of_colors <- green[c(1,3,5)]
names(vec_of_colors) <- dfclust$meta_cat %>% levels
vec_of_colors

# Create a bar chart
chart <- ms_barchart(
  data = dfclust,
  x = "category",
  y = "value", 
  group = "meta_cat",
  labels = "value") %>% 
  chart_settings(
    # vary_colors = TRUE,
    grouping = "clustered",
    overlap = -100, gap_width = 400, 
    dir = "vertical"
  ) %>%
  # as_bar_stack() %>%
  chart_data_labels(position = "outEnd") %>%
  # chart_labels_text(fp_text(bold = T)) %>% # removes other sensible formatting choices so not worth it
  chart_ax_x(major_tick_mark = "out") %>% 
  chart_ax_y(major_tick_mark = "out") %>% 
  chart_labels(
    title = "title", 
    xlab = "xlabel", 
    ylab = "ylabel"
  ) %>% 
  chart_data_fill(vec_of_colors) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
  chart_data_stroke("transparent") %>% 
  chart_theme(
    grid_major_line_x = fp_border(width = 0),
    grid_minor_line_x = fp_border(width = 0),
    grid_minor_line_y = fp_border(width = 0), 
    grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
    legend_position = "b"
  )

full_width_chart_slide(
  slides = ppt, 
  title = "Clustered bar chart", 
  chart = chart)


# ------------------------------------------------------------------------------
# Stacked bars

vec_of_colors <- cat_colors_5[1:3]
names(vec_of_colors) <- dfclust$meta_cat %>% levels
vec_of_colors

# Create a bar chart
chart <- ms_barchart(
  data = dfclust,
  x = "category",
  y = "value", 
  group = "meta_cat",
  labels = "value") %>% 
  as_bar_stack(dir = "horizontal") %>%
  chart_data_labels(position = "ctr") %>%
  # chart_labels_text(fp_text(bold = T)) %>% # removes other sensible formatting choices so not worth it
  chart_ax_x(major_tick_mark = "out") %>% 
  chart_ax_y(major_tick_mark = "out") %>% 
  chart_labels(
    title = "title", 
    xlab = "xlabel", 
    ylab = "ylabel"
  ) %>% 
  chart_data_fill(vec_of_colors) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
  chart_data_stroke("transparent") %>% 
  chart_theme(
    grid_major_line_x = fp_border(width = 0),
    grid_minor_line_x = fp_border(width = 0),
    grid_minor_line_y = fp_border(width = 0), 
    grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
    legend_position = "b"
  ) %>% 
  chart_data_stroke(values = "grey60") %>% 
  chart_data_line_width(values = 1.5)

full_width_chart_slide(
  slides = ppt, 
  title = "Stacked bar chart", 
  chart = chart)


# ------------------------------------------------------------------------------
# Line chart

df_time <- data.frame(
  y = sample(1:100, 1000, replace = T), 
  x = factor(sample(c("A","B","C","D"), 1000, replace = T)), 
  date = sample(seq.Date(date("2020/01/01"), date("2021/01/01"), by = "month"), 1000, replace = T)
)

df_time <- df_time %>% group_by(x, date) %>% summarise(y = mean(y))
df_time

chart <- ms_linechart(
  data = df_time, 
  x = "date", y = "y", group = "x") %>% 
  chart_ax_x(num_fmt = "mmm-yy")


full_width_chart_slide(
  slides = ppt, 
  title = "Line chart", 
  chart = chart)


# ------------------------------------------------------------------------------
# Line chart - formatted... 

vec_of_colors <- cat_colors_5[1:4]
names(vec_of_colors) <- df_time$x %>% levels
vec_of_colors


chart <- ms_linechart(
  data = df_time, 
  x = "date", y = "y", group = "x") %>% 
  chart_ax_x(num_fmt = "mmm-yy") %>% 
  chart_ax_x(major_tick_mark = "out") %>% 
  chart_ax_y(major_tick_mark = "out") %>% 
  chart_labels(
    title = "title", 
    xlab = "xlabel", 
    ylab = "ylabel"
  ) %>% 
  chart_data_fill(vec_of_colors) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
  chart_data_stroke("transparent") %>% 
  chart_theme(
    grid_major_line_x = fp_border(width = 0),
    grid_minor_line_x = fp_border(width = 0),
    grid_minor_line_y = fp_border(width = 0), 
    grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
    legend_position = "b"
  ) %>% 
  chart_data_stroke(values = vec_of_colors)

# %>% 
#   chart_data_stroke(values = "black") %>% 
#   chart_data_line_width(values = 1)


full_width_chart_slide(
  slides = ppt, 
  title = "Line chart", 
  chart = chart)


# ------------------------------------------------------------------------------
# Export data

print(ppt, target = "Testing mscharts.pptx")
