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

functions_folder <- "Y:/P20100-01/09 HO data/SECURE HO Data/R code/mscharts test/functions"

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

# Create a bar chart

bar_chart <- function(
    data, 
    x, 
    y, 
    title  = NULL, 
    xlab = NULL, 
    ylab = NULL, 
    data_label = T, 
    horiz = F
    ) {
  
  if(data_label == T){data_label <- y} else {data_label <- NULL}
  
  chart <- 
    ms_barchart(
      data = data,
      x = x,
      y = y, 
      labels = data_label) %>% 
      chart_data_labels(position = "outEnd") %>%
      chart_ax_x(major_tick_mark = "out") %>% 
      chart_ax_y(major_tick_mark = "out") %>% 
      chart_labels(
        title = title, 
        xlab = xlab, 
        ylab = ylab
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
  
  if(horiz == T){
  chart <- chart %>%  
    chart_settings(dir = "horizontal") %>% 
      chart_theme(title_y_rot = 0)
  }
  
  return(chart)
  
}

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

writeLines(
  c("bar_chart <- ", # Assignment needs to be added manually
    deparse(bar_chart)), # Extract function code
  paste0(functions_folder,"/bar_chart.R")) # File name is just the function name 


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

# - Supporting function for colour palette... 

nc_palette <- function(){
  
green <- c("#00ab85", "#33bd9e", "#66ccb5","#99decf","#ccede8")
purple <- c("#b053a1", "#bf75b5", "#d199c7", "#debad9", "#f0deed")
pink <- c("#f25c91", "#f57da8", "#f79ebf", "#fabfd4", "#fcdee8")
blue <- c("#7082d4", "#8f99de", "#abb5e5", "#c7cced", "#e3e5f5")
orange <- c("#ff8200", "#ff9c33", "#ffb566", "#ffcc99", "#ffe5cc")

# many_colors <- matrix(c(blue, pink, green, purple, orange), nrow = 5, ncol = 5)
many_colors <- data.frame(
  blue = blue, 
  pink = pink, 
  green = green, 
  purple = purple, 
  orange = orange, 
  row.names = c("Dark","less dark", "middling", "light", "lightest")
  )

return(many_colors)

}

palette <- nc_palette()
palette
palette %>% slice(c(1,3))

writeLines(
  c("nc_palette <- ", # Assignment needs to be added manually
    deparse(nc_palette)), # Extract function code
  paste0(functions_folder,"/nc_palette.R")) # File name is just the function name 



# - colour bar chart
colour_bar_chart <- function(
    data, 
    x, 
    y, 
    title  = NULL, 
    xlab = NULL, 
    ylab = NULL, 
    data_label = T, 
    horiz = F, 
    colour = "green"
    ) {
  

  
  if(horiz == F){horizontal <- "vertical"} else {horizontal <- "horizontal"}
  
  # n_colours_required <- data %>% pull(x) %>% unique
  
  grouping_var <- data %>% pull(x)
  
  if(is.factor(grouping_var)){
    n_colours_required <- grouping_var %>% levels
  } else {
    n_colours_required <- grouping_var %>% unique
  }
  
  
  if(colour == "green"){
    
    chart_colours <- palette[,3]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required    
    
  } else if (colour == "5") { 
    
    chart_colours <- palette[2,]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
      
  } else if (colour == "10") { 
    
    chart_colours <- c(palette[2,], palette[4,])
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
      
  }

  
  if(data_label == T){data_label <- y} else {data_label <- NULL}
  
  chart <- ms_barchart(
    data = data %>% mutate(category_proxy = .data[[x]]),
    x = x,
    y = y, 
    group = "category_proxy",
    labels = data_label) %>% 
    chart_settings(
      vary_colors = TRUE,
      grouping = "standard",
      overlap = 90, gap_width = 150, 
      dir = horizontal
    ) %>%
    # as_bar_stack() %>%
    chart_data_labels(position = "outEnd") %>%
    # chart_labels_text(fp_text(bold = T)) %>% # removes other sensible formatting choices so not worth it
    chart_ax_x(major_tick_mark = "out") %>% 
    chart_ax_y(major_tick_mark = "out") %>% 
    chart_labels(
      title = title, 
      xlab = xlab, 
      ylab = ylab
    ) %>% 
    chart_data_fill(chart_colours_required) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
    chart_data_stroke("transparent") %>% 
    chart_theme(
      grid_major_line_x = fp_border(width = 0),
      grid_minor_line_x = fp_border(width = 0),
      grid_minor_line_y = fp_border(width = 0), 
      grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
      legend_position = "n"
    )
  
}

chart <- 
  colour_bar_chart(data = df, 
                x = "category", 
                y = "value", 
                data_label = F)


full_width_chart_slide(
  slides = ppt, 
  title = "NatCen bar chart with colour from a function", 
  chart = chart)


chart <- 
  colour_bar_chart(data = df, 
                   x = "category", 
                   y = "value", 
                   horiz= T, 
                   colour = "5")


full_width_chart_slide(
  slides = ppt, 
  title = "NatCen bar chart with colour from a function", 
  chart = chart)

writeLines(
  c("colour_bar_chart <- ", # Assignment needs to be added manually
    deparse(colour_bar_chart)), # Extract function code
  paste0(functions_folder,"/colour_bar_chart.R")) # File name is just the function name 


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


# - clustered bar chart
clustered_bar_chart <- function(
    data, 
    group,
    x, 
    y, 
    title  = NULL, 
    xlab = NULL, 
    ylab = NULL, 
    data_label = T, 
    horiz = F, 
    colour = "green"
) {
  
  if(horiz == F){horizontal <- "vertical"} else {horizontal <- "horizontal"}
  
  grouping_var <- data %>% pull(group)
  
  if(is.factor(grouping_var)){
    n_colours_required <- grouping_var %>% levels
  } else {
    n_colours_required <- grouping_var %>% unique
  }
  
  if(colour == "green"){
    
    chart_colours <- palette[,3]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required    
    
  } else if (colour == "5") { 
    
    chart_colours <- palette[2,]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
    
  } else if (colour == "10") { 
    
    chart_colours <- c(palette[2,], palette[4,])
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
    
  }
  
  if(data_label == T){data_label <- y} else {data_label <- NULL}
  
  # Create clustreed bar chart
  chart <- ms_barchart(
    data = data,
    x = x,
    y = y, 
    group = group,
    labels = data_label) %>% 
    chart_settings(
      # vary_colors = TRUE,
      grouping = "clustered",
      overlap = -100, gap_width = 400, 
      dir = horizontal
    ) %>%
    # as_bar_stack() %>%
    chart_data_labels(position = "outEnd") %>%
    # chart_labels_text(fp_text(bold = T)) %>% # removes other sensible formatting choices so not worth it
    chart_ax_x(major_tick_mark = "out") %>% 
    chart_ax_y(major_tick_mark = "out") %>% 
    chart_labels(
      title = title, 
      xlab = xlab, 
      ylab = ylab
    ) %>% 
    chart_data_fill(chart_colours_required) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
    chart_data_stroke("transparent") %>% 
    chart_theme(
      grid_major_line_x = fp_border(width = 0),
      grid_minor_line_x = fp_border(width = 0),
      grid_minor_line_y = fp_border(width = 0), 
      grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
      legend_position = "b"
    )

}

chart <- 
  clustered_bar_chart(data = dfclust, 
                   x = "category", 
                   y = "value", 
                   group = "meta_cat", 
                   data_label = F)


full_width_chart_slide(
  slides = ppt, 
  title = "Clustered bar chart", 
  chart = chart)


writeLines(
  c("clustered_bar_chart <- ", # Assignment needs to be added manually
    deparse(clustered_bar_chart)), # Extract function code
  paste0(functions_folder,"/clustered_bar_chart.R")) # File name is just the function name 


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


# - Stacked bar charts as a function... 

# ISSUE - number_fmt is just ignored by the function, can't seem to get it to register.

stacked_bar_chart <- function(
    data, 
    group,
    x, 
    y, 
    title  = NULL, 
    xlab = NULL, 
    ylab = NULL, 
    data_label = T, 
    horiz = F, 
    colour = "green", 
    percent = F, 
    number_fmt = "0.0"
  ) {
  
  if(horiz == F){horizontal <- "vertical"} else {horizontal <- "horizontal"}
  
  if(percent == F){percent <- NULL} else {percent <- 100}
  
  grouping_var <- data %>% pull(group)
  
  if(is.factor(grouping_var)){
    n_colours_required <- grouping_var %>% levels
  } else {
    n_colours_required <- grouping_var %>% unique
  }
  
  if(colour == "green"){
    
    chart_colours <- palette[,3]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required    
    
  } else if (colour == "5") { 
    
    chart_colours <- palette[2,]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
    
  } else if (colour == "10") { 
    
    chart_colours <- c(palette[2,], palette[4,])
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
    
  }
  
  if(data_label == T){data_label <- y
  } else if(is.character(data_label)) {
    data_label <- data_label # use as name of variable with labels.
  } else {data_label <- NULL}
  
  # Create a bar chart
  chart <- ms_barchart(
    data = data,
    x = x,
    y = y, 
    group = group,
    labels = data_label) %>% 
    as_bar_stack(dir = horizontal) %>%
    chart_data_labels(position = "ctr", num_fmt = number_fmt) %>%
    # chart_labels_text(fp_text(bold = T)) %>% # removes other sensible formatting choices so not worth it
    chart_ax_x(major_tick_mark = "out") %>% 
    chart_ax_y(major_tick_mark = "out", limit_max = percent, num_fmt = number_fmt) %>% 
    chart_labels(
      title = title, 
      xlab = xlab, 
      ylab = ylab
    ) %>% 
    chart_data_fill(chart_colours_required) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
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
  
}

chart <- 
  stacked_bar_chart(data = dfclust, 
                    x = "category", 
                    y = "value", 
                    group = "meta_cat", 
                    data_label = F, 
                    horiz = T, 
                    colour = "10")

full_width_chart_slide(
  slides = ppt, 
  title = "Stacked bar chart", 
  chart = chart)

writeLines(
  c("stacked_bar_chart <- ", # Assignment needs to be added manually
    deparse(stacked_bar_chart)), # Extract function code
  paste0(functions_folder,"/stacked_bar_chart.r")) # File name is just the function name 




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

# Line chart as a function... 

line_chart <- function(
    data, 
    group,
    x, 
    y, 
    title  = NULL, 
    xlab = NULL, 
    ylab = NULL, 
    date = T,
    colour = "5"
) {
  
  grouping_var <- data %>% pull(group)
  
  if(is.factor(grouping_var)){
    n_colours_required <- grouping_var %>% levels
  } else {
    n_colours_required <- grouping_var %>% unique
  }
  
  if(colour == "green"){
    
    chart_colours <- palette[,3]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required    
    
  } else if (colour == "5") { 
    
    chart_colours <- palette[2,]
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
    
  } else if (colour == "10") { 
    
    chart_colours <- c(palette[2,], palette[4,])
    chart_colours_required <- chart_colours[1:length(n_colours_required)]
    names(chart_colours_required) <- n_colours_required 
    
  }
  
  chart <- ms_linechart(
    data = data, 
    x = x, y = y, group = group) %>% 
    chart_ax_x(major_tick_mark = "out") %>% 
    chart_ax_y(major_tick_mark = "out") %>% 
    chart_labels(
      title = title, 
      xlab = xlab, 
      ylab = ylab
    ) %>% 
    chart_data_fill(chart_colours_required) %>% # can only be mapped to 'groups', so this chart can only have one fill colour. 
    chart_data_stroke("transparent") %>% 
    chart_theme(
      grid_major_line_x = fp_border(width = 0),
      grid_minor_line_x = fp_border(width = 0),
      grid_minor_line_y = fp_border(width = 0), 
      grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
      legend_position = "b"
    ) %>% 
    chart_data_stroke(values = chart_colours_required)
  
    if(date == T){chart <- chart %>%  chart_ax_x(num_fmt = "mmm-yy")}
  
  return(chart)

}


writeLines(
  c("line_chart <- ", # Assignment needs to be added manually
    deparse(line_chart)), # Extract function code
  paste0(functions_folder,"/line_chart.R")) # File name is just the function name 



chart <- 
  line_chart(data = dfclust, 
             x = "category", 
             y = "value", 
             group = "meta_cat", 
             date = F)

full_width_chart_slide(
  slides = ppt, 
  title = "Line bar chart", 
  chart = chart)

chart <- 
  line_chart(data = df_time, 
             x = "date", 
             y = "y", 
             group = "x", 
             date = T)


full_width_chart_slide(
  slides = ppt, 
  title = "Line bar chart with dates", 
  chart = chart)

# ------------------------------------------------------------------------------
# Export data

print(ppt, target = "Testing mscharts.pptx")
