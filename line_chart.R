line_chart <- 
function (data, group, x, y, title = NULL, xlab = NULL, ylab = NULL, 
    date = T, colour = "5") 
{
    grouping_var <- data %>% pull(group)
    if (is.factor(grouping_var)) {
        n_colours_required <- grouping_var %>% levels
    }
    else {
        n_colours_required <- grouping_var %>% unique
    }
    if (colour == "green") {
        chart_colours <- palette[, 3]
        chart_colours_required <- chart_colours[1:length(n_colours_required)]
        names(chart_colours_required) <- n_colours_required
    }
    else if (colour == "5") {
        chart_colours <- palette[2, ]
        chart_colours_required <- chart_colours[1:length(n_colours_required)]
        names(chart_colours_required) <- n_colours_required
    }
    else if (colour == "10") {
        chart_colours <- c(palette[2, ], palette[4, ])
        chart_colours_required <- chart_colours[1:length(n_colours_required)]
        names(chart_colours_required) <- n_colours_required
    }
    chart <- ms_linechart(data = data, x = x, y = y, group = group) %>% 
        chart_ax_x(major_tick_mark = "out") %>% chart_ax_y(major_tick_mark = "out") %>% 
        chart_labels(title = title, xlab = xlab, ylab = ylab) %>% 
        chart_data_fill(chart_colours_required) %>% chart_data_stroke("transparent") %>% 
        chart_theme(grid_major_line_x = fp_border(width = 0), 
            grid_minor_line_x = fp_border(width = 0), grid_minor_line_y = fp_border(width = 0), 
            grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
            legend_position = "b") %>% chart_data_stroke(values = chart_colours_required)
    if (date == T) {
        chart <- chart %>% chart_ax_x(num_fmt = "mmm-yy")
    }
    return(chart)
}
