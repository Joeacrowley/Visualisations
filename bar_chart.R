bar_chart <- 
function (data, x, y, title = NULL, xlab = NULL, ylab = NULL, 
    data_label = T, horiz = F) 
{
    if (data_label == T) {
        data_label <- y
    }
    else {
        data_label <- NULL
    }
    chart <- ms_barchart(data = data, x = x, y = y, labels = data_label) %>% 
        chart_data_labels(position = "outEnd") %>% chart_ax_x(major_tick_mark = "out") %>% 
        chart_ax_y(major_tick_mark = "out") %>% chart_labels(title = title, 
        xlab = xlab, ylab = ylab) %>% chart_data_fill("#00ab85") %>% 
        chart_data_stroke("transparent") %>% chart_settings(dir = "vertical") %>% 
        chart_theme(grid_major_line_x = fp_border(width = 0), 
            grid_minor_line_x = fp_border(width = 0), grid_minor_line_y = fp_border(width = 0), 
            grid_major_line_y = fp_border(style = "solid", color = "grey85"), 
            legend_position = "n")
    if (horiz == T) {
        chart <- chart %>% chart_settings(dir = "horizontal") %>% 
            chart_theme(title_y_rot = 0)
    }
    return(chart)
}
