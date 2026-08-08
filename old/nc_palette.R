nc_palette <- 
function () 
{
    green <- c("#00ab85", "#33bd9e", "#66ccb5", "#99decf", "#ccede8")
    purple <- c("#b053a1", "#bf75b5", "#d199c7", "#debad9", "#f0deed")
    pink <- c("#f25c91", "#f57da8", "#f79ebf", "#fabfd4", "#fcdee8")
    blue <- c("#7082d4", "#8f99de", "#abb5e5", "#c7cced", "#e3e5f5")
    orange <- c("#ff8200", "#ff9c33", "#ffb566", "#ffcc99", "#ffe5cc")
    many_colors <- data.frame(blue = blue, pink = pink, green = green, 
        purple = purple, orange = orange, row.names = c("Dark", 
            "less dark", "middling", "light", "lightest"))
    return(many_colors)
}
