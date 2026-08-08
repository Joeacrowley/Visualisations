save_ggplot <- function(plot, 
                        wd = getwd(), 
                        name = character(),
                        dims = FALSE) { 
  
  if (length(name) > 0) {
    name <- name 
  } else {
    name <- "plot"
  }
  
  # Save the plot on a big canvas (40x40cm)
  ggsave(filename = paste0(wd, "/", name, ".png"), 
         width = 50,
         height = 40,
         units = c("cm"), 
         plot = plot)
  
  m_png <- magick::image_trim(magick::image_read(paste0(wd, "/", name, ".png")))
  
  # Save the image in a sensible size (in inches)
  png(filename = paste0(wd, "/", name, ".png"),
      width = (magick::image_info(m_png)$width/300), 
      height = (magick::image_info(m_png)$height/300), 
      units = "in", 
      res = 300)  # Set resolution as needed
  
  grid::grid.raster(m_png)
  dev.off()
  
  if (dims == TRUE) { 
    width <- magick::image_info(m_png)$width/300 
    height <- magick::image_info(m_png)$height/300
    return(list(width = width, 
                height = height))
  } 
  
}


