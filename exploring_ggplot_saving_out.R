
# Eploring GGplots 
# author: Ekaterina Khriakova 

# load packages -----

library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggh4x)
library(viridis)
library(magick)

library(png)
library(officer)

# load sample data ----- 
# Create a bar plot with labels using ggplot
text_size <- 10

plot<- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(stat = "count", fill = "skyblue", color = "black") + # Create a bar plot
  geom_text(
    aes(label = ..count..),  # Display the count labels on the bars
    stat = "count", vjust = -0.5, size = text_size/.pt
  ) +
  labs(
       x = "Cylinders",
       y = "Count")  # Add axis labels and title

# set font size 
plot<- plot + 
  theme(legend.title = element_blank(), 
        axis.text = element_text(colour = "black", family="sans",  size = text_size), 
        legend.text= element_text(colour = "black", family="sans",  size = text_size), 
        axis.title.x = element_text(size = text_size),
        legend.margin = margin (0,0,0,-1, "cm" ), 
  ) 

# set panel size 
plot<- plot + 
  force_panelsizes(rows = unit(7, "cm"), 
                   cols = unit(10, "cm"))

# add caption 
plot<- ggdraw(add_sub(plot, paste0("population\n", "base_line\n", "source\n"),
                       size = text_size-2,
                       vpadding=grid::unit(1.5, "lines"),
                       color = "black", fontfamily = "sans"))

# save out ----- 
plot %>% 
  ggsave(filename= "I:/Workdocs/Evaluation/Analysis team/Code Standardisation/ggplot_explore/plot.png", 
         width = 40, 
         height = 40, 
         units = c("cm")
         )

# PLOT AS SAVED BY GGSAVE (40X40cm)
# # Read the PNG image
img <- readPNG("I:/Workdocs/Evaluation/Analysis team/Code Standardisation/ggplot_explore/plot.png")
# # Get dimensions
width <- ncol(img)
height <- nrow(img)

# PLOT AS TRIMMED BY image_trim 
# trim plot 
m_png<- image_trim(image_read("I:/Workdocs/Evaluation/Analysis team/Code Standardisation/ggplot_explore/plot.png"))
image_info(m_png)$width
image_info(m_png)$height

w_diff<- ncol(img) - image_info(m_png)$width
h_diff<-  nrow(img) - image_info(m_png)$height

# Specify the DPI of the image (e.g., 300 DPI)
dpi <- 300

# Convert pixel dimensions to centimeters
(width / dpi) * 2.54
(height / dpi) * 2.54

(image_info(m_png)$width/dpi)* 2.54
(image_info(m_png)$height/dpi)* 2.54

plot %>% 
  ggsave(filename= "I:/Workdocs/Evaluation/Analysis team/Code Standardisation/ggplot_explore/plot_2.png", 
         width = (image_info(m_png)$width/dpi)* 2.54, 
         height = (image_info(m_png)$height/dpi)* 2.54, 
         units = c("cm")
  )

#image_write(m_png, paste0("Y:/P17482.01/Analysis/Output/LCA/visualisations/", variable, "_gg.png"))
doc <- read_docx()
doc <- body_add_img(doc, src = "I:/Workdocs/Evaluation/Analysis team/Code Standardisation/ggplot_explore/plot_2.png", 
                    width = (image_info(m_png)$width/dpi)* 2.54/ 2.54, 
                    height = (image_info(m_png)$height/dpi)* 2.54/ 2.54)

docx_file_1 <- print(doc, target = tempfile(fileext = ".docx"))

save_my_gg<- function (file_name, plot_name = last_plot(), plot_width = 50, plot_height = 50) { 
  
  file_name<- file_name
  #save the plot on a big canvas (40x40cm): 
  ggsave(filename = file_name, 
         width = plot_width,
         height = plot_height,
         units = c("cm"), 
         plot = plot_name, 
         dpi =300)
  
  #create a version of the plot with no white space around it 
  #to find out how big the plot is in cm without the white space
  m_png<- image_trim(image_read(file_name))
  dpi<- 300 
  #save the plot with optimal width and height 
  plot_name %>% 
    ggsave(filename= file_name, 
           width = (image_info(m_png)$width/dpi)* 2.54+0.5, 
           height = (image_info(m_png)$height/dpi)* 2.54+0.5, 
           units = c("cm")
    )
} 


doc <- read_docx()


image_path

# find out the size
img <- readPNG(image_path)
# # Get dimensions
width <- ncol(img)
height <- nrow(img)

doc <- doc %>%
  body_add_gg(my_plot)

docx_file_1 <- print(doc, target = tempfile(fileext = ".docx"))



library(officer)

# Create a Word document
doc <- read_docx()

# Add the ggplot to the Word document
# Add the ggplot to the Word document
doc <- doc %>%
  body_add_gg(my_plot)

docx_file_1 <- print(doc, target = tempfile(fileext = ".docx"))

# try with proper chart 
# feeling_gg

feeling_gg
doc <- doc %>%
  body_add_gg(feeling_gg)

docx_file_1 <- print(doc, target = tempfile(fileext = ".docx"))
print(docx_file_1)
