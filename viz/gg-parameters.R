
###### SIZING 

# One figure per powerpoint slide:
#png(file="****.png", width=600, height=400, res=80)

#-----------------------------------------
# CLIMATE RESPONSE SURFACE PLOTS THEME
#----------------------------------------

#Using additional fonts in ggplot
#install.packages("extrafont")

library(extrafont)
library(ggplot2)

#font_import(pattern = "GIL", prompt = FALSE)  # Import Gill family
#loadfonts(device="win")  # Load them all


theme_climSurface <- function(font_size = 10, ...) {
  
  theme_set(theme_light(base_size = font_size))
  
  theme_climateSurface <- theme(
        text             = element_text(color = "#444444"),
        strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
        plot.margin      = unit(c(10,5,5,5),"mm"),
        plot.title       = element_text(size = font_size + 1, face = "bold"),
        plot.subtitle    = element_text(size = font_size),
        axis.title       = element_text(size = font_size),
        axis.text        = element_text(size = font_size),
        legend.title     = element_blank(),
        legend.text      = element_text(size = font_size),
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        aspect.ratio     = 1
  )
}

theme_timeSeries <- function(font_size = 10, ...) {
  
  theme_set(theme_light(base_size = font_size))
  
  theme_timeSeries <- theme(
  text             = element_text(color = "#444444"),
  strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
  plot.title       = element_text(size = font_size, face = "bold"),
  plot.subtitle    = element_text(size = font_size),
  axis.title       = element_text(size = font_size),
  axis.text        = element_text(size = font_size),
  legend.title     = element_blank(),
  legend.text      = element_text(size = font_size),
  ...)  

}

theme_generic <- function(font_size = 10, ...) {
  
  theme_set(theme_light(base_size = font_size))
  
    theme_timeSeries <- theme(
    text             = element_text(color = "#444444"),
    strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
    plot.title       = element_text(size = font_size, face = "bold"),
    plot.subtitle    = element_text(size = font_size),
    axis.title       = element_text(size = font_size),
    axis.text        = element_text(size = font_size),
    legend.title     = element_blank(),
    legend.text      = element_text(size = font_size),
    panel.grid.minor = element_blank(),
    ...)
}

# Use with stylish GCM projections ribbon plot...
theme_climTrends <- function(font_size = 10, ...) {
  
  theme_set(theme_light(base_size = font_size))
  
    theme_timeSeries <- theme(
    text             = element_text(color = "#444444"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    plot.margin      = unit(c(10,5,5,5),"mm"),
    plot.title       = element_text(size = font_size, face = "bold"),
    plot.subtitle    = element_text(size = font_size),
    axis.title       = element_text(size = font_size),
    axis.text        = element_text(size = font_size),
    legend.title     = element_blank(),
    legend.text      = element_text(size = font_size),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border     = element_rect(colour = "gray90", fill=NA, size=0.1),
    ...)
}


  
#   
#   
#   <- theme(
#   text             = element_text(color = "#444444"),
#   strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
#   plot.title       = element_text(size = font_size, face = "bold"),
#   plot.subtitle    = element_text(size = font_size),
#   axis.title       = element_text(size = font_size),
#   axis.text        = element_text(size = font_size),
#   legend.title     = element_blank(),
#   legend.text      = element_text(size = font_size)#,
#   #panel.grid.major = element_line(color = "gray95")
#   #panel.grid.minor = element_blank()
# )
# 
# 
# 
#   
#   
#   
#   theme_set(theme_light(base_size = font_size))
#   
#   theme_climateSurface <- theme(
#         text             = element_text(color = "#444444"),
#         strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
#         plot.margin      = unit(c(10,5,5,5),"mm"),
#         plot.title       = element_text(size = font_size, face = "bold"),
#         plot.subtitle    = element_text(size = font_size),
#         axis.title       = element_text(size = font_size),
#         axis.text        = element_text(size = font_size),
#         legend.title     = element_blank(),
#         legend.text      = element_text(size = font_size),
#         panel.grid.major = element_line(color = "gray95"),
#         panel.grid.minor = element_blank(),
#         aspect.ratio     = 1
#   )
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 













################################################################################




fsize <- 10
theme_set(theme_light(base_size = 10))
theme_clim_surface <- theme(
  text             = element_text(family = "Gill Sans", color = "#444444"),
  strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
  plot.margin      = unit(c(10,5,5,5),"mm"),
  plot.title       = element_text(size = fsize, face = "bold"),
  plot.subtitle    = element_text(size = fsize),
  axis.title       = element_text(size = fsize),
  axis.text        = element_text(size = fsize),
  axis.title.x     = element_text(margin = margin(t = 20)),
  legend.title     = element_blank(),
  legend.text      = element_text(size = fsize),
  panel.grid.major = element_line(color = "gray95"),
  panel.grid.minor = element_blank(),
  legend.position  = "none",
  aspect.ratio = 1
)

clim_surface_axis_labels <- list(
  temp = expression("Temperature change ("* degree * C *")"),
  prcp = "Precip. change (%)"
)
