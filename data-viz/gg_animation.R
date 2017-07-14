
# Animated GGPLOT

devtools::install_github("dgrtwo/gganimate")
library(gganimate)
library(gapminder)
library(ggplot2)


p <- ggplot(gapminder, 
    aes(x = gdpPercap, y = lifeExp, 
        size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()

gganimate(p, interval = .2, "output.mp4")
