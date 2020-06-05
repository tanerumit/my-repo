

library(tidyverse)

#=========================================
# CREATE THEMES
# We'll create two themes:
#
# 1. theme.porttheme
#    - this will be a general theme that
#      we'll apply to most of our charts
#      to format the text, titles, etc
#
# 2. theme.smallmult
#    - we'll apply this exclusively to
#      "small multiple" charts
#      (AKA, trellis charts).  We need this
#      because the axis text needs to be
#      smaller for the small multiples
#=========================================


#----------------------------------------
# GENERAL THEME
# - we'll use this for most of our charts and build on it when we need to
#----------------------------------------

theme.porttheme <-
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank()) +
  theme(panel.grid.major.x = element_line(color = "#F3F3F3")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor   = element_blank()) +
  theme(legend.position    = "none")


#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
# - there are several bar charts that
#   are very wide, and need some
#   special formatting
#------------------------------------

theme.widebar <-
  theme.porttheme +
  theme(plot.title = element_text(size = 30)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(legend.title = element_blank(), legend.background = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = c(.9,.55)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))


#-----------------------------------
# THEME FOR 'WIDE' BAR CHARTS
#  - we'll use this for small multiple
#    charts.  these also have some
#    special formatting requirements
#------------------------------------

theme.smallmult <-
  theme.porttheme +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90))


#GE THE DATA
url.world_ports <- url("http://sharpsightlabs.com/wp-content/datasets/world_ports.RData")

load(url.world_ports)

glimpse(df.world_ports)

param.rank_n = 15


#============================
# BUMP CHART: CHINA
# here, we'll highlight China
#============================


df.world_ports %>%
  filter(rank <= param.rank_n) %>%
  mutate(china_flag = ifelse(economy == "China", T,F)) %>%
  mutate(china_labels = ifelse(china_flag == T, port,"other")) %>%
  ggplot(aes(x = year, y = rank, group = port_label)) +
  geom_line(aes(color = china_labels, alpha = china_flag), size = 2) +
  geom_point(aes(color = china_labels, alpha = china_flag), size = 2.3) +
  geom_point(color = "#FFFFFF", alpha = .8, size = .3) +
  geom_text(data = df.world_ports %>% filter(year == "2014", rank <= param.rank_n),
    aes(label = port_label, x = '2014') , hjust = -.05, color = "#888888", size = 4) +
  geom_text(data = df.world_ports %>% filter(year == "2004", rank <= param.rank_n),
    aes(label = port_label, x = '2004') , hjust = 1.05, color = "#888888", size = 4) +
  scale_x_discrete(expand = c(.3, .3)) +
  scale_y_reverse(breaks = c(1,5,10,15)) +
  scale_alpha_discrete(range = c(.4,.9)) +
  labs(title = "Top Chinese ports from 2004 to 2014") +
  labs(subtitle = "(Port ranks, by volume)") +
  labs(x = "Year", y = "Rank") +
  theme.porttheme +
  theme(panel.grid.major.x = element_line(color = "#F3F3F3")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#4e79a5","#f18f3b","#af7a0a","#e0585b",
    "#5aa155","#edc958","#77b7b2","#BBBBBB"))

################################################################################
################################################################################
################################################################################
################################################################################



#### THICK POINTS (metro stations)

library(ggplot2)

# For shapes that have a border (like 21), you can colour the inside and
# outside separately. Use the stroke aesthetic to modify the width of the
# border

ggplot(mtcars, aes(wt, mpg)) +
  theme_light() +
  geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 2)


#### Rounding edges (pathways)

df = structure(list(x = c(10, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6, 10.7,
  10.8,10.9), y = c(282.083195814139, 281.463374904196,
    280.846027959358,280.231142308826, 279.618705353623,
    279.008704566105, 278.401127489482, 277.795961737318,
    277.193194993064, 276.592815009577)),
  .Names = c("x", "y"), row.names = c(NA, 10L), class = "data.frame")


ggplot(df, aes(x, y)) + geom_path(lwd = 4, lineend = "round")



################################################################################

df1 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(6160, 6160, 6160, 6160), sn = 1)

df2 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(7500, 7500, 7500, 7500), sn = 2)

df3 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(0, 6160, 6160, 6160), sn = 3)

df4 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(0, 6160, 6160, 6160), sn = 4)

df5 <- data_frame(name = 'FOMI',
  stage = rep(0.9, 4), value = rep(0, 4), sn = c(1,2,3,4))

df <- bind_rows(df1, df2, df3, df4) %>%
  mutate(sn = as.factor(sn))

ggplot(df, aes(x = stage, y = value, group = sn, color = sn)) +
  theme_light()  +
  geom_step(lwd = 0.8, alpha = 0.5,
    mapping = aes(x = jitter(stage,0.05), y = jitter(value, 0.05),
      group=sn, col=sn)) +
  geom_point(shape = 21, colour = "black",
    fill = "white", size = 2, stroke = 2)



#################################################################################
