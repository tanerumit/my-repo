
###### SIZING 

# One figure per powerpoint slide:
#png(file="****.png", width=600, height=400, res=80)

theme_set(theme_light(base_size = 10))

#----------------------------------------
# CLIMATE RESPONSE SURFACE PLOTS THEME
#----------------------------------------

theme_climsurface <-theme_update(
  plot.title = element_text(face = "bold")
)

climsurface_axis_labels <- list(
  temp = expression("Temperature change (" * degree * C *")"),
  prcp = "Precipitation change (%)"
)

#----------------------------------------
# GENERAL THEME
#----------------------------------------

theme.porttheme <- theme(
  text               = element_text(family = "Gill Sans", color = "#444444"),
  plot.title         = element_text(size = 24),
  plot.subtitle      = element_text(size = 18),
  axis.title         = element_text(size = 14),
  axis.title.y       = element_text(angle = 0, vjust = .5, margin = margin(r = 15)),
  axis.text          = element_text(size = 10),
  axis.title.x       = element_text(margin = margin(t = 20)),
  legend.title       = element_blank(),
  panel.grid.major.x = element_line(color = "#F3F3F3"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor   = element_blank(),
  legend.position    = "none"
)

#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
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
# THEME FOR 'WIDE' BAR CHARTS (2)
#------------------------------------

theme.smallmult <-
  theme.porttheme +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90))

