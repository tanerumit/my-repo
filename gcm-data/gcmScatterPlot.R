
gcmScatterPlot <- function(
  data = NULL, hist_period = NULL, proj_period = NULL,
  tavg_axis_breaks = NULL, prcp_axis_breaks = NULL, save = FALSE,
  plot_historical = TRUE) {
  
  require(ggplot2)
  require(dplyr)
  
  # Summarize data for each projection
  scenarios <- names(data)
  
  df <- lapply(names(data), 
    function(x) bind_rows(data[[x]], .id = "model")) %>%
    setNames(scenarios) %>%
    bind_rows(.id = "scenario") %>% ungroup()
  
  data_hist <- df %>% filter(scenario == "historical") %>%
    filter(year %in% hist_period) %>%
    group_by(model) %>%
    summarize_at(vars(prcp:tmin), mean) %>%
    mutate(prcp = prcp * 12)
  
  data_proj <- df %>% filter(scenario != "historical") %>%
    filter(year %in% proj_period) %>%
    group_by(scenario, model) %>%
    summarize_at(vars(prcp:tmin), mean) %>%
    mutate(prcp = prcp * 12)

  # mean precip and temp changes
  delta_prcp <- data_proj %>% 
    select(scenario, model, prcp) %>%
    left_join(select(data_hist, model, hist_prcp = prcp), by = "model") %>%
    mutate(prcp = (prcp - hist_prcp) / hist_prcp * 100)
  
  delta_tavg <- data_proj %>% 
    select(scenario, model, tavg) %>%
    left_join(select(data_hist, model, hist_tavg = tavg), by = "model") %>%
    mutate(tavg = tavg - hist_tavg)
  
  delta_hist <- data_frame(scenario = "historical", model = NA, prcp = 0, tavg = 0)
  
  delta_clim <- delta_prcp %>%
    left_join(delta_tavg, by = c("scenario", "model")) %>%
    na.omit() %>% select(scenario, model, prcp, tavg) %>%
    bind_rows(delta_hist,.)

  # Axis steps
  if(is.null(tavg_axis_breaks)) {
    tavg_axis_breaks <- seq(0, round(max(delta_tavg$tavg, na.rm = T),0) + 2, 1)
  }
  if(is.null(prcp_axis_breaks)) {
  prcp_axis_breaks <- seq(
    round(min(delta_prcp$prcp, na.rm = TRUE),-1) -20,
    round(max(delta_prcp$prcp, na.rm = TRUE),-1) +20,
    10)
  }

  tavg_step <- (tavg_axis_breaks[2] - tavg_axis_breaks[1])/2 
  prcp_step <- (prcp_axis_breaks[2] - prcp_axis_breaks[1])/2 
  
  # Axis limits
  tavg_axis_lim  <- range(tavg_axis_breaks) + c(- tavg_step, tavg_step)
  prcp_axis_lim  <- range(prcp_axis_breaks) + c(- prcp_step, prcp_step)
  
  histp_range <- paste(min(hist_period), max(hist_period), sep = "-")
  projp_range <- paste(min(proj_period), max(proj_period), sep = "-")
  
  title <- paste0("Climate Changes:\nFuture period (",projp_range,") - Historical period (",histp_range,")")
  gg_name <- paste0("./gcmScatter_hist(",histp_range,")_proj(",projp_range,").png")
  
  gg <- ggplot(mapping = aes(x = tavg, y = prcp)) +
    labs(title = title, 
      x = expression("Temperature change (" * degree * C *")"), 
      y = "Precipation change (%)",
      color = NULL) +
    geom_point(aes(color = scenario), 
      delta_clim, shape = 1, stroke = 1, size = 2, alpha = 0.8) +
    scale_color_manual(
      values = c("black", "blue3", "dodgerblue", "darkorange","firebrick"),
      breaks = c("historical", "rcp26", "rcp45", "rcp60", "rcp85"), 
      labels = c("Historical", "RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5")) +
    scale_x_continuous(expand = c(0,0), breaks = tavg_axis_breaks, 
      limits = tavg_axis_lim) +
    scale_y_continuous(expand = c(0,0), breaks = prcp_axis_breaks, 
      limits = prcp_axis_lim, labels = prcp_axis_breaks) 
  
  baseSize <- 10
  gg <- gg + 
    theme_bw(base_size = baseSize) +
    theme(
      plot.margin      = unit(c(10,5,5,5),"mm"),
      plot.title       = element_text(face = "bold"),
      strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
      legend.text      = element_text(size = baseSize),
      panel.grid.major = element_line(colour = "gray95"),
      panel.grid.minor = element_blank(),
      aspect.ratio = 1
    )

  if(save == TRUE) ggsave(gg_name, height = 7, width = 8) 
  
  return(gg)
  
}