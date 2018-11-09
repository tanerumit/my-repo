
plotClimateTrends <- function(variable, plotsDir, data) {
  
  if (variable == "prcp") {
    
    climate_data <- data %>% rename(value = prcp)
    variable_axis_title <- "mean precipitation (inches)" # Variable axis title 
  
  } else if (variable == "tavg") {
    
    climate_data <- data %>% rename(value = tavg)
    variable_axis_title <- expression("mean temperature (" * degree * F *")") # Variable axis title 
    
    }

  #Climate_data should be a data_frame, with columns: date (date), station (chr), value (numeric)
  station_list <- unique(climate_data$station)

  climate_data_mon <- climate_data %>%
    na.omit() %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(station, year, month) %>%
    summarize(value = ifelse(variable == "prcp", sum(value, na.rm = TRUE), mean(value, na.rm = TRUE)))

  climate_data_wyear <- climate_data %>%
    na.omit() %>%
    mutate(wyear = getWaterYear(date, start_month = 10)) %>%
    group_by(station, wyear) %>% 
    summarize(value = ifelse(variable == "prcp", sum(value, na.rm = TRUE), mean(value, na.rm = TRUE))) %>%
    slice(-c(1, n()))

  p <- ggplot(climate_data_wyear, aes(x = wyear, y = value)) +
    theme_light(base_size = 10) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_wrap(~ station, scales = "free", nrow = 5) +
    labs(x = "year", y = variable_axis_title)

  ggsave(filename = paste0(plotsDir, "obs_annual_", variable, "_trends.png"), 
    height = 8, width = 6.4)
  
  for (i in 1:length(station_list)) {
    
    station_cur <- station_list[i]
    
    df <- climate_data_mon %>% filter(station == station_cur) %>%
      mutate(month =  factor(month, levels = 1:12, labels = month.abb))
    
    p <- ggplot(df, aes(x = year, y = value)) +
      ggtitle(paste0("Station: ", station_cur)) +
      theme_light(base_size = 10) +
      geom_point(alpha = 0.6) + 
      geom_smooth(method = "lm") + 
      facet_wrap(~ month, scales = "free", nrow = 4) +
      labs(x = "year", y = variable_axis_title)
   
    name <- paste0(plotsDir, "obs_monthly_", variable, "_trends_", station_cur, ".png")  
    
    ggsave(filename = name, height = 8.2, width = 6.4)
  
  }
}