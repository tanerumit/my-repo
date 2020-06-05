


cmip5ScatterPlot_mod_UI <- function(id) 
  {

  ns = NS(id)
  
  highchartOutput(ns("GCMplot"), height = "500px")

}


cmip5ScatterPlot_mod <- function(input, output, session, data, hist.period,
                                 proj.period)
  
  {

  #hist.period <- reactive({input$hist_period}) 
  #proj.period <- 2066:2095 
  tavg.breaks <- seq(0,8,1) 
  prcp.breaks <- seq(-40,40,10)
  
  # Axis limits
  tavg_step <- (tavg.breaks[2] - tavg.breaks[1])/2
  tavg_lim  <- range(tavg.breaks) + c(- tavg_step, tavg_step)
  prcp_step <- (prcp.breaks[2] - prcp.breaks[1])/2
  prcp_lim  <- range(prcp.breaks) + c(- prcp_step, prcp_step)
  
  rcp_col <- c("#08519c", "#abd9e9", "#fe9929", "#f03b20")
  
  delta_clim <- reactive({
    
    dataR <- data() 
    hist.periodR <- hist.period()
    proj.periodR <- proj.period()
    
    # Summarize data for each projection
    df <- lapply(names(dataR),
                 function(x) bind_rows(dataR[[x]], .id = "model")) %>%
      setNames(names(dataR)) %>%
      bind_rows(.id = "scenario") %>% ungroup() %>%
      mutate(scenario = factor(scenario, 
                               levels = c("historical", "rcp26", "rcp45","rcp60","rcp85"),
                               labels = c("Historical", "RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5")))
    
    data_hist <- df %>% filter(scenario == "Historical") %>%
      filter(year %in% hist.periodR) %>%
      group_by(model) %>%
      summarize_at(vars(prcp, tavg), mean) %>%
      mutate(prcp = prcp * 12)
    
    data_proj <- df %>% filter(scenario != "Historical") %>%
      filter(year %in% proj.periodR) %>%
      group_by(scenario, model) %>%
      summarize_at(vars(prcp, tavg), mean) %>%
      mutate(prcp = prcp * 12)
    
    # mean precip and temp changes
    delta_prcp <- data_proj %>%
      select(scenario, model, prcp) %>%
      left_join(select(data_hist, model, hist_prcp = prcp), by = "model") %>%
      mutate(prcp = round((prcp - hist_prcp) / hist_prcp * 100,2))
    
    delta_tavg <- data_proj %>%
      select(scenario, model, tavg) %>%
      left_join(select(data_hist, model, hist_tavg = tavg), by = "model") %>%
      mutate(tavg = round(tavg - hist_tavg,2))
    
    delta_prcp %>%
      left_join(delta_tavg, by = c("scenario", "model")) %>%
      na.omit() %>% select(scenario, model, prcp, tavg)
    
    # # Axis breaks (if not provided)
    # if(is.null(tavg.breaks)) {
    #   tavg.breaks <- seq(0, round(max(delta_tavg$tavg, na.rm = T),0) + 2, 1)
    # }
    # if(is.null(prcp.breaks)) {
    #   prcp.breaks <- seq(
    #     round(min(delta_prcp$prcp, na.rm = TRUE),-1) -20,
    #     round(max(delta_prcp$prcp, na.rm = TRUE),-1) +20,
    #     10)
    # }

  })
  
  output$GCMplot <- renderHighchart({
      
      df2 <- delta_clim()
    
      highchart() %>%
        hc_add_series_df(df2, 
                         type="scatter", radius=7,
                         x=tavg, y=prcp, group=scenario, allowPointSelect = T,
                         borderwidth = 2) %>%
        #Axis settings
        hc_xAxis(min = min(tavg.breaks), max = max(tavg.breaks),
                 tickInterval= tavg.breaks[2] - tavg.breaks[1],
                 gridLineWidth=2, crosshair=T,
                 title = list(text = "Temperature change (°C)")) %>%
        hc_yAxis(min = min(prcp.breaks), max = max(prcp.breaks),
                 tickInterval= prcp.breaks[2] - prcp.breaks[1],
                 gridLineWidth=2, crosshair=T,
                 title = list(text="Precipitation change (%)")) %>%
        #Plot appearance
        hc_add_theme(hc_theme_smpl()) %>%
        hc_plotOptions(
          series = list(marker = list(symbol = "circle"))) %>%
        hc_legend(
          title = list(text="Scenarios", fontSize = "20px"),
          align = "right", verticalAlign = "top",
          layout = "vertical", x = 0, y = 50) %>%
        hc_colors(rcp_col) %>%
        #Additional information & settings
        hc_tooltip(formatter = JS("function(){
          return (' Scenario: ' + this.point.scenario +
                  ' <br> Model :' + this.point.model +
                  ' <br> Delta Temp (DegC) : '+ this.x +
                  ' <br> Delta Precip (%) : '+ this.y)}"),
                   borderWidth = 2) %>%
        hc_exporting(enabled = T) %>%
        hc_size(800, 600)

  })
 
}
