

# UI Function
responseSurface_mod_UI <- function(id) {
  
  ns = NS(id)
  
  plotOutput(ns("surface"))
  
}

# Server-side function
responseSurface_mod <- function(input, output, session, data, pheight, pwidth) {
  
  df <- reactive({data()}) 

  #Create the ggplot object
  p <- reactive({
    ggplot(df, aes(x, y, fill = z)) + geom_raster()
  })  
  
  #Render the ggplot object
  output$surface <- renderPlot(p(), height = pheight, width = pwidth)

}














# # Server-side function
# responseSurface_mod <- function(input, output, session, sliderIn, pheight, pwidth) {
#   
#   
#   # Data prep for the ggplot2 object
#   dat <- reactive({
#     df <- expand.grid(x = 0:sliderIn(), y = 0:sliderIn())
#     df$z <- runif(nrow(df))
#     return(df)
#     
#   })
#   
#   #Create the ggplot object
#   p <- reactive({
#     ggplot(dat(), aes(x, y, fill = z)) + geom_raster()
#   })  
#   
#   #Render the ggplot object
#   output$surface <- renderPlot(p(), height = pheight, width = pwidth)
#   
# }