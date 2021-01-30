
# Packages required
library(shiny)
library(ggplot2)

# UI Function
climateResponseSurface_mod_UI <- function(id, label = NULL) {

  ns = NS(id)
  plotOutput(ns("surface"))

}

# Server-side function
climateResponseSurface_mod <- function(id) {

  moduleServer(id,

    function(input, output, session) {

      #df <- reactive({data()})
      df <- data1

      #Create the ggplot object

      p <- reactive({
        ggplot(df, aes(x, y, fill = z)) + geom_raster()
      })

      #Render the ggplot object
      output$surface <- renderPlot(p())

    } #function close
  ) # moduleServer close
}

################################################################################
### Demonstration

data1 <- tibble::tribble(
  ~x,  ~y,    ~z,
  1L, 0.7, 71.83,
  2L, 0.7, 71.67,
  3L, 0.7, 71.33,
  4L, 0.7, 70.83,
  5L, 0.7,  70.5,
  0L, 0.8,    78,
  1L, 0.8, 77.83,
  2L, 0.8, 77.67,
  3L, 0.8,  77.5,
  4L, 0.8,    77,
  5L, 0.8, 76.83,
  0L, 0.9, 84.67,
  1L, 0.9, 84.17,
  2L, 0.9, 83.17,
  3L, 0.9, 82.67,
  4L, 0.9,  82.5,
  5L, 0.9,  81.5,
  0L,   1,    90,
  1L,   1, 89.67,
  2L,   1,    89,
  3L,   1, 88.83,
  4L,   1, 88.33,
  5L,   1, 88.17,
  0L, 1.1, 92.67,
  1L, 1.1,  92.5,
  2L, 1.1, 91.83,
  3L, 1.1, 91.67,
  4L, 1.1, 91.33,
  5L, 1.1, 90.83,
  0L, 1.2, 95.17,
  1L, 1.2, 95.17,
  2L, 1.2, 94.83,
  3L, 1.2, 94.17,
  4L, 1.2, 93.83,
  5L, 1.2, 93.83,
  0L, 1.3,    96,
  1L, 1.3, 95.83,
  2L, 1.3, 95.67,
  3L, 1.3, 95.67,
  4L, 1.3, 95.67,
  5L, 1.3, 95.67,
  0L, 1.4, 96.83,
  1L, 1.4, 96.83,
  2L, 1.4, 96.67,
  3L, 1.4, 96.67,
  4L, 1.4,  96.5,
  5L, 1.4, 96.33,
  0L, 1.5, 97.17,
  1L, 1.5, 97.17,
  2L, 1.5, 97.17,
  3L, 1.5, 97.17,
  4L, 1.5, 97.17,
  5L, 1.5,    97
  )

# Use the module in an app
ui <- fluidPage(

  climateResponseSurface_mod_UI("Surface1")
)

server <- function(input, output, session) {

  climateResponseSurface_mod("Surface1")
}

if (interactive()) {
  shinyApp(ui, server)
}

