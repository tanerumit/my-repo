

# UI Function
selectInput_mod_UI <- function(id, ...) {
  
  ns = NS(id)
  
  selectInput(ns("run1"), ...)

}

# Server function
selectInput_mod <- function(input, output, session) {
  
  output <- reactive({input$run1})
  
  return(output)
}

