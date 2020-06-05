

# Module that creates a button to generate random numbers and then displays it.

# UI Function
button_mod_UI <- function(id) {
  
  ns = NS(id)
  
  list(
    
    actionButton(
      ns("next_num"), 
      "Click to generate a random number"
    ),
    
    textOutput(ns("output_area"))
  )
}

# Server function
button_mod <- function(input, output, session) {
  
  observeEvent(input$next_num, {
  
    output$output_area <- renderText({
    
      rnorm(1)
    })
  })
}