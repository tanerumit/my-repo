

parcoords_mod_UI <- function(id) 
{
  
  ns = NS(id)
  
  parcoordsOutput(ns("parcoordsPlot"), width = "100%", height = "400px")

}


parcoords_mod <- function(input, output, session, data) {
  
  output$parcoordsPlot <- renderParcoords({
    
    parcoords(data())
    
  })

}

