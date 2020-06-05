

### UI-side function
fileUpload_mod_UI <- function(id) {
  
  ns = NS(id)
  
  fileInput(ns("file1"), "Choose file")
  

}


### Server function
fileUpload_mod <- function(input, output, session) {
  
  ns <- session$ns
  
  userFile <- reactive({
    
    #if no file is selected, don't do anything!
    validate(need(input$file1, message = FALSE))
  
    input$file1
  
  })
 
  #Parse to file into a list object
  path <- reactive({userFile()$datapath})
  data <- reactive({read_csv(userFile()$datapath)}) 

  return(list(data = data, path = path))
  
}

