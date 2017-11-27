
################################################################################
### General Purpose Diagnosis tools


# Benchmarking -----------------------------------------------------------------

  #Microbencmark, more precise than system.time()
  library(microbenchmark)
  x <- runif(100)
  microbenchmark(sqrt(x), x ^ 0.5)

### Memory usage 
  install.packages("pryr")
  devtools::install_github("hadley/lineprof")
  library(pryr)

  #how many bytes of memory an object occupies:
  object_size(1:10)

  #total size of all objects in memory:
  mem_used()

  #how memory changes during code execution
  mem_change(x <- 1:1e6)

  #variable???s location in memory 
  x <- 1:10
  y <- x
  c(address(x), address(y))



# Code efficiency --------------------------------------------------------------



  #how many names point to that location.
  x <- 1:5
  y <- x
  refs(x)

  #print a message every time the traced object is copied:
  x <- 1:10
  tracemem(x)
  x[5] <- 6L
  y <- x



# Debugging tools --------------------------------------------------------------


