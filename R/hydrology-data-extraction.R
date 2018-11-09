
############## GRDC DATA

library("hddtools")

#time-series data extraction from GRCDC database
data <- tsGRDC(stationID = 1291100, plotOption = FALSE)
catalogueGRDC(columnName = "river", columnValue = "Zambezi")

