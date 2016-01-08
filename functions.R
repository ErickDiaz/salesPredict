readCSV <- function(){
  
  ##Cargando la data del CSV
  data <- read.csv(file = "resume_sales.csv", header = T, sep = ",")
  
  ## Mostrando el resumen de la data
  print( summary(data))

  return(data)  
}

predictSales <- function(){
  library(sqldf)
  options(digits=8)
  options(sqldf.driver = "SQLite")
  
  raw_data <- readCSV()
  
  df <- sqldf( "select QTR_ID, MONTH_ID, YEAR_ID, PRODUCTLINEID, sum(SALES) from raw_data group by 1,2,3,4" )
  
  print(df)
}