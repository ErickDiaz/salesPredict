readCSV <- function(){
  
  ##Cargando la data del CSV
  data <- read.csv(file = "resume_sales.csv", header = T, sep = ",")
  
  ## Mostrando el resumen de la data
  print( summary(data))

  return(data)  
}

summaryModel <- function( s ){
  rse <- s$sigma
  df <- s$df[2]
  mse <- s$r.squared
  pa <- mse *100
  
  print(paste("degrees of freedom",df))
  print(paste("Residual standard error: ",rse))  
  print(paste("Multiple R squared error : ",mse))
  print( paste("Prediction accuracy :", pa , "%", collapse=" "))
  
  return(mse)
}

plotDataFrame <- function (df) {
  plot(x=df$date, y=df$y,col='black',type="l" )        
  points(x=df$date , y=df$trend, col='red', type='l', lwd=2)  
}


predictSales <- function(){
  library(sqldf)
  options(digits=8)
  options(sqldf.driver = "SQLite")
  
  raw_data <- readCSV()
  
  df <- sqldf( "select QTR_ID as x1, MONTH_ID as x2, YEAR_ID as x3, PRODUCTLINEID as x4, sum(SALES) as y 
               from raw_data 
               group by 1,2,3,4
               order by x1,x2,x3" )
  
  
  
  lm <- lm(y ~  x1 + x2 + x3 + x4, data = df)
  summaryModel(summary(lm))
  
  res <- predict(lm,df)  
  df[, "trend"] <- res
  
  print(df)
  
  
}

