###################################
## Forecast model evaluation  #####
###################################


### Testing with real data from the application server ###


#' load all necessary libraries
loadLibraries <- function()
{
  library(forecast) # import ARIMA etc. 
  library(MASS)
  library(TSA) # import periodogram etc. 
  library(car) # import qqPlot
  library(xts) # import periodicity and time functions (will also load zoo)
  library(RCurl) # get lib for retrieving data from URLs
  library(stringr) # remove white space from string
}


init <- function()
{
  loadLibraries()
  source("functions.R")
}



getEnergyData <- function(url, transformPrice=TRUE)
{
  
  baseUrl <- "http://localhost:8081/em-app/rest"
  
  url <- str_replace_all(string = url, pattern=" ", replacement = "%20")
     
  urlData <- getURL(paste(baseUrl,url,"?transformPrice=",transformPrice,sep=""))
  
  return (getCSV(urlData))
}


getCSV <- function(csvString, header=TRUE)
{
  con <- textConnection(csvString)
  dataDF <- read.csv(con, header = header, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
  close(con)
  return (dataDF)
}


plotData <- function(url, transformPrice=TRUE, title="")
{
  priceDF <- getEnergyData(url, transformPrice)
  
  dates <- strptime(priceDF$Date, "%Y-%m-%d %H:%M")
  #   allDAPrices <- ts(allPriceDF[,-1])
  
  dat.xts <- xts(priceDF[-1], 
                 order.by = dates)
  
  # plot.zoo -> locations and dates
  plot.zoo(dat.xts, main=title, xlab="")
  
#   # plot.ts -> no dates
#   plot(allDAPrices, type='l', main='DA prices, 5 weeks 2014', 
#        ylab='Price in € per MWh', xlab='')
  
}



allDALocationsPlotByYear <- function(year, plot=TRUE)
{
  
  if(!plot)
  {
    prepareImage("da","5 weeks",year)
    plotData(paste0("/daprices/price/csv/-1/",year,"-06-22/",year,"-07-28",sep=""), TRUE, paste0("DA prices, 5 weeks ",year))
    closeImage()
    
    prepareImage("da","8 months",year)
    plotData(paste0("/daprices/price/csv/-1/",year,"-01-02/",year,"-07-31",sep=""), TRUE, paste0("DA prices, 8 months ",year))
    closeImage()
    
    prepareImage("da","6 months",year)
    plotData(paste0("/daprices/price/csv/-1/",year,"-07-01/",year,"-12-31",sep=""), TRUE, paste0("DA prices, 6 months ",year))
    closeImage()
  }
  
  else
  {
    plotData(paste("/daprices/price/csv/-1/",year,"-06-22/",year,"-07-28",sep=""), TRUE, paste("DA prices, 5 weeks ",year,sep=""))
    
    plotData(paste("/daprices/price/csv/-1/",year,"-01-02/",year,"-07-31",sep=""), TRUE, paste("DA prices, 8 months ",year,sep=""))
    
    plotData(paste("/daprices/price/csv/-1/",year,"-07-01/",year,"-12-31",sep=""), TRUE, paste("DA prices, 6 months ",year,sep=""))
  }
  
}


allRTLocationsPlotByYear <- function(year, plot=TRUE, onlySpring=FALSE)
{
  
  if(!plot)
  {
    if(onlySpring)
    {
      prepareImage("rt","5 months",year)
      plotData(paste0("/rtprices/price/csv/-1/",year,"-01-02/",year,"-05-31",sep=""), TRUE, paste0("RT prices, 5 months ",year))
      closeImage()
    }
    else
    {
      prepareImage("rt","5 weeks",year)
      plotData(paste0("/rtprices/price/csv/-1/",year,"-06-22/",year,"-07-28",sep=""), TRUE, paste0("RT prices, 5 weeks ",year))
      closeImage()
      
      prepareImage("rt","8 months",year)
      plotData(paste0("/rtprices/price/csv/-1/",year,"-01-02/",year,"-07-31",sep=""), TRUE, paste0("RT prices, 8 months ",year))
      closeImage()
      
      prepareImage("rt","6 months",year)
      plotData(paste0("/rtprices/price/csv/-1/",year,"-07-01/",year,"-12-31",sep=""), TRUE, paste0("RT prices, 6 months ",year))
      closeImage()
      
      prepareImage("rt","6 weeks",year)
      plotData(paste0("/rtprices/price/csv/-1/",year,"-10-01/",year,"-11-10",sep=""), TRUE, paste("RT prices, 6 weeks ",year))
      closeImage()
    }
  }
  
  else
  {
    if(onlySpring)
    {
      plotData(paste0("/rtprices/price/csv/-1/",year,"-01-02/",year,"-05-31",sep=""), TRUE, paste0("RT prices, 5 months ",year))
    }
    else
    {
      plotData(paste("/rtprices/price/csv/-1/",year,"-06-22/",year,"-07-28",sep=""), TRUE, paste("RT prices, 5 weeks ",year,sep=""))
      
      plotData(paste("/rtprices/price/csv/-1/",year,"-01-02/",year,"-07-31",sep=""), TRUE, paste("RT prices, 8 months ",year,sep=""))
      
      plotData(paste("/rtprices/price/csv/-1/",year,"-07-01/",year,"-12-31",sep=""), TRUE, paste("RT prices, 6 months ",year,sep=""))
      
      plotData(paste("/rtprices/price/csv/-1/",year,"-10-01/",year,"-11-10",sep=""), TRUE, paste("RT prices, 6 weeks ",year,sep=""))
    }
  }
}


allDALocationsPlot <- function() 
{
  
  allDALocationsPlotByYear(2014, FALSE)
  
  allDALocationsPlotByYear(2013, FALSE)
  
  allDALocationsPlotByYear(2012, FALSE)
  
}


allRTLocationsPlot <- function() 
{
  allRTLocationsPlotByYear(2015, FALSE, onlySpring=TRUE)
  
  allRTLocationsPlotByYear(2014, FALSE)
  
  allRTLocationsPlotByYear(2013, FALSE)
  
  allRTLocationsPlotByYear(2012, FALSE)
  
}


prepareImage <- function(type, timeRange, year)
{
  tr <- str_replace_all(string=timeRange, pattern=" ", repl="")
  filename <- paste0(type,"_",year,"_",tr)
  if(type == "da")
  {
    basePath <- "/../screenshots/Data Analysis/DA/time series/"
  }
  else 
  {
    basePath <- "/../screenshots/Data Analysis/RT/time series/"
  }
  path <- paste0(getwd(),basePath,filename)
  
  png(file = paste0(path,".png"),width=840,height=620,units="px",pointsize=16)
}

prepareImageAggregated <- function(simulationName, model, width=640, height=625)
{
  filename <- paste0(simulationName,"_",model)
  
  basePath <- "/../screenshots/Data Analysis/simulationResults/"
  path <- paste0(getwd(),basePath,filename)
  
  png(file = paste0(path,".png"),width=width,height=height,units="px",pointsize=16)
}

closeImage <- function()
{
  dev.off()
}


simulationResults <- function()
{
  simulationResultsPerLocation(6,FALSE)
}


simulationResultsPerLocation <- function(locationId, plot=TRUE) 
{
  for(i in 1:length(modelNames)) {
    plotSimulationResults(locationId,i,plot)
  }
}


plotSimulationResults <- function(locationId, numModel, plot=TRUE)
{
  if(locationId < 4) {
    simPrefix <- paste0("da_sim_",locationId,"_")
    type <- "DA"
  }
  else {
    simPrefix <- paste0("rt_sim_",locationId,"_")
    type <- "RT"
  }
  
  simPostFix <- "_1w_1w"
  
  if(!plot) {
    prepareImageAggregated(paste0(simPrefix,"x",simPostFix), modelNames[numModel])
  }
  
  par(mfrow=c(3,1))
  par(xpd=TRUE)
  
  plotSim2w <- getAccuracyMeasures(paste0(simPrefix,"2w",simPostFix),numModel)
  plotSim3w <- getAccuracyMeasures(paste0(simPrefix,"3w",simPostFix),numModel)
  plotSim4w <- getAccuracyMeasures(paste0(simPrefix,"4w",simPostFix),numModel)
  
  barplot(plotSim2w,legend=F,beside=T,main=paste0('Forecast Errors for model "',modelNames[numModel],'", 2 weeks trainings data (',type,')'),
          axis.lty = 0,las=1)
  barplot(plotSim3w,legend=F,beside=T,main=paste0('Forecast Errors for model "',modelNames[numModel],'", 3 weeks trainings data (',type,')'),
          axis.lty = 0,las=1)
  legend("topleft", fill = gray.colors(5), inset=c(0.02,-0.4), legend=rownames(plotSim2w))
  barplot(plotSim4w,legend=F,beside=T,main=paste0('Forecast Errors for model "',modelNames[numModel],'", 4 weeks trainings data (',type,')'),
          axis.lty = 0,las=1)
  
  if(!plot) {
    closeImage()
  }
  
}



getAccuracyMeasures <- function(simName, numModel)
{  
  path <- "C:/Users/Andreas/Dropbox/uni/DA/Implementation/Java/em-app/src/main/resources/data/simulationResults/aggregated/"
  simName <- get(load(paste0(path,simName,".RData")))

  model <- numModel
  
  trainingValues <- c()
  testValues <- c()
  prefix <- "test_"
  
  for(i in 1:length(accMeasures)) {
    name <- paste0(prefix,accMeasures[i])
    for(h in 1:length(fcHorizons)) {
      acc <- simName[[1]][[model]][[h]]
      value <- acc[[name]]
      if(!is.finite(value)) {
        value <- 0
      }
      testValues <- c(testValues,value)
    }
  }
  
  accMeasurePlot <- matrix((testValues),nrow=5,byrow=TRUE)
  colnames(accMeasurePlot) <- fcHorizons
  rownames(accMeasurePlot) <- accMeasures
  accMeasurePlot <- as.table(accMeasurePlot)
  return (accMeasurePlot)
}



printXTable <- function(data, exponential=TRUE)
{
  if(max(data) > 1000 && exponential) {
    print(xtable(data, digits=-2))
  }
  else {
    print(xtable(data, digits=2))
  }
}


#' batchRMSEResults: Get results from forecast simulation for multiple locationIds
#' -----------------------
#' 
batchRMSEResults <- function()
{
  
  getRMSEResults(6)
}


#' getRMSEResults: Get results from forecast simulation 
#' -----------------------
#' Get results from forecast simulation for the given location
#' over a time range of 3 years, generate models in intervals of 1 week
#' with 2,3 and 4 weeks of trainings data period
#' 5 locations are available from the forecast simulation (Application server):
#'  1) Hamina, locationId 1, DA
#'  2) St.Ghislain, locationId 2, DA
#'  3) Portland, locationId 4, RT
#'  4) Richmond, locationId 6, RT
#'  5) Hatfield, locationId 8, RT
#'  
#' prints latex tables for each simulation (2,3 and 4 weeks of trainings data)
#'  
#' @param locationId the location from which to get simulation results
getRMSEResults <- function(locationId)
{  
  if(locationId < 4) {
    simPrefix <- paste0("da_sim_",locationId,"_")
    type <- "DA"
  }
  else {
    simPrefix <- paste0("rt_sim_",locationId,"_")
    type <- "RT"
  }
  
  simPostFix <- "_1w_1w"

  sim2wRMSE <- getRMSEPerSimulation(paste0(simPrefix,"2w",simPostFix))
  sim3wRMSE <- getRMSEPerSimulation(paste0(simPrefix,"3w",simPostFix))
  sim4wRMSE <- getRMSEPerSimulation(paste0(simPrefix,"4w",simPostFix))
  
  printXTable(sim2wRMSE)
  printXTable(sim3wRMSE)
  printXTable(sim4wRMSE)
  
}


#' getRMSEPerSimulation: function to retrieve RMSE values for all models per given simulation
#' -------------------
#' The accuracy values to be retrieved are already computed in the simulation
#' This method just extracts the RMSE values from the simulation
#' @param simName the simulation name to compute the errors for
#' @param output 
getRMSEPerSimulation <- function(simName)
{  
  path <- "C:/Users/Andreas/Dropbox/uni/DA/Implementation/Java/em-app/src/main/resources/data/simulationResults/aggregated/"
  simName <- get(load(paste0(path,simName,".RData")))
  
  trainingValues <- c()
  testValues <- c()
  prefix <- "test_"
  accMeasure <- "RMSE"
  accName <- paste0(prefix,accMeasure)
  
  for(i in 1:length(modelNames)) {
    model <- i
    for(h in 1:length(fcHorizons)) {
      acc <- simName[[1]][[model]][[h]]
      value <- acc[[accName]]
      if(!is.finite(value)) {
        value <- 0
      }
      testValues <- c(testValues,value)
    }
  }
  
  RMSEmatrix <- matrix((testValues),nrow=length(modelNames),byrow=TRUE)
  colnames(RMSEmatrix) <- fcHorizons
  rownames(RMSEmatrix) <- modelNames
  RMSEmatrix <- as.table(RMSEmatrix)
  
  return (RMSEmatrix)
}



#' getRMSEForAggregatedFcHorizons
#' -------------------
#' @param output 
getRMSEForAggregatedFcHorizons <- function()
{
  simPrefix <- c()
  
  simPrefix[1] <- paste0("da_sim_1_")
  simPrefix[2] <- paste0("da_sim_2_")
  simPrefix[3] <- paste0("rt_sim_4_")
  simPrefix[4] <- paste0("rt_sim_6_")
  
  simPostFix <- "_1w_1w"
  
  path <- "C:/Users/Andreas/Dropbox/uni/DA/Implementation/Java/em-app/src/main/resources/data/simulationResults/aggregated/"
  
  prefix <- "test_"
  accMeasure <- "RMSE"
  accName <- paste0(prefix,accMeasure)
  
  matrixSimNames <- c()
  
  for ( s in 1:length(simPrefix)) {
    
    data <- forecastAggregateByWeek(simPrefix[s], simPostFix)
    
    printXTable(data, exponential=FALSE)
  }
  
}


forecastAggregateByWeek <- function(simPrefix, simPostFix) 
{
  modelAggregate <- c()
  weeks <- c("2 weeks", "3 weeks", "4 weeks")
  # week as training period
  for ( w in 2: 4 ) {
    
    simName <- paste0(simPrefix,w,"w",simPostFix)
    simName <- get(load(paste0(path,simName,".RData")))
    
    for(i in 1:length(modelNames)) {
      model <- i
      testValues <- c()
      for(h in 1:length(fcHorizons)) {
        acc <- simName[[1]][[model]][[h]]
        value <- acc[[accName]]
        if(!is.finite(value)) {
          value <- 0
        }
        testValues <- c(testValues,value)
      }
      modelAggregate <- append(modelAggregate, mean(testValues, na.rm=TRUE))
    }
    
  }
  
  RMSEmatrix <- matrix((modelAggregate),ncol=length(modelNames),byrow=TRUE)
  colnames(RMSEmatrix) <- modelNames
  rownames(RMSEmatrix) <- weeks
  RMSEmatrix <- as.table(RMSEmatrix)
  
  return (RMSEmatrix)

}


#' getRMSEForAggregatedFcHorizons (not finished!)
#' -------------------
#' @param simName the simulation name to compute the errors for
#' @param output 
getRMSEForAggregatedFcHorizons <- function(simName)
{
  simPrefix <- c()
  
  simPrefix[1] <- paste0("da_sim_1_")
  simPrefix[2] <- paste0("da_sim_2_")
  simPrefix[3] <- paste0("da_sim_4_")
  simPrefix[4] <- paste0("da_sim_6_")
  
  simPostFix <- "_1w_1w"
  
  path <- "C:/Users/Andreas/Dropbox/uni/DA/Implementation/Java/em-app/src/main/resources/data/simulationResults/aggregated/"
  
  prefix <- "test_"
  accMeasure <- "RMSE"
  accName <- paste0(prefix,accMeasure)
  
  modelAggregate <- c()
  matrixSimNames <- c()
  
  for ( s in 1:length(simPrefix)) {
    
    # week as training period
    for ( w in 2: 4 ) {
    
      simName <- paste0(simPrefix[s],w,"w",simPostFix)
      simName <- get(load(paste0(path,simName,".RData")))
      
      matrixSimNames <- append(matrixSimNames, paste0(s,"_",w,"w"))
      
      for(i in 1:length(modelNames)) {
        model <- i
        testValues <- c()
        for(h in 1:length(fcHorizons)) {
          acc <- simName[[1]][[model]][[h]]
          value <- acc[[accName]]
          if(!is.finite(value)) {
            value <- 0
          }
          testValues <- c(testValues,value)
        }
        modelAggregate <- append(modelAggregate, mean(testValues, na.rm=TRUE))
      }
      
    }
    
  }
  
  RMSEmatrix <- matrix((modelAggregate),ncol=length(modelNames),byrow=TRUE)
  colnames(RMSEmatrix) <- modelNames
  rownames(RMSEmatrix) <- modelNames
  RMSEmatrix <- as.table(RMSEmatrix)
  
  return (RMSEmatrix)
}




graphTest <- function() 
{
  ######################
  #### creating bar plot, RMSE
  
  # create besides bar plot for comparing RMSE error measures
  RMSE_test <- matrix(c(seq(1,55)),nrow=5,byrow=TRUE)
  colnames(RMSE_test) <- c("train","1h","3h","6h","12h","18h","24h","36h","48h","96h","168h")
  rownames(RMSE_test) <- c("ME","MAE","RMSE","MPE","MAPE")
  RMSE_test <- as.table(RMSE_test)
  RMSE_test
  barplot(RMSE_test,legend=T,beside=T,main='RMSE by forecast horizon',axis.lty = 1,args.legend = list(x="topleft"))

  
  # create besides bar plot for comparing RMSE error measures
  RMSE_test <- matrix(c(seq(1,20)),nrow=2,byrow=TRUE)
  colnames(RMSE_test) <- c("1h","3h","6h","12h","18h","24h","36h","48h","96h","168h")
  rownames(RMSE_test) <- c("Training set","Test set")
  RMSE_test <- as.table(RMSE_test)
  RMSE_test
  barplot(RMSE_test,legend=T,beside=T,main='RMSE by forecast horizon',axis.lty = 1)
  
  
  ############################
  ### rotate axis labels
  #   "las" represents the style of axis labels. 
  #   (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
  x <- barplot(simrun_1, space=0, las=1)
  text(cex=1, x=x+.25, y=-20, names(simrun_1), xpd=TRUE, srt=45, pos=2)
  
  x <- barplot(table(mtcars$cyl), xaxt="n")
  labs <- paste(names(table(mtcars$cyl)), "cylinders")
  text(cex=1, x=x-.25, y=-1.25, labs, xpd=TRUE, srt=45, pos=2)
}


testEvaluation <- function()
{
  priceDFTraining <- getEnergyData("/daprices/price/csv/1/2012-01-02/2014-01-15 23:00", TRUE)
  priceDFTest <- getEnergyData("/daprices/price/csv/1/2014-01-16/2014-01-22 23:00", TRUE)
  
  result <- evaluateModels(priceDFTraining, priceDFTest, output=TRUE)
}


#' evaluateModels: function for automatic model evaluation
#' -------------------
#' Calls the function to evaluate forecasting models based on the provided 
#' trainings and test data
#' @param priceDFTraining a data frame (read from csv) to provide training data
#' @param priceDFTest a data frame (read from csv) to provide test data
#' @param output logical, indicates whether progress information and results
#'        should be printed to console
evaluateModels <- function(priceDFTraining, priceDFTest, output=FALSE)
{
  if(output) {
    print("Start model evaluation")
  }
  pricesTraining <- priceDFTraining[,-1]
  pricesTest <- priceDFTest[,-1]
  
  result <- fcModelEvaluation(pricesTraining, pricesTest, output=output)
  
  return (result)
}


#' fcModelEvaluation: function for automatic generation of forecast accuracy measures
#' -------------------
#' Iterates through a list of defined models, generates the models based on 
#' the given trainings data, and computes accuracy measures for all models based on
#' the given test data
#' @param pricesTraining a list of energy prices used for model training
#' @param pricesTest a list of energy prices for testing model accuracy
#' @param output logical, indicates whether progress information and results
#'        should be printed to console
fcModelEvaluation <- function(pricesTraining, pricesTest, output=FALSE)
{
  if(output) {
    print("Generating ARIMA model ...")
  }
  resultArima <- generateARIMAModel(pricesTraining, targetPeriod = 24)
  modelArima <- resultArima[[1]]
  period <- resultArima[[2]]
  
  if(output) {
    print("ARIMA model generated")
    print(paste0("Generate models based on created time series with period ",period))
  }
  
  pricesTraining_ts <- ts(pricesTraining, frequency = period)
  
  modelMean <- meanf(pricesTraining_ts)
  modelSes <- HoltWinters(pricesTraining_ts, beta=FALSE, gamma=FALSE)
  modelHolt <- HoltWinters(pricesTraining_ts, beta=TRUE, gamma=FALSE)
  modelHoltWinters <- NULL
  if(period > 1  &&  period*2 <= length(pricesTraining)) {
    if(output) {
      print("Generating HoltWinters ...")
    }
    modelHoltWinters <- HoltWinters(pricesTraining_ts, beta=TRUE, gamma=TRUE)
  }
  modelTbats <- tbats(pricesTraining_ts)
  
  if(output) {
    print("Finished model generation")
    print("Getting accuracy measures ...")
  }
  
  modelList <- list(modelMean, modelSes, modelHolt, modelHoltWinters, modelArima, modelTbats)
  
  accuracyList <- list()
  
  fcHorizonList <- list(1,3,6,12,18,24,36,48,96,168)
  
  for(i in 1:length(modelList)) {
    model <- modelList[[i]]
    accuracyModelList <- list()
    for(j in 1:length(fcHorizonList)) {
      fcHorizon <- fcHorizonList[[j]]
      if(i == 1) {
        model <- meanf(pricesTraining_ts, h=fcHorizon)
      }
      accuracyModelList[[j]] <- accuracy(forecast(model, h=fcHorizon), pricesTest)
    }
    accuracyList[[i]] <- accuracyModelList
  }
  
  if(output) {
    print("Accuracy measures saved")
  }
  
  resultList <- list(modelList,accuracyList)
  
  return(resultList)
}



modelGenerationTest <- function()
{

  priceDF <- getEnergyData("/daprices/price/csv/localTZ/1/2014-07-07/2014-07-20 23:00", FALSE)
  
  priceDFTest <- getEnergyData("/daprices/price/csv/localTZ/1/2014-07-21/2014-07-28 23:00", FALSE)
  
  prices <- priceDF[,-1]
  
  prices_test <- priceDFTest[,-1]
  
  periods <- getPeriodsWithTarget(prices, target_period=24, output=TRUE, plot=TRUE)
  
  prices_ts <- ts(prices, frequency=periods)
  prices_ts_no_period <- ts(prices)
  prices_ts_no_period <- ts(prices, frequency=1)

  
  test_mean <- meanf(prices)
  test_mean_ts <- meanf(prices_ts)
  test_mean_no_period <- meanf(prices_ts_no_period)
  
  test_tbats <- tbats(prices)
  test_tbats_ts <- tbats(prices_ts)
  test_tbats_no_period <- tbats(prices_ts_no_period)
  
  test_arima <- auto.arima(prices)
  test_arima_ts <- auto.arima(prices_ts)
  test_arima_ts_no_period <- auto.arima(prices_ts_no_period)
  
  
  plot(test_mean)
  plot(test_mean_ts)
  plot(test_mean_no_period)
  
  plot(test_tbats)
  plot(test_tbats_ts)
  plot(test_tbats_no_period)
  
  plot(test_arima)
  plot(test_arima_ts)
  plot(test_arima_ts_no_period)
  
  plot(forecast(test_tbats_ts))
  
  plot(forecast(test_arima_ts_no_period, h=48))
  plot(forecast(test_arima_ts))
  
  arima_result <- generate_model(data=prices, periods=periods, output= TRUE, plot= TRUE)
  arima_result_no_bx <- generate_model(data=prices, periods=periods, boxcox=FALSE, output= TRUE, plot= TRUE)
  test_auto_arima <- auto.arima(prices)
  test_auto_arima_ts <- auto.arima(prices_ts)
  
  plot(forecast(arima_result))
  plot(forecast(arima_result_no_bx))
  plot(forecast(test_auto_arima, h=48))
  plot(forecast(test_auto_arima_ts, h=48))
  
  accuracy(forecast(test_mean), prices_test)
  
  accuracy(forecast(test_tbats), prices_test)
  
  accuracy(forecast(test_tbats_ts), prices_test)
  accuracy(forecast(arima_result), prices_test)
  accuracy(forecast(arima_result_no_bx), prices_test)
  accuracy(forecast(test_auto_arima_ts), prices_test)
  
  # test forecast results
  fc_arima_result <- forecast(arima_result, h=24)
  plot(fc_arima_result)
  fc_arima_result$mean
  
  
  # get frequency of a time series (contained in a model)
  # frequency(modelArima$x)
  
  #   > modelSes$SSE
  #   [1] 3635.941
  #   > modelHolt$SSE
  #   [1] 4116.123
  #   > modelHoltWinters$SSE
  #   [1] 2815.921
  
  # Idea: Double-Seasonal Holt-Winters Forecasting (dshw)
  
}



helsinkiDATest <- function() 
{
  helsinkiPriceDF <- getEnergyData("/daprices/price/csv/1/2014-06-22/2014-07-28", FALSE)
  
  helsinkiPriceDF$Date <- strptime(helsinkiPriceDF$Date, "%Y-%m-%d %H:%M")
  
  helsinkiDates <- helsinkiPriceDF$Date
  helsinkiPrices <- ts(helsinkiPriceDF[,-1])
  
  plot(helsinkiDates, helsinkiPrices, type='l', main='Helsinki prices, 5 weeks', 
       ylab='Price in € per MWh', xlab='')
  
  plot(helsinkiPriceDF$Date, helsinkiPriceDF[,-1], type='l', main='Helsinki prices, 5 weeks', 
       ylab='Price in € per MWh', xlab='')
  
  plot.ts(helsinkiPrices)
}


matrixTest <- function()
{
  
  # creating data frame and averaging over columns
  w=c(5,6,7,8)
  x=c(1,2,3,4)
  y=c(1,2,3)
  length(y)=4
  z=data.frame(w,x,y)
  colMeans(z, na.rm = TRUE)
  colMeansTest <- colMeans(z, na.rm = TRUE)
  colMeansTest[["w"]] # get value

  
  acc1[,"ME"] # access column "ME"
  acc1[1,"ME"] # access row 1 of column "ME"
  acc_list <- list(acc1, acc2) # create list of matrices
  acc_list[[1]] # access list element 1
  acc_list[[3]] <- acc3 # add new matrix as list element
  
  
  # list of lists (model list)
  acc_list2 <- list(acc2)
  acc_ext_list <- list(acc_list, acc_list2) # create list of lists
  acc_ext_list[[1]] # access first list
  acc_ext_list[[1]][[3]] # access 3rd element of first list
  acc_ext_list[[1]][[3]][,"RMSE"] # access column "RMSE" of ...
  acc_ext_list[[1]][[3]][2,"RMSE"] # access 2nd row of ...
  
}


csvTest <- function()
{
  
  write.csv(z, file = "MyData.csv",row.names=FALSE, na="")
  
  write.csv(model1_fc1, file = "model1_fc1.csv", na="")
}


table_output_test <- function()
{
  library(gridExtra)
  pdf("data_output.pdf", height=11, width=8.5)
  grid.table(RMSEmatrix)
  dev.off()
}


accuracyTest <- function()
{
  fit1 <- rwf(EuStockMarkets[1:200,1],h=100)
  fit2 <- meanf(EuStockMarkets[1:200,1],h=100)
  accuracy(fit1)
  accuracy(fit2)
  accuracy(fit1,EuStockMarkets[201:300,1])
  accuracy(fit2,EuStockMarkets[201:300,1])
  accuracy(fit1,EuStockMarkets[201:250,1])
  accuracy(fit2,EuStockMarkets[201:250,1])
  plot(fit1)
  lines(EuStockMarkets[1:300,1], col="black")
  plot(fit2)
}


tbatsTest <- function() {
  fit <- tbats(USAccDeaths, use.parallel=FALSE)
  plot(forecast(fit))
  
  # taylor
  # Half-hourly electricity demand in England and Wales from Monday 5 June 2000 to Sunday 27 August 2000. 
  # Discussed in Taylor (2003), and kindly provided by James W Taylor. 
  # -> 84 days, 2016 hours, 4032 half-hours
  # ATTENTION: took ~ 3 minutes
  taylor.fit <- tbats(taylor)
  taylor.fit.fc <- forecast(taylor.fit)
  taylor.fit.acc <- accuracy(taylor.fit.fc)
}


sesTest <- function() {
  # Rainfall in london #
  rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
  rainseries <- ts(rain,start=c(1813))
  plot.ts(rainseries)
  rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE) # just ses, no params
  rainseriesforecasts
  plot(rainseriesforecasts)
  
  rainseriesforecasts_h <- HoltWinters(rainseries, beta=TRUE, gamma=FALSE)
  rainseriesforecasts_h
  plot(rainseriesforecasts_h)
  
  rainseriesforecasts_hw <- HoltWinters(rainseries, beta=TRUE, gamma=TRUE)
  rainseriesforecasts_hw
  plot(rainseriesforecasts_hw)
  
  rainseriesforecasts_tbats <- tbats(rainseries) 
  rainseriesforecasts_tbats
  plot(rainseriesforecasts_tbats)
  
  rainseriesforecasts_mfc <- meanf(rainseries) # naive fc
  rainseriesforecasts_mfc
  plot(rainseriesforecasts_mfc)
  
  rainseriesforecasts_rwf <- rwf(rainseries) 
  rainseriesforecasts_rwf
  plot(rainseriesforecasts_rwf)
  
  rainseriesforecasts_ses <- ses(rainseries)
  rainseriesforecasts_ses
  plot(rainseriesforecasts_ses)
  
  # Forecasting rainfall (out-of-sample)
  rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
  rainseriesforecasts2
  plot.forecast(rainseriesforecasts2)
  
  rainseriesforecasts2_h <- forecast.HoltWinters(rainseriesforecasts_h, h=8)
  rainseriesforecasts2_h
  plot.forecast(rainseriesforecasts2_h)
  
  rainseriesforecasts2_tbats <- forecast(rainseriesforecasts_tbats, h=8)
  rainseriesforecasts2_tbats
  plot.forecast(rainseriesforecasts2_tbats)
  
  # Showing residuals
  rainseriesforecasts2$residuals
  plot(rainseriesforecasts2$residuals)
  
  # Calculating auto-correlation function of residuals
  acf(rainseriesforecasts2$residuals, lag.max=20)
  
  # Carry out a Ljung box test to test for evidence of non-correlated residuals
  Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
}

