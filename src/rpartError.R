source("compare.R")


rulesError <- function()
{
  dataList <- list( "datasets/winequality", "datasets/spambase", "datasets/nursery" )
  #dataList <- list( "datasets/winequality" )
  errors <- computeErrorForDatasets( dataList )
  print( errors )
  return ( errors )
}


computeErrorForDatasets <- function( datasetsList )
{
  cpList <- c( 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.3, 0.05, 0.07, 0.1, 0.5 )
  minSplitsBuckets <- list( c( 1, 1 ), c( 10, 3), c(20, 7), c(40, 14), c( 100, 10), c( 100, 40 ))
  
  
  errorList <- list()
  for( dataset in datasetsList )
  {
    errorList <- append( errorList, list( compareDataset( dataset, cpList, minSplitsBuckets ) ) )
  }
  
  errorMatrix = do.call(cbind, errorList)
  
  
  datasetsNamesArray <- c()
  for( datasetName in datasetsList )
  {
    datasetsNamesArray <- c( datasetsNamesArray, datasetName )
  }
  

  colnames( errorMatrix ) <- datasetsNamesArray
  
  return( errorMatrix )
}

errorSet <- rulesError()
#compareDataset( "datasets/winequality" )