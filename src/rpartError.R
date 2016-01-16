source("compare.R")


rulesError <- function()
{
  dataList <- list( "datasets/winequality", "datasets/spambase", "datasets/nursery1", "datasets/nursery2","datasets/nursery3" )
  errors <- computeErrorForDatasets( dataList )
  print( errors )
}


computeErrorForDatasets <- function( datasetsList )
{
  errorList <- list()
  for( dataset in datasetsList )
  {
    errorList <- append( errorList, list( compareDataset( dataset ) ) )
  }
  
  errorMatrix = do.call(cbind, errorList)
  
  return( errorMatrix )
}

rulesError()
#compareDataset( "datasets/winequality" )