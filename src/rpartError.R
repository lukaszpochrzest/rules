source("compare.R")


rulesError <- function()
{
  dataList <- list( "datasets/winequality", "datasets/spambase", "datasets/nursery" )
  errors <- computeErrorForDatasets( dataList )
  print( errors )
}


computeErrorForDatasets <- function( datasetsList )
{
  errorList <- list()
  for( dataset in datasetsList )
  {
    errorList <- list( errorList, compareDataset( dataset ) )
  }
  
  return( errorList )
}

rulesError()
#compareDataset( "datasets/winequality" )