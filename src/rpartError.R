source("compare.R")


rulesError <- function()
{
  dataList <- list( "datasets/winequality", "datasets/spambase", "datasets/nursery1", "datasets/nursery2","datasets/nursery3" )
  errors <- computeErrorForDatasets( dataList )
  print( errors )
}


computeErrorForDatasets <- function( datasetsList )
{
  errorList <- numeric()
  for( dataset in datasetsList )
  {
    errorList <- append( errorList, compareDataset( dataset ) )
  }
  
  return( errorList )
}

rulesError()
#compareDataset( "datasets/winequality" )