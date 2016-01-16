#trzeba ustawić w pliku naiveBayes.R libDir, żeby to jakoś z sensem śmieciło

workingDir <- "D:/ProgramyINNE/rpartRules/src/"
setwd( workingDir )

source("e1071naiveBayes.R")
source("rules.R")
#installBayes()
loadBayesLibs()


##
##
rpartCompare <- function( trainData, pruneData, testData, rpartFormula, cp)
{
  modelRpart <- rpart(cp = cp, data =  trainData, formula = rpartFormula )
  
  # Rpart RULE SET GENERATION
  rpartRuleSet <- generateRuleSet( object = modelRpart, trainingDataFrame = trainData )
  rpartRuleSetPruned <- prune( ruleSet = rpartRuleSet, pruningDataFrame = pruneData, printLog = FALSE)
  
  predict1 <- predict( object = rpartRuleSet, newdata = testData, printLog = FALSE)
  predict2 <- predict( object = rpartRuleSetPruned, newdata = testData, printLog = FALSE)
  #predict3 <- predict( object = modelRpart, newdata = testData, type = "class")
  
  resultError <- c( predict1$error, predict2$error )    #pureRpart = predict3
  names( resultError ) <- c( "ruleSet", "prunedRuleSet")
  
  return( list( rpartMethod = modelRpart$method, errorArray = resultError ) )
}



##  Compares datasets and returns list of errors
##
##
compareDataset <- function( filePrefix )
{
  # Make datasets files names from prefix set in parameter
  trainingFilePostfix <- paste( filePrefix, ".training", sep = "" )
  priningFilePostfix <- paste( filePrefix, ".pruning", sep = "" )
  testFilePostfix <- paste( filePrefix, ".test", sep = "" )
  
  # Read datasets
  rpartTrainingDataFrame <- read.csv(file = trainingFilePostfix, header = TRUE, sep = ";")
  rpartPruningDataFrame <- read.csv(file = priningFilePostfix, header = TRUE, sep = ";")
  rpartTestDataFrame <- read.csv(file = testFilePostfix, header = TRUE, sep = ";")
  
  # Extract column names form datasets and create formula
  colNames <- colnames( rpartTrainingDataFrame )
  attribNames <- colNames[-length(colNames)]
  className <- colNames[length(colNames)]
  attribString <- paste(attribNames, collapse=" + ")
  
  bayesFormula <- as.formula(paste("as.factor(", className, ") ~ ", attribString ))
  rpartFormula <- as.formula( paste(className, paste(attribNames, collapse=" + "), sep=" ~ ") )
  
  # Regression or classification
  classExample <- rpartTrainingDataFrame[ 2, length(colNames) ]
  
  # Building bayes model
  modelBayes <- naiveBayes( formula = bayesFormula, data = rpartTrainingDataFrame )
  
  # Building rpart model
  #modelRpart <- rpart(cp = 0.03, data =  rpartTrainingDataFrame, rpartFormula )
  

  # Rpart RULE SET GENERATION
  #rpartRuleSet <- generateRuleSet(object = modelRpart, trainingDataFrame = rpartTrainingDataFrame)
  #rpartRuleSetPruned <- prune(ruleSet = rpartRuleSet, pruningDataFrame = rpartPruningDataFrame, printLog = FALSE)
  

  #predict1 <- predict(object = rpartRuleSet, newdata = rpartTestDataFrame, printLog = FALSE)
  #predict2 <- predict(object = rpartRuleSetPruned, newdata = rpartTestDataFrame, printLog = FALSE)
  
  result0 <- rpartCompare( rpartTrainingDataFrame, rpartPruningDataFrame, rpartTestDataFrame, rpartFormula, 0)
  result1 <- rpartCompare( rpartTrainingDataFrame, rpartPruningDataFrame, rpartTestDataFrame, rpartFormula, 0.01)
  result2 <- rpartCompare( rpartTrainingDataFrame, rpartPruningDataFrame, rpartTestDataFrame, rpartFormula, 0.04)
  result3 <- rpartCompare( rpartTrainingDataFrame, rpartPruningDataFrame, rpartTestDataFrame, rpartFormula, 0.07)
  result4 <- rpartCompare( rpartTrainingDataFrame, rpartPruningDataFrame, rpartTestDataFrame, rpartFormula, 0.1)
  result5 <- rpartCompare( rpartTrainingDataFrame, rpartPruningDataFrame, rpartTestDataFrame, rpartFormula, 0.13)
  
  
  #error1 <- predict1$error
  #error2 <- predict2$error
  errorBayes <- bayesError(model = modelBayes, dataset = rpartTestDataFrame, result0$rpartMethod )
  
  return ( c(errorBayes, result0$errorArray, result1$errorArray, result2$errorArray, result3$errorArray, result4$errorArray, result5$errorArray) )
  #return ( list(errorBayes, error1, error2 ) )
}

## Function makes the same as compareDataset but only for bayes
##
##
bayesTest <- function( filePrefix )
{
  # Make datasets files names from prefix set in parameter
  trainingFilePostfix <- paste( filePrefix, ".training", sep = "" )
  priningFilePostfix <- paste( filePrefix, ".pruning", sep = "" )
  testFilePostfix <- paste( filePrefix, ".test", sep = "" )
  
  # Read datasets
  rpartTrainingDataFrame <- read.csv(file = trainingFilePostfix, header = TRUE, sep = ";")
  rpartPruningDataFrame <- read.csv(file = priningFilePostfix, header = TRUE, sep = ";")
  rpartTestDataFrame <- read.csv(file = testFilePostfix, header = TRUE, sep = ";")
  
  # Extract column names form datasets and create formula
  colNames <- colnames( rpartTrainingDataFrame )
  
  print(length(colNames))
  
  attribNames <- colNames[-length(colNames)]
  className <- colNames[length(colNames)]
  attribString <- paste(attribNames, collapse=" + ")
  rpartFormula <- as.formula(paste("as.factor(", className, ") ~ ", attribString ))
  
  # Building bayes model
  modelBayes <- naiveBayes( formula = rpartFormula, data = rpartTrainingDataFrame )
  print( rpartFormula )
  
  bayesError(model = modelBayes, dataset = rpartTestDataFrame)
}

