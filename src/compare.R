#trzeba ustawić w pliku naiveBayes.R libDir, żeby to jakoś z sensem śmieciło

workingDir <- "D:/ProgramyINNE/rpartRules/src/"
setwd( workingDir )

source("e1071naiveBayes.R")
source("rules.R")
#installBayes()
loadBayesLibs()





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
  modelRpart <- rpart(cp = 0, data =  rpartTrainingDataFrame, rpartFormula )
  

  # Rpart RULE SET GENERATION
  rpartRuleSet <- generateRuleSet(object = modelRpart, trainingDataFrame = rpartTrainingDataFrame)
  rpartRuleSetPruned <- pruneRuleSet(ruleSet = rpartRuleSet, pruningDataFrame = rpartPruningDataFrame, printLog = FALSE)
  

  predict1 <- predict(object = rpartRuleSet, newdata = rpartTestDataFrame, printLog = FALSE)
  predict2 <- predict(object = rpartRuleSetPruned, newdata = rpartTestDataFrame, printLog = FALSE)
  
  error1 <- predict1$error
  error2 <- predict2$error
  error3 <- bayesError(model = modelBayes, dataset = rpartTestDataFrame, modelRpart$method )
  
  return ( list(error1,error2,error3 ) )
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

