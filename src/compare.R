#trzeba ustawiæ w pliku naiveBayes.R libDir, ¿eby to jakoœ z sensem œmieci³o

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
  rpartFormula <- as.formula(paste("as.factor(", className, ") ~ ", attribString ))
  
  # Regression or classification
  classExample <- rpartTrainingDataFrame[ 1, length(colNames) ]
  if( is.character(classExample))
    useMethod <- "class"
  else if( is.numeric(classExample))
    useMethod <- "anova"
  
  # Building bayes model
  modelBayes <- naiveBayes( formula = rpartFormula, data = rpartTrainingDataFrame )
  modelRpart <- rpart(cp = 0, method = "anova", data =  rpartTrainingDataFrame, rpartFormula )
  

  # Rpart RULE SET GENERATION
  rpartRuleSet <- generateRuleSet(object = modelRpart)
  rpartRuleSetPruned <- pruneRuleSet(ruleSet = rpartRuleSet, trainingDataFrame = rpartTrainingDataFrame, pruningDataFrame = rpartPruningDataFrame, printLog = FALSE)
  

  error1 <- predict(object = rpartRuleSet, newdata = rpartTestDataFrame[,1:ncol(rpartTestDataFrame)], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  error2 <- predict(object = rpartRuleSetPruned, newdata = rpartTestDataFrame[,1:ncol(rpartTestDataFrame)], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  error3 <- bayesError(model = modelBayes, dataset = rpartTestDataFrame)
  
  return ( list(error1,error2,error3) )
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

