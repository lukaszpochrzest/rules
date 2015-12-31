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
  #attribNames <- colNames[-length(colNames)]
  className <- colNames[length(colNames)]
  #rpartFormula <- as.formula( paste(className, paste(attribNames, collapse=" + "), sep=" ~ ") )
  rpartFormula <- as.formula(paste("as.factor(", className, ") ~ ."))
  
  # Building bayes model
  modelBayes <- naiveBayes( formula = rpartFormula, data = rpartTrainingDataFrame )
  modelRpart <- rpart(cp = 0, method = "anova", data =  rpartTrainingDataFrame, rpartFormula )
  

  # Rpart RULE SET GENERATION
  rpartRuleSet <- generateRuleSet(object = modelRpart)
  rpartRuleSetPruned <- pruneRuleSet(ruleSet = rpartRuleSet, trainingDataFrame = rpartTrainingDataFrame, pruningDataFrame = rpartPruningDataFrame, printLog = FALSE)
  

  error1 <- predict(object = rpartRuleSet, newdata = rpartTestDataFrame[,1:ncol(rpartTestDataFrame)], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  #error1
  error2 <- predict(object = rpartRuleSetPruned, newdata = rpartTestDataFrame[,1:ncol(rpartTestDataFrame)], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  #error2
  bayesError(model = modelBayes, dataset = rpartTestDataFrame)
  
  return ( list(error1,error2) )
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
  attribNames <- colNames[-length(colNames)]
  className <- colNames[length(colNames)]
  #className <- as.factor( className )
  #rpartFormula <- as.formula( paste( className, paste(attribNames, collapse=" + "), sep=" ~ ") )
  attribString <- paste(attribNames, collapse=" + ")
  rpartFormula <- as.formula(paste("as.factor(", className, ") ~ ", attribString ))
  
  # Building bayes model
  modelBayes <- naiveBayes( formula = rpartFormula, data = rpartTrainingDataFrame )
  #print( rpartFormula )
  
  bayesError(model = modelBayes, dataset = rpartTestDataFrame)
}

