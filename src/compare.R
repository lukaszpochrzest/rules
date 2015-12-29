#trzeba ustawiæ w pliku naiveBayes.R libDir, ¿eby to jakoœ z sensem œmieci³o

workingDir <- "D:/ProgramyINNE/rpartRules/src/"
setwd( workingDir )

source("e1071naiveBayes.R")
source("rules.R")
#installBayes()
loadBayesLibs()



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
  rpartFormula <- as.formula( paste(className, paste(attribNames, collapse=" + "), sep=" ~ ") )
  
  # Building bayes model
  modelBayes <- naiveBayes( formula = rpartFormula, data = rpartTrainingDataFrame )
  modelRpart <- rpart(cp = 0, method = "anova", data =  rpartTrainingDataFrame, rpartFormula )
  

  # Rpart RULE SET GENERATION
  rpartRuleSet <- generateRuleSet(object = modelRpart)
  rpartRuleSetPruned <- pruneRuleSet(ruleSet = rpartRuleSet, trainingDataFrame = rpartTrainingDataFrame, pruningDataFrame = rpartPruningDataFrame, printLog = FALSE)
  
  #modelRpart
  #modelBayes
  #error1 <- predict(ruleSet = rpartRuleSet, toBeClassifiedDataFrame = rpartTestDataFrame[1:5,], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  #error2 <- predict(ruleSet = rpartRuleSetPruned, toBeClassifiedDataFrame = rpartTestDataFrame[1:5,], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  
  #error1 <- rpartPredict(ruleSet = rpartRuleSet, toBeClassifiedDataFrame = rpartTestDataFrame[1:5,], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  #error2 <- rpartPredict(ruleSet = rpartWineDataRuleSetPruned, toBeClassifiedDataFrame = rpartWineTestDataFrame[1:5,], trainingDataFrame = rpartWineTrainingDataFrame, printLog = FALSE)
  #error1
}

