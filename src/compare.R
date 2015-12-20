#trzeba ustawiæ w pliku naiveBayes.R libDir, ¿eby to jakoœ z sensem œmieci³o

workingDir <- "D:/ProgramyINNE/ClassificationR/"
setwd( workingDir )

source("e1071naiveBayes.R")
source("rules.R")
#installBayes()
loadBayesLibs()



compareDataset <- function( filePrefix )
{
  trainingFilePostfix <- paste( filePrefix, ".training", sep = "" )
  priningFilePostfix <- paste( filePrefix, ".pruning", sep = "" )
  testFilePostfix <- paste( filePrefix, ".test", sep = "" )
  
  rpartTrainingDataFrame <- read.csv(file = trainingFilePostfix, header = TRUE, sep = ";")
  rpartPruningDataFrame <- read.csv(file = priningFilePostfix, header = TRUE, sep = ";")
  rpartTestDataFrame <- read.csv(file = testFilePostfix, header = TRUE, sep = ";")
  
  colNames <- colnames( rpartTrainingDataFrame )
  attribNames <- colNames[-length(colNames)]
  className <- colNames[length(colNames)]
  rpartFormula <- as.formula( paste(className, paste(attribNames, collapse=" + "), sep=" ~ ") )
  
  
  #datasetBayes <- makeTestDataset(rpartTrainingDataFrame, ncol(rpartTrainingDataFrame))
  #modelBayes <- buildNaiveBayes( datasetBayes$classes, datasetBayes$attributes )
  modelBayes <- naiveBayes( formula = rpartFormula, data = rpartTrainingDataFrame )
  modelRpart <- rpart(cp = 0, method = "anova", data =  rpartTrainingDataFrame, rpartFormula )
  
  #rpartFormula
  #quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol
  modelRpart$call$formula[0]
  # RULE SET GENERATION
  #rpartRuleSet <- generateRuleSet(object = modelRpart)
  
  
  #modelRpart
  #modelBayes
  
  #error1 <- rpartPredict(ruleSet = rpartRuleSet, toBeClassifiedDataFrame = rpartTestDataFrame[1:5,], trainingDataFrame = rpartTrainingDataFrame, printLog = FALSE)
  #error2 <- rpartPredict(ruleSet = rpartWineDataRuleSetPruned, toBeClassifiedDataFrame = rpartWineTestDataFrame[1:5,], trainingDataFrame = rpartWineTrainingDataFrame, printLog = FALSE)
}

