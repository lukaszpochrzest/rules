
source( "rules.R")

rpartWineTrainingDataFrame <- read.csv(file = "datasets/winequality.training", header = TRUE, sep = ";")
rpartWinePruningDataFrame <- read.csv(file = "datasets/winequality.pruning", header = TRUE, sep = ";")
rpartWineTestDataFrame <- read.csv(file = "datasets/winequality.test", header = TRUE, sep = ";")
rpartWineTrainingSetDataTreeObject <- rpart(cp = 0, method = "anova", data =  rpartWineTrainingDataFrame, formula = quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

# RULE SET GENERATION
rpartWineTrainingDataRuleSet <- generateRuleSet(object = rpartWineTrainingSetDataTreeObject, trainingDataFrame = rpartWineTrainingDataFrame)

# RULE SET PRUNING
rpartWineDataRuleSetPruned <- pruneRuleSet(ruleSet = rpartWineTrainingDataRuleSet, pruningDataFrame = rpartWinePruningDataFrame, printLog = FALSE)

# CLASSIFICATION AND ERROR COMPUTATION #need some new "real data" set, pruning data set slice used temporarily instead
error1 <- predict(object= rpartWineTrainingDataRuleSet, newdata = rpartWineTestDataFrame[1:5,], printLog = FALSE)
error2 <- predict(object = rpartWineDataRuleSetPruned, newdata = rpartWineTestDataFrame[1:5,], printLog = FALSE)

#modelBayes <- naiveBayes(quality ~ ., data = rpartWineTrainingDataFrame, laplace = 3)
#bayesPrediction <- predict( modelBayes, rpartWineTestDataFrame[,-ncol(rpartWineTestDataFrame)] )

source("compare.R")
compareDataset("datasets/winequality")
compareDataset("datasets/spambase")
compareDataset("datasets/nursery")
