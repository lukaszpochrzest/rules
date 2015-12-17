####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                                                                             ##########################################################
#############################################                             RULES GENERATION                                ##########################################################
#############################################                                                                             ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                               USE EXAMPLES                                  ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

               ################################################################
               ####################### continuous data ########################
               ################################################################

   rpartWineTrainingDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/winequality/sets_divided/winequality-red.csv.training", header = TRUE, sep = ";")
   rpartWinePruningDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/winequality/sets_divided/winequality-red.csv.pruning", header = TRUE, sep = ";")
   #rpartWineDataInputDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/winequality/winequality-red.csv", header = TRUE, sep = ";")   

   rpartWineTrainingSetDataTreeObject <- rpart(cp = 0, method = "anova", data =  rpartWineTrainingDataFrame, formula = quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

   plot(rpartWineTrainingSetDataTreeObject, uniform = TRUE)
   text(rpartWineTrainingSetDataTreeObject, all = FALSE, use.n = TRUE, cex = 0.75)

   # RULE SET GENERATION
   rpartWineTrainingDataRuleSet <- generateRuleSet(object = rpartWineTrainingSetDataTreeObject)

   # RULE SET PRUNING
   rpartWineDataRuleSetPruned <- pruneRuleSet(ruleSet = rpartWineTrainingDataRuleSet, pruningDataFrame = rpartWinePruningDataFrame, trainingDataFrame = rpartWineTrainingDataFrame, printLog = TRUE)

   # CLASSIFICATION AND ERROR COMPUTATION #need some new "real data" set, pruning data set slice used temporarily instead
   error <- predict(ruleSet = rpartWineTrainingDataRuleSet, toBeClassifiedDataFrame = rpartWinePruningDataFrame[1:5,], trainingDataFrame = rpartWineTrainingDataFrame, printLog = TRUE)

               ################################################################
               ######################### discrete data ########################
               ################################################################

   rpartNurseryTrainingDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/nursery/sets_divided/nursery_data.csv.training", header = TRUE, sep = ",")
   rpartNurseryPruningDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/nursery/sets_divided/nursery_data.csv.pruning", header = TRUE, sep = ",")
   #rpartNurseryInputDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/nursery/nursery_data.csv", header = TRUE, sep = ",")

   rpartNurseryTrainingSetDataTreeObject <- rpart(cp = 0, method = "class", data = rpartNurseryTrainingDataFrame, formula = nursery ~ parents + has_nurs + form + children + housing + finance + social + health)

   plot(rpartNurseryTrainingSetDataTreeObject, uniform = TRUE)
   text(rpartNurseryTrainingSetDataTreeObject, all = FALSE, use.n = TRUE, cex = 0.75)

   # RULE SET GENERATION
   rpartNurseryTrainingDataRuleSet <- generateRuleSet(object = rpartNurseryTrainingSetDataTreeObject)

   # RULE SET PRUNING
   rpartNurseryDataRuleSetPruned <- pruneRuleSet(ruleSet = rpartNurseryTrainingDataRuleSet,trainingDataFrame = rpartNurseryTrainingDataFrame, pruningDataFrame = rpartNurseryPruningDataFrame, printLog = TRUE)

   # CLASSIFICATION AND ERROR COMPUTATION #need some new "real data" set, pruning data set slice used temporarily instead
   error <- predict(ruleSet = rpartNurseryTrainingDataRuleSet, toBeClassifiedDataFrame = rpartNurseryPruningDataFrame[1:5,], trainingDataFrame = rpartNurseryTrainingDataFrame, printLog = TRUE)

####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

## TODO LIST
+ functions documentation
+ performance improvement (generateRuleSet() - categorical version!)
+ missing data handling
+ automatic missing data tests
+ automatic generateRuleSet() tests (done only by hand..)
+ automatic pruneRuleSet() tests (done only by hand..)
+ automatic predict() tests (done only by hand..)





