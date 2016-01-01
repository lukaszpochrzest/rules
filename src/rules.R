

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
#
#               ################################################################
#               ####################### continuous data ########################
#               ################################################################
#
#   rpartWineTrainingDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/winequality/sets_divided/winequality-red.csv.training", header = TRUE, sep = ";")
#   rpartWinePruningDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/winequality/sets_divided/winequality-red.csv.pruning", header = TRUE, sep = ";")
#   #rpartWineDataInputDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/winequality/winequality-red.csv", header = TRUE, sep = ";")   
#
#   rpartWineTrainingSetDataTreeObject <- rpart(cp = 0, method = "anova", data =  rpartWineTrainingDataFrame, formula = quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)
#
#   plot(rpartWineTrainingSetDataTreeObject, uniform = TRUE)
#   text(rpartWineTrainingSetDataTreeObject, all = FALSE, use.n = TRUE, cex = 0.75)
#
#   # RULE SET GENERATION
#   rpartWineTrainingDataRuleSet <- generateRuleSet(object = rpartWineTrainingSetDataTreeObject, trainingDataFrame = rpartWineTrainingDataFrame)
#
#   # RULE SET PRUNING
#   rpartWineDataRuleSetPruned <- prune(ruleSet = rpartWineTrainingDataRuleSet, pruningDataFrame = rpartWinePruningDataFrame, printLog = TRUE)
#
#   # CLASSIFICATION AND ERROR COMPUTATION #need some new "real data" set, pruning data set slice used temporarily instead
#   error <- predict(object = rpartWineTrainingDataRuleSet, newdata = rpartWinePruningDataFrame[1:5,], printLog = TRUE)
#
#               ################################################################
#               ######################### discrete data ########################
#               ################################################################
#
#   rpartNurseryTrainingDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/nursery/sets_divided/nursery_data.csv.training", header = TRUE, sep = ",")
#   rpartNurseryPruningDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/nursery/sets_divided/nursery_data.csv.pruning", header = TRUE, sep = ",")
#   #rpartNurseryInputDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/nursery/nursery_data.csv", header = TRUE, sep = ",")
#   
#   #cp = 0, 
#   rpartNurseryTrainingSetDataTreeObject <- rpart(method = "class", data = rpartNurseryTrainingDataFrame, formula = nursery ~ parents + has_nurs + form + children + housing + finance + social + health)
#
#   plot(rpartNurseryTrainingSetDataTreeObject, uniform = TRUE)
#   text(rpartNurseryTrainingSetDataTreeObject, all = FALSE, use.n = TRUE, cex = 0.75)
#
#   # RULE SET GENERATION
#   rpartNurseryTrainingDataRuleSet <- generateRuleSet(object = rpartNurseryTrainingSetDataTreeObject, trainingDataFrame = rpartNurseryTrainingDataFrame)
#
#   # RULE SET PRUNING
#   rpartNurseryDataRuleSetPruned <- prune(ruleSet = rpartNurseryTrainingDataRuleSet, pruningDataFrame = rpartNurseryPruningDataFrame, printLog = TRUE)
#
#   # CLASSIFICATION AND ERROR COMPUTATION #need some new "real data" set, pruning data set slice used temporarily instead
#   error <- predict(object = rpartNurseryTrainingDataRuleSet, newdata = rpartNurseryPruningDataFrame[1:5,], printLog = TRUE)
#
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

require(rpart)

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                            CLASS DEFINITIONS                                ##########################################################
#############################################                                                                             ##########################################################
#############################################            CategoricalSelector, ContinuousSelector, Complex and Rule        ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

CategoricalSelector <- function(decisionVariable, possibleValues)
{
  me <- list(type = "categorical", decisionVariable = decisionVariable, possibleValues = possibleValues)
  class(me) <- append(class(me),"CategoricalSelector")
  return(me)
}

ContinuousSelector <- function(decisionVariable, cutpoint, relation)
{
  me <- list(type = "continuous", decisionVariable = decisionVariable, cutpoint = cutpoint, relation = relation)
  class(me) <- append(class(me),"ContinuousSelector")
  return(me)
}

Complex <- function()
{
  me <- list()
  class(me) <- append(class(me), "Complex")
  return(me)
}

Consequent <- function(consequentKey, consequentValue)
{
  me <- list(consequentKey = consequentKey, consequentValue = consequentValue)
  class(me) <- append(class(me), "Consequent")
  return(me)
}

Rule <- function(complex, consequent)
{
  me <- list(complex = complex, consequent = consequent)
  class(me) <- append(class(me), "Rule")
  return(me)
}

RuleSet <- function(ruleList, trainingDataFrame) {
  me <- list(ruleList = ruleList, trainingDataFrame = trainingDataFrame)
  class(me) <- append(class(me), "ruleset")
  return(me)
}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                              OVERLOADED METHODS                             ##########################################################
#############################################                                                                             ##########################################################
#############################################                      check(), as.character() and print()                    ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

.check <- function(selector, sample)
{
  UseMethod("check",selector)
}

.check.CategoricalSelector <- function(selector, sample) # sample may be both singular (1 row data frame) or multiple
{
  return(sapply(sample[[selector$decisionVariable]], function (x) {any(x == selector$possibleValues)}))
}

.check.ContinuousSelector <- function(selector, sample)  # sample may be both singular (1 row data frame) or multiple
{
  if(selector$relation == ">=")
  {
    return (sapply(sample[[selector$decisionVariable]], ">=", selector$cutpoint))  #  wersja bez lapply
  }
  else if(selector$relation == "<")
  {
    return (sapply(sample[[selector$decisionVariable]], "<", selector$cutpoint))  # wersja bez lapply
  }
  else#just to make sure
  {
    stop(paste("Error. Unknown Continous selector relation ", selector$relation))
  }
}
.check.Complex <- function(complex, sample)# sample may be both singular (1 row data frame) or multiple, complex ma be both singular(1 selector) or multiple
{
  selectorCheckResult <- sapply(complex, function(selector)
  {
    checkResult <- .check(selector, sample)
    if(is.vector(checkResult))
    { # in fact.. if sample is single
      checkResult <- matrix(checkResult, ncol = length(checkResult))
    }
    return (checkResult)
  } )
  if(is.vector(selectorCheckResult)) {
    selectorCheckResult <- matrix(selectorCheckResult, ncol = length(selectorCheckResult))
  }
  return (apply(selectorCheckResult, 1, function(x) { all(x) }))
}

.check.Rule <- function(rule, sample)
{
  return (.check(rule$complex,sample))
}

as.character.CategoricalSelector <- function(x, ...)
{
  return(paste(x$decisionVariable, "=", paste(x$possibleValues, collapse = "||" )))
}

as.character.ContinuousSelector <- function(x, ...)
{
  return(paste(x$decisionVariable, x$relation, x$cutpoint))
}

as.character.Complex <- function(x, ...)
{
  return(paste(lapply(x, as.character),collapse = " AND "))
}

as.character.Consequent <- function(x, ...)
{
  return(paste(x$consequentKey, " CLASSIFIED AS  ", x$consequentValue))
}

as.character.Rule <- function(x, ...)
{
  return(paste(as.character(x$complex), " => ", as.character(x$consequent)))
}


print.CategoricalSelector <- function(x, ...)
{
  print(as.character(x))
} 

print.ContinuousSelector <- function(x, ...)
{
  print(as.character(x))
}

print.Complex <- function(x, ...)
{
  print(as.character(x))
}

print.Rule <- function(x, ...)
{
  print(as.character(x))
}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                         RULE SET GENERETING FUNCTIONS                       ##########################################################
#############################################                                                                             ##########################################################
#############################################       generateRuleSet() - funkcja używana przez zewnętrznego użytkownika    ##########################################################
#############################################                                                                             ##########################################################
#############################################                                                                             ##########################################################
#############################################       generatePaths() - funkcja wewnętrzna, generuje ścieżki z drzewa       ##########################################################
#############################################                         (tzn. sekwencje numerów węzłów drzewa)              ##########################################################
#############################################                                                                             ##########################################################
#############################################       generateRuleSetUsingPaths() = funkcja wewnętrzna, właściwa funkcja    ##########################################################
#############################################                         używana do generowania zbiorów reguł                ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

#' Generates set of rules from a rpart object.
#'
#' @param object An rpart object
#' @param trainingDataFrame Data frame that was used to build a \code{object}
#' @return Generated set of rules
#' @examples
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' ruleSet <- generateRuleSet(fit)
#' print(ruleSet)
generateRuleSet <- function(object, trainingDataFrame)
{
  generatedPaths <- .generatePaths(frame = object$frame)
  return(.generateRuleSetUsingPaths(paths = generatedPaths, object = object, trainingDataFrame))
}

.generatePaths <- function(frame)
{
  is.leaf <- (frame$var == "<leaf>")
  leaves <- as.integer(row.names(frame))[is.leaf]
  leaves_size <- length(leaves)
  
  paths <- list()
  paths[1:leaves_size] <- leaves[1:leaves_size] 

  parents <- leaves
  while(1)
  {
    parents <- (parents - parents%%2L)/2L
    if(all(parents<1L))
    {
      break;
    }
    
    for(i in 1: leaves_size)
    {
      if(parents[i]>0)
      {
        paths[[i]] <- append(paths[[i]],parents[i])     
      }
    }
    
  }
  return(paths)
}

.generateRuleSetUsingPaths <- function(paths, object, trainingDataFrame)
{
  variableClassifyOn <- as.character( attr(object$terms,"variables")[[2]] ) #as.character(object$call$formula[[2]])  # variable we are classifing (on?) (:= variable we are trying to guess)
  paths_size <- length(paths)
  paths_conditions <- list()
  paths_conditions[1:paths_size] <- rep(NULL,paths_size)[1:paths_size]
  #paths_conditions[1:paths_size] <- rep(list(),paths_size)[1:paths_size]
  
  no_leaf_frame <- object$frame[object$frame$var!="<leaf>",]
  index <- cumsum(c(1, no_leaf_frame$ncompete + no_leaf_frame$nsurrogate + 1))
  irow <- index[c(rep(TRUE, length(index)-1), FALSE)]
  #ncat <- object$splits[irow,2L]
  xlevels <- attr(object, "xlevels")
  ylevels <- attr(object, "ylevels")
  
  for(i in 1:paths_size)
  {
    path <- paths[[i]]
    path.length <- length(path)
    
    ####
    ####
    #result class/number
    #leaf.id <- path[1]
    leaf.id <- path[1L]
    frame.row.index <- which(row.names(object$frame)==leaf.id)
    if(length(frame.row.index)==0)
    {                             #check if which function failed
      stop(paste("INTERNAL ERROR. Frame row with leaf.id ", leaf.id, " not found while getting path result class."))
    }
    yval <- object$frame[frame.row.index,5L]
    result <- NULL
    if(object$method == "anova")
    {
      result <- yval
      #paths_conditions[[i]][[1]] <- list(result=result)
    }
    else if(object$method == "class")
    {
      result <- ylevels[yval]
      #paths_conditions[[i]][[1]] <- list(result=result)
    }
    else
    {
      stop(paste("Error. Rpart method ", object$method, " not supported."))
    }
    ####
    ####
    
    if(path.length == 1)
    {
      next;
    }
    
    ####
    ####
    #selectors
    complex <- Complex()
    for(path_node_iter in 2:path.length)
    {
      node.id <- path[path_node_iter]
      previous_node.id <- path[path_node_iter-1]
      frame.row.index <- which(row.names(no_leaf_frame)==node.id)
      decision_variable <- paste(no_leaf_frame$var[frame.row.index])
      splits_index_column_value <- object$splits[irow[frame.row.index],4L]
      ncat <- object$splits[irow[frame.row.index],2L]
      previous_node.is_right_child <- ifelse(previous_node.id%%2==1, TRUE, FALSE)
      if(ncat > 1L )
      {               # is decision variable categorical ?
        csplit_row_index <- splits_index_column_value
        csplit_go_right_left_value <- ifelse(previous_node.is_right_child, 3L, 1L)
        xlevel_index <- which(attributes(xlevels)$names==decision_variable)
        decision_variable.values_to_child <- xlevels[[xlevel_index]][object$csplit[csplit_row_index, ]==csplit_go_right_left_value]
        #print(paste(i,":", path_node_iter," ", decision_variable, "/", decision_variable.values_to_child))
        
        type <- "categorical"
        
        ##
        #condition <- paste(decision_variable,"=",ifelse(previous_node.is_right_child,"R","L"),paste(decision_variable.values_to_child,collapse=" "))
        #if(path_node_iter == 2)
        #{
        #  paths_conditions[[i]] <- condition
        #}
        #else 
        #{
        #  paths_conditions[[i]] <- append(paths_conditions[[i]],condition)  
        #}
        ##
        
        #selector <- list(type = type, decision_variable = paste(decision_variable), possible_values = decision_variable.values_to_child)
        selector <- CategoricalSelector(decisionVariable = decision_variable, possibleValues = decision_variable.values_to_child)
        #paths_conditions[[i]][[path_node_iter]] <- selector
        complex[[path_node_iter-1]] <- selector
      }
      else
      {               # is decision variable continuous ?
        type <- "continuous"
        cutpoint <- splits_index_column_value
        relation <- ifelse(previous_node.is_right_child,ifelse(ncat<0L,">=","<"),ifelse(ncat<0L,"<",">="))
        
        #selector <- list(type = type, decision_variable = decision_variable, cutpoint = cutpoint, relation = relation)
        selector <- ContinuousSelector(decisionVariable = decision_variable, cutpoint = cutpoint, relation = relation)
        #paths_conditions[[i]][[path_node_iter]] <- selector
        complex[[path_node_iter-1]] <- selector
      }
      
    }
    ####
    ####
    
    #paths_conditions[[i]][[2]] <- complex
    paths_conditions[[i]] <- Rule(complex = complex, consequent = Consequent(consequentKey = variableClassifyOn, consequentValue = result))
  }
  ruleSet <- RuleSet(ruleList = paths_conditions, trainingDataFrame = trainingDataFrame)
  return(ruleSet)
  
}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                          RULE SET PRUNING FUNCTIONS                         ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

prune <- function(ruleSet, ...)
{
  UseMethod("prune")
}

#' Prunes rule set
#'
#' @param ruleSet Set of rules to prune
#' @param pruningDataFrame Data used to prune rules
#' @param trainingDataFrame Data that was used to train source rpart model
#' @param printLog Boolean value tells whether to print additinal information while pruning rules or not
#' @return Pruned rule set
#' @examples
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' ruleSet <- generateRuleSet(fit)
#' prunedRuleSet <- pruneRuleSet(ruleSet, kyphosis, kyphosis)
#' print(prunedRuleSet)
prune.ruleset <- function(ruleSet, pruningDataFrame, printLog = FALSE)
{
  if (!inherits(ruleSet, "ruleset")) stop("Not a legitimate \"ruleset\" object")
  ruleList <- ruleSet$ruleList
  trainingDataFrame <- ruleSet$trainingDataFrame
  ruleListPruned <- lapply(ruleList, function(x)
    {
      log("----------------------------------------------------------------", printLog)
      log("----------------------------------------------------------------", printLog)
      .pruneRule(rule = x, pruningDataFrame = pruningDataFrame, trainingDataFrame = trainingDataFrame, printLog = printLog)
    })
  #class(ruleSetPruned) <- "ruleset"
  ruleSetPruned <- RuleSet(ruleList = ruleListPruned, trainingDataFrame = trainingDataFrame)
  return (ruleSetPruned)
}

.pruneRule <- function(rule, pruningDataFrame, trainingDataFrame, debugMaxIters = 0, printLog = FALSE) # use trainingDataFrame to calculate new rule consequent
{
  complex <- rule$complex
  complexLength <- length(complex)
  if(complexLength < 2) # what about up-to-root prunings(<1 condition)?
  {
    return (rule)
  }
  
  prunedComplex <- complex
  variableClassifyOn <- rule$consequent$consequentKey
  classifiedAs <- rule$consequent$consequentValue
  
  prunedComplexError <- .computeErrorComplex(complex = prunedComplex, dataFrame = pruningDataFrame, variableClassifyOn = variableClassifyOn, classifiedAs = classifiedAs)
  
  debugIterationsCount <- 0
  i<-1
  pruned <- TRUE
  while(1)
  {
    l <- length(prunedComplex)
    
    if(l == 1)
    {
      break
    }
    if(i>l)
    {
      break
    }
    
    debugIterationsCount <- debugIterationsCount+1
    prunedComplexTry <- prunedComplex
    
    ##
    log("----------------------------------------------------------------", printLog)
    log(paste(debugIterationsCount, ". trying to remove selector: ", as.character(prunedComplexTry[[i]])), printLog)
    ##
    
    prunedComplexTry[[i]] <- NULL
    
    variableClassifyOn <- rule$consequent$consequentKey
    classifiedAs <- rule$consequent$consequentValue
    
    prunedComplexTryError <- .computeErrorComplex(complex = prunedComplexTry, dataFrame = pruningDataFrame, variableClassifyOn = variableClassifyOn, classifiedAs = classifiedAs)
    
    ##
    log(paste("new err vs old: ",prunedComplexTryError, " vs ", prunedComplexError), printLog)
    ##
    
    pruned <- FALSE
    
    if(prunedComplexError >= prunedComplexTryError)
    {
      prunedComplex <- prunedComplexTry
      prunedComplexError <- prunedComplexTryError
      pruned <- TRUE
      
      ################################################################################################################################
      ##### compute new class !FROM TRAINING (NOT PRUNING) SET! - so that we dont get too overfitted to pruning set ##################
      ################################################################################################################################
      tempOldClassifiedAs <- classifiedAs
      if(is.character(classifiedAs))
      {   # "class"

        trainingDataCoveredSamples <- trainingDataFrame[.check(prunedComplex, trainingDataFrame), variableClassifyOn]
        classifiedAs <- attr(which.max(table(trainingDataCoveredSamples)), "names")
      }
      else if(is.numeric(classifiedAs))
      {
        trainingDataCoveredSamples <- trainingDataFrame[.check(prunedComplex, trainingDataFrame), variableClassifyOn]
        classifiedAs <- mean(trainingDataCoveredSamples)
      }
      else #just in case
      {
        stop(paste("Error. Unknown type of rule consequent: ", classifiedAs))
      }
      ################################################################################################################################
      
      log(paste("-------->pruned! old class: ", tempOldClassifiedAs ," changed to ",classifiedAs), printLog)
    }
    
    if(pruned == FALSE)
    {
      i<-i+1
      log("not pruned!", printLog)
    }
    if(debugIterationsCount == debugMaxIters)
    {
      break
    }
  }
  rule$complex <-prunedComplex
  return (rule)
}

.computeErrorComplex <- function(complex, dataFrame, variableClassifyOn, classifiedAs) # complex must be single (from single rule)
{
  if(is.character(classifiedAs))
  {   # "class"
    coveredSamplesBool <- .check(complex, dataFrame)
    #variableClassifyOn <- rule$consequent$consequentKey
    #classifiedAs <- rule$consequent$consequentValue
    coveredSamples <- dataFrame[coveredSamplesBool, variableClassifyOn]
    #print(paste("covered samples:", paste(coveredSamples, collapse=" ")))
    #newConsequentValue <- attr(which.max(table(coveredSamples)), "names") # most frequent value in coveredSamples   # attr(which.max(table(rpartNurseryTrainingDataFrame[1:10,]["social"])), "names") 
    coveredSamplesCount <- length(coveredSamples)
    #log(paste("computeErrorComplex().coveredSamplesCount:",coveredSamplesCount), TRUE)
    if(coveredSamplesCount == 0)
    {
      return (0)
    }
    positivelyClassifiedCount <- sum(coveredSamples == classifiedAs)
    return ((coveredSamplesCount-positivelyClassifiedCount)/coveredSamplesCount)
  }
  else if(is.numeric(classifiedAs))
  {   # "continuous"
    coveredSamplesBool <- .check(complex, dataFrame)
    coveredSamples <- dataFrame[coveredSamplesBool, variableClassifyOn]
    coveredSamplesCount <- length(coveredSamples)
    #log(paste("computeErrorComplex().coveredSamplesCount:",coveredSamplesCount), TRUE)
    if(coveredSamplesCount == 0)
    {
      return (0)
    }
    mse <- sum(sapply(coveredSamples, function(x) {(x-classifiedAs)^2}))/coveredSamplesCount #sum(sapply(rpartWineDataInputDataFrame[1:4,"quality"], function(x) (x - mean(rpartWineDataInputDataFrame[1:4,"quality"]))^2))/length(rpartWineDataInputDataFrame[1:4,"quality"])
    return (mse)
  }
  else#just in case
  {
    stop(paste("Error. Unknown type of rule consequent: ", classifiedAs))
  }
}

#computeErrorRuleSingle <- function(rule, dataFrame) # rule must be single
#{
#  coveredSamplesBool <- check(rule, dataFrame)
#  variableClassifyOn <- rule$consequent$consequentKey
#  classifiedAs <- rule$consequent$consequentValue
#  coveredSamples <- dataFrame[coveredSamplesBool, variableClassifyOn]
#  coveredSamplesCount <- length(coveredSamples)
#  print(paste("coveredSamplesCount:",coveredSamplesCount))
#  if(coveredSamplesCount == 0)
#  {#print("tu")
#    return (0)
#  }
#  positivelyClassifiedCount <- sum(coveredSamples == classifiedAs)
#  return ((coveredSamplesCount-positivelyClassifiedCount)/coveredSamplesCount)
#}

#computeErrorRuleMultiple <- function(rules, dataFrame)
#{
#  return (sapply(rules, computeErrorRuleSingle, dataFrame))
#}

##performance version
#computeErrorRuleMultiple <- function(rule, dataFrame)
#{
#  #lapply(rpartNurseryDataRuleSet[1:3],check, rpartNurseryDataInputDataFrame[1:10,])
#  #samplesCovered <- lapply(rule, check, dataFrame)
#  coveredSamplesBool <- sapply(rule, check, dataFrame)
#  #lapply(rpartNurseryDataRuleSet[1:3], function(x) {x$consequent$consequentValue})
#  variableClassifyOn <- lapply(rule, function(x) {x$consequent$consequentValue})
#  classifiedAs <- lapply(rule, function(x) {x$consequent$consequentKey})
#  #apply(samplesCovered,2, function(x) {rpartNurseryDataInputDataFrame[1:10,][x,]})
#  coveredSamplesFullInfo <- apply(coveredSamplesBool, 2, function(x) {dataFrame[x,]})
#  ##DONT KNOW WHAT NEXT...
#}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                       CLASSIFICATION USING RULE SET                         ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

predict <- function(object, ...)
{
  UseMethod("predict")
}

#' Returns a vector of predicted responses from a ruleSet object.
#'
#' @param object RuleSet object used to predict. This is assumed to be the result of either \code{generateRuleSet} or \code{pruneRuleSet} function.
#' @param newdata Data frame containing the values at which predictions are required. The predictors referred to in the right side of formula(object) must be present by name in newdata. If missing, the fitted values are returned
#' @param trainingDataFrame Data that was used to train source rpart model
#' @param printLog Boolean value tells whether to print additinal information while predicting or not
#' @return vetor of predicted responses
#' @examples
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' ruleSet <- generateRuleSet(fit)
#' prediction <- predict(ruleSet)
#' print(prediction)
predict.ruleset <- function(object, newdata, printLog,
                          #type = c("vector", "prob", "class", "matrix"), na.action = na.pass,
                          ...)
{
  if (!inherits(object, "ruleset")) stop("Not a legitimate \"ruleset\" object")

#predict <- function(ruleSet, toBeClassifiedDataFrame, trainingDataFrame, printLog)
#{
  ##apply(rpartNurseryTrainingDataFrame[1:5,],MARGIN = 1,  function(x) {x[8]} )
  #apply(dataToBeClassified, 1, function(x) {lapply} )
  
  ruleList <- object$ruleList
  trainingDataFrame <- object$trainingDataFrame
  
  toBeClassifiedDataFrame <- newdata
  
  # find out how many samples from training set is covered by each rule
  rulesCoveredSamplesCount <- sapply(ruleList, function(x)
    {
      return (sum(.check(x, trainingDataFrame)))
    })
  
  log(paste("rulesCoveredSamplesCount:", paste(rulesCoveredSamplesCount, collapse = " ")), printLog = printLog)
  
  # is-sample-covered-by-rule matrix
  samplesCoveredByRulesMatrixBool <- apply(toBeClassifiedDataFrame, 1, function(sample)
    {
      return (sapply(ruleList, function(rule) 
        {
          #single sample, single rule
          return (.check(rule,sample))
        }) )
    })
  
  log("is-sample-covered-by-rule matrix:", printLog = printLog)
  log(samplesCoveredByRulesMatrixBool, printLog = printLog)
  
  # pick the rule with most samples from training set
  samplesCount <- ncol(samplesCoveredByRulesMatrixBool)
  samplesRulesNumbers <- vector(mode = "numeric", length = samplesCount)
  for(i in 1:samplesCount)
  {
    log(paste("--> sample ", i), printLog = printLog)
    ithSampleRuleCoverMask <- samplesCoveredByRulesMatrixBool[,i]
    log("     is covered by:", printLog = printLog)
    log(ithSampleRuleCoverMask, printLog = printLog)
    #which.max(rulesCoveredSamplesCount[rulesCover[,1]])
    ruleThatWinsIndex <- which(ithSampleRuleCoverMask)[which.max(rulesCoveredSamplesCount[ithSampleRuleCoverMask])]
    log(paste("     but the winner is rule no:", ruleThatWinsIndex), printLog = printLog)
    #ruleThatWinsIndex == 0 if none of existing rules cover this sample
    samplesRulesNumbers[[i]] <- ruleThatWinsIndex
  }
  
  log("     so, eventually, chosen rules are:", printLog = printLog)
  log(samplesRulesNumbers, printLog = printLog)
  
  samplesRulesNumberIter <- 0
  error <- 0
  overallNumberOfClassificationsDone <- 0
  nonClassfiedSamplesCount <- 0
  result <- apply(toBeClassifiedDataFrame, 1, function(sample)
  {
    # sample is a single row from data frame now. its class is "numeric" !!
    samplesRulesNumberIter <<- samplesRulesNumberIter + 1

    # logging ...    
    log(paste("--> sample:", samplesRulesNumberIter), printLog = printLog)
    tempSingleSampleDf <- rbind(sample)
    colnames(tempSingleSampleDf) <- colnames(toBeClassifiedDataFrame)
    log(tempSingleSampleDf, printLog = printLog)
    #
    
    winningRuleIndex <- samplesRulesNumbers[[samplesRulesNumberIter]]# this sample winning rule index
    
    log(paste("     winning rule index:", winningRuleIndex), printLog = printLog)
    log(paste("     winning rule:", ruleList[[winningRuleIndex]]), printLog = printLog)
    
    if(winningRuleIndex < 1L)
    { # this sample wasnt classified (no rule covered the sample)
      nonClassfiedSamplesCount <<- nonClassfiedSamplesCount + 1
    }
    
    overallNumberOfClassificationsDone <<- overallNumberOfClassificationsDone + 1
    
    variableWeWereClassifingOn <- ruleList[[winningRuleIndex]]$consequent$consequentKey
    classifiedAs <- ruleList[[winningRuleIndex]]$consequent$consequentValue
    shouldBeClassifiedAs <- sample[[variableWeWereClassifingOn]]
    
    log(paste("     should be classified as ", shouldBeClassifiedAs, ", classified as ", classifiedAs), printLog = printLog)
    
    if( is.character(classifiedAs))
    { # "categorical"
      #print("class")
      if(!(shouldBeClassifiedAs == classifiedAs))
      {
        log("     missed!", printLog = printLog)
        error <<- error + 1
      }
    }
    else if(is.numeric(classifiedAs))
    { # "continuous"
      #print((classifiedAs - shouldBeClassifiedAs)^2)
      error <<- error + (classifiedAs - shouldBeClassifiedAs)^2
      #print("anova")
    }
    else
    { # just in case
      stop("Error. Unknown rule classification variable type.")
    }
    return(classifiedAs)
  })
  
  log(error, printLog = printLog)
  
  # compute classification error
  if(overallNumberOfClassificationsDone > 0L)
  {
    error <- error/overallNumberOfClassificationsDone    
  }
  
  log("----------------------------------------------------------------", printLog = printLog)
  log(paste("Classifications done count: ", overallNumberOfClassificationsDone), printLog = printLog)
  log(paste("Misclassified factor / mse: ", error), printLog = printLog)
  log(paste("Samples not classified count: ", nonClassfiedSamplesCount), printLog = printLog)
  

  return (error)
  
  
}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                                 LOGGING                                     ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

log <- function(x, printLog = FALSE)
{
  if(printLog)
  {
    print(x)    
  }
}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                                   TESTS                                     ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

test <- function()
{
  # prepare data
  categoricalTestDataSingle <- .categoricalTestDataSingle()
  categoricalTestDataMultiple <- .categoricalTestDataMultiple()
  continuousTestDataSingle <- .continuousTestDataSingle()
  continuousTestDataMultiple <- .continuousTestDataMultiple()
  
  # tests
  .continuousSelectorTest(continuousTestDataSingle, continuousTestDataMultiple)
  .categoricalSelectorTest(categoricalTestDataSingle, categoricalTestDataMultiple)
  .categoricalComplexTest(categoricalTestDataSingle, categoricalTestDataMultiple)
  
  print("TESTS PASSED SUCCESSFULLY!")
}

.continuousSelectorTest <- function(df.single, df.multiple)
{
  ####################################################################################################################################################################################
  ####################################################################################################################################################################################
  #############################################                                                                           ############################################################
  #############################################   CONTINUOUS SELECTOR CHECK TEST                                          ############################################################
  #############################################                                                                           ############################################################
  #############################################     [single sample; categorical selector with single possible value]      ############################################################
  #############################################     [multiple sample; categorical selector with multiple possible values] ############################################################
  #############################################                                                                           ############################################################
  ####################################################################################################################################################################################
  ####################################################################################################################################################################################
  test.name <- "CONTINUOUS SELECTOR CHECK TEST"
  test.name.subtitle.1 <- "[single sample; categorical selector with single possible value]"
  test.name.subtitle.2 <- "[multiple sample; categorical selector with multiple possible values]"
  
  ############################################################################
  ## SINGLE SAMPLE
  ############################################################################
  continuousSelector.sample.single <- ContinuousSelector(decisionVariable = colnames(df.single)[1], cutpoint = df.single[1,2]-1, relation = "<")

  # launch tested function
  testResult <- .check(selector = continuousSelector.sample.single, sample = df.single)
  expectedTestResult <- c(TRUE)
  
  # check test results
  if(!(all(testResult == expectedTestResult) ))
  {
    stop(paste(test.name, " ", test.name.subtitle.1, " failed!"))  
  }
  
  ############################################################################
  ## MULTIPLE SAMPLE
  ############################################################################
  continuousSelector.sample.multiple <- ContinuousSelector(decisionVariable = colnames(df.multiple)[3L], cutpoint = df.multiple[2,3]-1, relation = ">=")

  # launch tested function
  testResult <- .check(selector = continuousSelector.sample.multiple, sample = df.multiple)
  expectedTestResult <- c(FALSE, TRUE)
  
  # check test results
  if(!(all(testResult == expectedTestResult) ))
  {
    stop(paste(test.name, " ", test.name.subtitle.2, " failed!"))  
  }
  
}

.categoricalSelectorTest <-function(df.single, df.multiple)
{
####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                           ############################################################
#############################################   CATEGORICAL SELECTOR CHECK TEST                                         ############################################################
#############################################                                                                           ############################################################
#############################################     [single sample; categorical selector with single possible value]      ############################################################
#############################################     [single sample; categorical selector with multiple possible values]   ############################################################
#############################################                                                                           ############################################################
#############################################     [multiple sample; categorical selector with single possible value]    ############################################################
#############################################     [multiple sample; categorical selector with multiple possible values] ############################################################
#############################################                                                                           ############################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
test.name <- "CATEGORICAL SELECTOR CHECK TEST"
test.name.subtitle.1.1 <- "[single sample; categorical selector with single possible value][no fake data]"
test.name.subtitle.1.2 <- "[single sample; categorical selector with single possible value][fake data]"
test.name.subtitle.2 <- "[single sample; categorical selector with multiple possible values]"
test.name.subtitle.3 <- "[multiple sample; categorical selector with single possible value]"
test.name.subtitle.4 <- "[multiple sample; categorical selector with multiple possible values]"

############################################################################
## SINGLE SAMPLE
############################################################################

fakeData <- "fakeData"

categoricalSelector.single.sample.single.1 <- CategoricalSelector(decisionVariable = colnames(df.single)[3L], possibleValues = c(as.character(df.single[1L, 3L])))
categoricalSelector.single.sample.single.2 <- CategoricalSelector(decisionVariable = colnames(df.single)[2L], possibleValues = c(fakeData))
categoricalSelector.multiple.sample.single <- CategoricalSelector(decisionVariable = colnames(df.single)[2L], possibleValues = c(as.character(df.single[1L, 2L]), fakeData))

# launch tested function
testResult <- .check(selector = categoricalSelector.single.sample.single.1, sample = df.single)
expectedTestResult <- TRUE

# check test results
if(!(all(testResult == expectedTestResult)))
{
  stop(paste(test.name, " ", test.name.subtitle.1.1, " failed!"))  
}

# launch tested function
testResult <- .check(selector = categoricalSelector.single.sample.single.2, sample = df.single)
expectedTestResult <- FALSE
# check test results
if(!(all(testResult == expectedTestResult)))
{
  stop(paste(test.name, " ", test.name.subtitle.1.2, " failed!"))  
}

# launch tested function
testResult <- .check(selector = categoricalSelector.multiple.sample.single, sample = df.single)
expectedTestResult <- TRUE

# check test results
if(!(all(testResult == expectedTestResult)))
{
  stop(paste(test.name, " ", test.name.subtitle.2, " failed!"))  
}

############################################################################
## MULTPILE SAMPLE
############################################################################

categoricalSelector.single.sample.multiple <- CategoricalSelector(decisionVariable = colnames(df.multiple)[4L], possibleValues = c(as.character(df.multiple[6L, 4L])))
categoricalSelector.multiple.sample.multiple <- CategoricalSelector(decisionVariable = colnames(df.multiple)[1L], possibleValues = c(as.character(df.multiple[1L, 1L]), as.character(df.multiple[3L, 1L])))

# launch tested function
testResult <- .check(selector = categoricalSelector.single.sample.multiple, sample = df.multiple)
expectedTestResult <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)

# check test results
if(!(all(testResult == expectedTestResult)))
{
  stop(paste(test.name, " ", test.name.subtitle.3, " failed!"))  
}

# launch tested function
testResult <- .check(selector = categoricalSelector.multiple.sample.multiple, sample = df.multiple)
expectedTestResult <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)

# check test results
if(!(all(testResult == expectedTestResult) ))
{
  stop(paste(test.name, " ", test.name.subtitle.4, " failed!"))  
}

####################################################################################################################################################################################
####################################################################################################################################################################################
}

.categoricalComplexTest <- function(df.single, df.multiple)
{
  ####################################################################################################################################################################################
  ####################################################################################################################################################################################
  #############################################                                                                           ############################################################
  #############################################   CATEGORICAL COMPLEX CHECK TEST                                          ############################################################
  #############################################                                                                           ############################################################
  #############################################     [single sample; single-selector complex]                              ############################################################
  #############################################     [single sample; multiple-selector complex]                            ############################################################
  #############################################                                                                           ############################################################
  #############################################     [multiple sample; single-selector complex]                            ############################################################
  #############################################     [multiple sample; multiple-selector complex]                          ############################################################
  #############################################                                                                           ############################################################
  ####################################################################################################################################################################################
  ####################################################################################################################################################################################
  test.name <- "CATEGORICAL COMPLEX CHECK TEST"
  test.name.subtitle.1 <- "[single sample; single-selector complex]"
  test.name.subtitle.2 <- "[single sample; multiple-selector complex]"
  test.name.subtitle.3 <- "[multiple sample; single-selector complex]"
  test.name.subtitle.4 <- "[multiple sample; multiple-selector complex]"
  
  ############################################################################
  ## SINGLE SAMPLE
  ############################################################################
  
  fakeData <- "fakeData"
  
  categoricalSelector.sample.single.1 <- CategoricalSelector(decisionVariable = colnames(df.single)[4L], possibleValues = c(as.character(df.single[1L, 4L])))
  categoricalSelector.sample.single.2 <- CategoricalSelector(decisionVariable = colnames(df.single)[4L], possibleValues = c(as.character(fakeData)))
  
  complex.single <- Complex()
  complex.single[[1]] <- categoricalSelector.sample.single.1
  
  complex.multiple <- Complex()
  complex.multiple[[1]] <- categoricalSelector.sample.single.1
  complex.multiple[[2]] <- categoricalSelector.sample.single.2
  
  complex.multiple.2 <- Complex()
  complex.multiple.2[[1]] <- CategoricalSelector(decisionVariable = colnames(df.multiple)[3L], possibleValues = c(as.character(df.multiple[2L, 3L]), as.character(df.multiple[5L, 3L])))
  complex.multiple.2[[2]] <- CategoricalSelector(decisionVariable = colnames(df.multiple)[4L], possibleValues = c(as.character(df.multiple[2L, 4L]), as.character(df.multiple[6L, 4L])))

  # launch tested function
  testResult <- .check(complex.single, sample = df.single)
  expectedTestResult <- TRUE
  
  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.1, " failed!"))  
  }
  
  # launch tested function
  testResult <- .check(complex.multiple, sample = df.single)
  expectedTestResult <- FALSE
  
  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.2, " failed!"))  
  }
  
  ############################################################################
  ## MULTIPLE SAMPLE
  ############################################################################
  
  # launch tested function
  testResult <- .check(complex.single, sample = df.multiple)
  expectedTestResult <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  
  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.3, " failed!"))  
  }
  
  # launch tested function
  testResult <- .check(complex.multiple, sample = df.multiple)
  expectedTestResult <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  
  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.4, ".1 failed!"))  
  }
  
  # launch tested function
  testResult <- .check(complex.multiple.2, sample = df.multiple)
  expectedTestResult <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  
  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.4, ".2 failed!"))  
  }
  
}

.categoricalTestDataSingle <- function()
{
  df.single.column.names <- c("c1", "c2", "c3", "c4")
  df.single.column.1 <- c("c1.v1")
  df.single.column.2 <- c("c2.v1")
  df.single.column.3 <- c("c3.v1")
  df.single.column.4 <- c("c4.v1")
  df.single.row.name <- c("r1")
  df.single <- data.frame(
    df.single.column.1,
    df.single.column.2,
    df.single.column.3,
    df.single.column.4,
    row.names = df.single.row.name
  )
  colnames(df.single) <- df.single.column.names
  return(df.single)
}
.categoricalTestDataMultiple <- function()
{
  df.multiple.column.names <- c("c1", "c2", "c3", "c4")
  df.multiple.column.1 <- c("c1.v1", "c1.v2", "c1.v3", "c1.v4", "c1.v5", "c1.v6")
  df.multiple.column.2 <- c("c2.v1", "c2.v2", "c2.v3", "c2.v4", "c2.v5", "c2.v6")
  df.multiple.column.3 <- c("c3.v1", "c3.v2", "c3.v3", "c3.v4", "c3.v5", "c3.v6")
  df.multiple.column.4 <- c("c4.v1", "c4.v2", "c4.v3", "c4.v4", "c4.v5", "c4.v6")
  df.multiple.row.names <- c("r1", "r2", "r3", "r4", "r5", "r6")
  df.multiple <- data.frame(
    df.multiple.column.1,
    df.multiple.column.2,
    df.multiple.column.3,
    df.multiple.column.4,
    row.names = df.multiple.row.names
  )
  colnames(df.multiple) <- df.multiple.column.names
  return(df.multiple)
}
.continuousTestDataSingle <- function()
{
  df.single.column.names <- c("c1", "c2", "c3", "c4")
  df.single.column.1 <- c(1.1)
  df.single.column.2 <- c(2.2)
  df.single.column.3 <- c(3)
  df.single.column.4 <- c(4.4)
  df.single.row.name <- c("r1")
  df.single <- data.frame(
    df.single.column.1,
    df.single.column.2,
    df.single.column.3,
    df.single.column.4,
    row.names = df.single.row.name
  )
  colnames(df.single) <- df.single.column.names
  return(df.single)
}
.continuousTestDataMultiple <- function()
{
  df.multiple.column.names <- c("c1", "c2", "c3", "c4")
  df.multiple.column.1 <- c(1.1, 11.1)
  df.multiple.column.2 <- c(2.2, 12.2)
  df.multiple.column.3 <- c(3, 33.3)
  df.multiple.column.4 <- c(4.4, 14.4)
  df.multiple.row.names <- c("r1", "r2")
  df.multiple <- data.frame(
    df.multiple.column.1,
    df.multiple.column.2,
    df.multiple.column.3,
    df.multiple.column.4,
    row.names = df.multiple.row.names
  )
  colnames(df.multiple) <- df.multiple.column.names
  return(df.multiple)
}
