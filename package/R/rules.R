

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
#   rpartWineTrainingDataFrame <- read.csv(file = "/home/lukasz/programming/mow/repo/src/datasets/winequality.training", header = TRUE, sep = ";")
#   rpartWinePruningDataFrame <- read.csv(file = "/home/lukasz/programming/mow/repo/src/datasets/winequality.pruning", header = TRUE, sep = ";")
#   rpartWineTestDataFrame <- read.csv(file = "/home/lukasz/programming/mow/repo/src/datasets/winequality.test", header = TRUE, sep = ";")
#   #rpartWineDataInputDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/winequality/winequality-red.csv", header = TRUE, sep = ";")
#
#   rpartWineTrainingSetDataTreeObject <- rpart(cp = 0, method = "anova", data =  rpartWineTrainingDataFrame, formula = quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)
#
#   plot(rpartWineTrainingSetDataTreeObject, uniform = TRUE)
#   text(rpartWineTrainingSetDataTreeObject, all = FALSE, use.n = TRUE, cex = 0.75)
#
#   # RULE SET GENERATION
#   rpartWineTrainingDataruleSet <- generateRuleSet(object = rpartWineTrainingSetDataTreeObject, trainingDataFrame = rpartWineTrainingDataFrame)
#
#   # RULE SET PRUNING
#   rpartWineDataruleSetPruned <- prune(ruleSet = rpartWineTrainingDataruleSet, pruningDataFrame = rpartWinePruningDataFrame, printLog = TRUE)
#
#   # CLASSIFICATION AND ERROR COMPUTATION #need some new "real data" set, pruning data set slice used temporarily instead
#   predict <- predict(object = rpartWineTrainingDataruleSet, newdata = rpartWineTestDataFrame)
#
#   # error withou pruning = 0.515719064796118 # sprawdzone z rpart
#
#
#               ################################################################
#               ######################### discrete data ########################
#               ################################################################
#
#   rpartNurseryTrainingDataFrame <- read.csv(file = "/home/lukasz/programming/mow/repo/src/datasets/nursery.training", header = TRUE, sep = ";")
#   rpartNurseryPruningDataFrame <- read.csv(file = "/home/lukasz/programming/mow/repo/src/datasets/nursery.pruning", header = TRUE, sep = ";")
#   rpartNurseryTestDataFrame <- read.csv(file = "/home/lukasz/programming/mow/repo/src/datasets/nursery.test", header = TRUE, sep = ";")
#   #rpartNurseryInputDataFrame <- read.csv(file = "/home/lukasz/programming/mow/data/nursery/nursery_data.csv", header = TRUE, sep = ",")
#
#   #cp = 0,
#   rpartNurseryTrainingSetDataTreeObject <- rpart(method = "class", data = rpartNurseryTrainingDataFrame, formula = class ~ parents + has_nurs + form + children + housing + finance + social + health)
#
#   plot(rpartNurseryTrainingSetDataTreeObject, uniform = TRUE)
#   text(rpartNurseryTrainingSetDataTreeObject, all = FALSE, use.n = TRUE, cex = 0.75)
#
#   # RULE SET GENERATION
#   rpartNurseryTrainingDataruleSet <- generateRuleSet(object = rpartNurseryTrainingSetDataTreeObject, trainingDataFrame = rpartNurseryTrainingDataFrame)
#
#   # RULE SET PRUNINGd
#   rpartNurseryDataruleSetPruned <- prune(ruleSet = rpartNurseryTrainingDataruleSet, pruningDataFrame = rpartNurseryPruningDataFrame, printLog = TRUE)
#
#   # CLASSIFICATION AND ERROR COMPUTATION #need some new "real data" set, pruning data set slice used temporarily instead
#   predict <- predict(object = rpartNurseryTrainingDataruleSet, newdata = rpartNurseryTestDataFrame)
#
#   # error without pruning = 0.121157729414404 # sprawdzone z rpart
#
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
#############################################     categoricalselector, continuousselector, cmplex , rule and ruleSet     ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

#' Selector used with discrete data
#'
#' @return \item{decisionVariable}{decision variable name} \item{possibleValues}{list of possible values of a decisionvarioable}
#' @usage categoricalselector("result", c("good", "average"))
categoricalselector <- function(decisionVariable, possibleValues)
{
  me <- list(type = "categorical", decisionVariable = decisionVariable, possibleValues = possibleValues)
  class(me) <- append(class(me),"categoricalselector")
  return(me)
}

#' Selector used with continuous data
#'
#' @return \item{decisionVariable}{decision variable name} \item{cutpoint}{value which causes division} \item{relation}{either >= or <}
#' @usage continuousselector("score", 5.21, ">=")
continuousselector <- function(decisionVariable, cutpoint, relation)
{
  me <- list(type = "continuous", decisionVariable = decisionVariable, cutpoint = cutpoint, relation = relation)
  class(me) <- append(class(me),"continuousselector")
  return(me)
}

#' List of selectors
cmplex <- function()
{
  me <- list()
  class(me) <- append(class(me), "cmplex")
  return(me)
}

#' rule implication consequent of a key = value format
#'
#' @return \item{consequentKey}{key} \item{consequentValue}{value}
#' @usage consequent("points", 74)
consequent <- function(consequentKey, consequentValue)
{
  me <- list(consequentKey = consequentKey, consequentValue = consequentValue)
  class(me) <- append(class(me), "consequent")
  return(me)
}

#' rule, which is a implication, with \code{cmplex} as antecedent and \code{consequent} as consequent
#'
#' @return \item{complex}{rule complex} \item{consequent}{rule consequent}
rule <- function(complex, consequent)
{
  me <- list(complex = complex, consequent = consequent)
  class(me) <- append(class(me), "rule")
  return(me)
}

#' ruleset, which is implemented as a tuple of a list of \code{rule} rules and training data, which was used to build rpart model, that was used to generate rulelist
#'
#' @return \item{rulelist}{list of rules} \item{trainingDataFrame}{training data, which was used to build rpart model, that was used to generate rulelist}
ruleset <- function(ruleList, trainingDataFrame) {
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
  UseMethod(".check",selector)
}

.check.categoricalselector <- function(selector, sample)
{
  return(sapply(sample[[selector$decisionVariable]], function (x) {any(x == selector$possibleValues)}))
}

.check.continuousselector <- function(selector, sample)
{
  if(selector$relation == ">=")
  {
    return (sapply(as.numeric(sample[[selector$decisionVariable]]), ">=", selector$cutpoint))
  }
  else if(selector$relation == "<")
  {
    return (sapply(as.numeric(sample[[selector$decisionVariable]]), "<", selector$cutpoint))
  }
  else#just to make sure
  {
    stop(paste("Error. Unknown Continous selector relation ", selector$relation))
  }
}
.check.cmplex <- function(complex, sample)
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

.check.rule <- function(rule, sample)
{
  return (.check(rule$complex,sample))
}

as.character.categoricalselector <- function(x, ...)
{
  return(paste(x$decisionVariable, "=", paste(x$possibleValues, collapse = "||" )))
}

as.character.continuousselector <- function(x, ...)
{
  return(paste(x$decisionVariable, x$relation, x$cutpoint))
}

as.character.cmplex <- function(x, ...)
{
  return(paste(lapply(x, as.character),collapse = " AND "))
}

as.character.consequent <- function(x, ...)
{
  return(paste(x$consequentKey, " CLASSIFIED AS  ", x$consequentValue))
}

as.character.rule <- function(x, ...)
{
  return(paste(as.character(x$complex), " => ", as.character(x$consequent)))
}

as.character.ruleset <- function(x, ...)
{
  return(x$ruleList)
}

print.categoricalselector <- function(x, ...)
{
  print(as.character(x))
}

print.continuousselector <- function(x, ...)
{
  print(as.character(x))
}

print.cmplex <- function(x, ...)
{
  print(as.character(x))
}

print.rule <- function(x, ...)
{
  print(as.character(x))
}

#' This function prints an \code{ruleset} object. It is a method for the generic function print of class "\code{ruleset}".
#'
#' @param x \code{ruleset} object.This is assumed to be the result of some function that produces an object with the same named components as that returned by the \code{generateRuleSet} function.
print.ruleset <- function(x, ...)
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
#' @param object A \code{rpart} object.
#' @param trainingDataFrame A data frame that was used to build \code{object}. It is required for creating ruleset object. ruleset objects need to contain infromation about this data, because ruleset pruning operation needs it to update rules' consequent values whenever a rule is pruned.
#' @return Generated set of rules
#' @examples
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' ruleSet <- generateRuleSet(fit, trainingDataFrame = kyphosis)
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

    sapply(1:leaves_size, function(i) {
      if(parents[i]>0)
      {
        paths[[i]] <<- append(paths[[i]],parents[i])
      }
    })
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
    }
    else if(object$method == "class")
    {
      result <- ylevels[yval]
    }
    else
    {
      stop(paste("Error. Rpart method ", object$method, " not supported."))
    }

    if(path.length == 1)
    {
      next;
    }

    complex <- cmplex()
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

        selector <- categoricalselector(decisionVariable = decision_variable, possibleValues = decision_variable.values_to_child)
        complex[[path_node_iter-1]] <- selector
      }
      else
      {               # is decision variable continuous ?
        type <- "continuous"
        cutpoint <- splits_index_column_value
        relation <- ifelse(previous_node.is_right_child,ifelse(ncat<0L,">=","<"),ifelse(ncat<0L,"<",">="))

        selector <- continuousselector(decisionVariable = decision_variable, cutpoint = cutpoint, relation = relation)
        complex[[path_node_iter-1]] <- selector
      }

    }

    paths_conditions[[i]] <- rule(complex = complex, consequent = consequent(consequentKey = variableClassifyOn, consequentValue = result))
  }
  ruleSet <- ruleset(ruleList = paths_conditions, trainingDataFrame = trainingDataFrame)
  return(ruleSet)

}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                          RULE SET PRUNING FUNCTIONS                         ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

#' Prunes rule set
#'
#' @param ruleSet Set of rules to prune
#' @param pruningDataFrame Data used to prune rules
#' @param printLog Boolean value tells whether to print additinal information while pruning rules or not
#' @return Pruned rule set
#' @examples
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' ruleSet <- generateRuleSet(fit, kyphosis)
#' prunedruleSet <- prune(ruleSet = ruleSet, pruningDataFrame = kyphosis)
#' print(prunedruleSet)
prune.ruleset <- function(ruleSet, pruningDataFrame, printLog = FALSE)
{
  if (!inherits(ruleSet, "ruleset")) stop("Not a legitimate \"ruleset\" object")
  ruleList <- ruleSet$ruleList
  trainingDataFrame <- ruleSet$trainingDataFrame

  pruningDataFrame <- .strip(pruningDataFrame)
  trainingDataFrame <- .strip(trainingDataFrame)

  ruleListPruned <- lapply(ruleList, function(x)
  {
    logMsg("----------------------------------------------------------------", printLog)
    logMsg(paste("---------------- pruning rule: ", x), printLog)
    .prunerule(rule = x, pruningDataFrame = pruningDataFrame, trainingDataFrame = trainingDataFrame, printLog = printLog)
  })

  ruleSetPruned <- ruleset(ruleList = ruleListPruned, trainingDataFrame = trainingDataFrame)
  return (ruleSetPruned)
}

.prunerule <- function(rule, pruningDataFrame, trainingDataFrame, printLog = FALSE) # use trainingDataFrame to calculate new rule consequent
{
  complex <- rule$complex
  complexLength <- length(complex)
  if(complexLength < 2)
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
    logMsg("----------------------------------------------------------------", printLog)
    logMsg(paste(debugIterationsCount, ". trying to remove selector: ", as.character(prunedComplexTry[[i]])), printLog)
    ##

    prunedComplexTry[[i]] <- NULL

    variableClassifyOn <- rule$consequent$consequentKey
    classifiedAs <- rule$consequent$consequentValue

    prunedComplexTryError <- .computeErrorComplex(complex = prunedComplexTry, dataFrame = pruningDataFrame, variableClassifyOn = variableClassifyOn, classifiedAs = classifiedAs)

    if(printLog==TRUE) {cat(paste("\tnew error vs old error: ",prunedComplexTryError, " vs ", prunedComplexError, "\n"))}

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

      if(printLog) {cat(paste("\tpruned! old class: ", tempOldClassifiedAs ," changed to ", classifiedAs, "\n"))}
    }

    if(pruned == FALSE)
    {
      i<-i+1
      if(printLog) {cat("\tnot pruned!\n")}
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
    coveredSamples <- dataFrame[coveredSamplesBool, variableClassifyOn]
    coveredSamplesCount <- length(coveredSamples)
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

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                                 PREDICTION                                  ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

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
  
  ##strip data from rows with Na values
  #trainingDataFrame <- trainingDataFrame[complete.cases(trainingDataFrame),]
  ##strip data from rows with empty factor values
  #trainingDataFrame <- trainingDataFrame[rowSums(trainingDataFrame[,colnames(trainingDataFrame)]=='')==0,]
  trainingDataFrame <- .strip(trainingDataFrame)
  
  ##strip data from rows with Na values
  #toBeClassifiedDataFrame <- newdata[complete.cases(newdata),]
  ##strip data from rows with empty factor values
  #toBeClassifiedDataFrame <- toBeClassifiedDataFrame[rowSums(toBeClassifiedDataFrame[,colnames(toBeClassifiedDataFrame)]=='')==0,]
  toBeClassifiedDataFrame <- .strip(newdata)
  
  # find out how many samples from training set is covered by each rule
  rulesCoveredSamplesCount <- sapply(ruleList, function(x)
  {
    return (sum(.check(x, trainingDataFrame)))
  })
  
  logMsg(paste("rulesCoveredSamplesCount:", paste(rulesCoveredSamplesCount, collapse = " ")), printLog = printLog)
  
  # is-sample-covered-by-rule matrix
  samplesCoveredByRulesMatrixBool <- apply(toBeClassifiedDataFrame, 1, function(sample)
  {
    return (sapply(ruleList, function(rule)
    {
      #single sample, single rule
      return (.check(rule,sample))
    }) )
  })
  
  logMsg("is-sample-covered-by-rule matrix:", printLog = printLog)
  logMsg(samplesCoveredByRulesMatrixBool, printLog = printLog)
  
  # pick the rule with most samples from training set
  samplesCount <- ncol(samplesCoveredByRulesMatrixBool)
  samplesRulesNumbers <- vector(mode = "numeric", length = samplesCount)
  for(i in 1:samplesCount)
  {
    logMsg(paste("--> sample ", i), printLog = printLog)
    ithSampleRuleCoverMask <- samplesCoveredByRulesMatrixBool[,i]
    logMsg("     is covered by:", printLog = printLog)
    logMsg(ithSampleRuleCoverMask, printLog = printLog)
    #which.max(rulesCoveredSamplesCount[rulesCover[,1]])
    ruleThatWinsIndex <- which(ithSampleRuleCoverMask)[which.max(rulesCoveredSamplesCount[ithSampleRuleCoverMask])]
    logMsg(paste("     but the winner is rule no:", ruleThatWinsIndex), printLog = printLog)
    #ruleThatWinsIndex == 0 if none of existing rules cover this sample
    samplesRulesNumbers[[i]] <- ruleThatWinsIndex
  }
  
  logMsg("     so, eventually, chosen rules are:", printLog = printLog)
  logMsg(samplesRulesNumbers, printLog = printLog)
  
  samplesRulesNumberIter <- 0
  error <- 0
  overallNumberOfClassificationsDone <- 0
  nonClassfiedSamplesCount <- 0
  result <- apply(toBeClassifiedDataFrame, 1, function(sample)
  {
    # sample is a single row from data frame now. its class is "numeric" !!
    samplesRulesNumberIter <<- samplesRulesNumberIter + 1
    
    # logging ...
    logMsg(paste("--> sample:", samplesRulesNumberIter), printLog = printLog)
    tempSingleSampleDf <- rbind(sample)
    colnames(tempSingleSampleDf) <- colnames(toBeClassifiedDataFrame)
    logMsg(tempSingleSampleDf, printLog = printLog)
    #
    
    winningRuleIndex <- samplesRulesNumbers[[samplesRulesNumberIter]]# this sample winning rule index
    
    logMsg(paste("     winning rule index:", winningRuleIndex), printLog = printLog)
    logMsg(paste("     winning rule:", ruleList[[winningRuleIndex]]), printLog = printLog)
    
    if(winningRuleIndex < 1L)
    { # this sample wasnt classified (no rule covered the sample)
      nonClassfiedSamplesCount <<- nonClassfiedSamplesCount + 1
      return (NaN)
    }
    
    overallNumberOfClassificationsDone <<- overallNumberOfClassificationsDone + 1
    
    variableWeWereClassifingOn <- ruleList[[winningRuleIndex]]$consequent$consequentKey
    classifiedAs <- ruleList[[winningRuleIndex]]$consequent$consequentValue
    shouldBeClassifiedAs <- sample[[variableWeWereClassifingOn]]
    
    logMsg(paste("     should be classified as ", shouldBeClassifiedAs, ", classified as ", classifiedAs), printLog = printLog)
    
    if( is.character(classifiedAs))
    { # "categorical"
      #print("class")
      if(!(shouldBeClassifiedAs == classifiedAs) && shouldBeClassifiedAs != "")
      {
        logMsg("     missed!", printLog = printLog)
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
  
  logMsg(error, printLog = printLog)
  
  # compute classification error
  if(overallNumberOfClassificationsDone > 0L)
  {
    error <- error/overallNumberOfClassificationsDone
  }
  
  logMsg("----------------------------------------------------------------", printLog = printLog)
  logMsg(paste("Stripped samples count: ", nrow(newdata) - nrow(toBeClassifiedDataFrame) ), printLog = printLog)
  logMsg(paste("Classifications done count: ", overallNumberOfClassificationsDone), printLog = printLog)
  logMsg(paste("Misclassified factor / mse: ", error), printLog = printLog)
  logMsg(paste("Samples not classified count: ", nonClassfiedSamplesCount), printLog = printLog)
  
  return (list(predictions = result, error = error))
}

#' Returns a vector of predicted responses from a ruleset object
#'
#' @param object ruleset object used to predict. This is assumed to be the result of either \code{generateRuleSet} or \code{pruneruleSet} function.
#' @param newdata Data frame containing the values at which predictions are required. The predictors referred to in the right side of formula(object) must be present by name in newdata.
#' @return vector of predicted responses from a ruleset object
#' @examples
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' ruleSet <- generateRuleSet(fit, kyphosis)
#' print(ruleSet)
#' prediction <- predict(objec = ruleSet, newdata = kyphosis)
predict.ruleset <- function(object, newdata, printLog = FALSE)
{
  if (!inherits(object, "ruleset")) stop("Not a legitimate \"ruleset\" object")

  ruleList <- object$ruleList
  trainingDataFrame <- object$trainingDataFrame

  trainingDataFrame <- .strip(trainingDataFrame)
  toBeClassifiedDataFrame <- .strip(newdata)

  # find out how many samples from training set is covered by each rule
  rulesCoveredSamplesCount <- sapply(ruleList, function(x)
  {
    return (sum(.check(x, trainingDataFrame)))
  })

  samplesCoveredByrulesMatrixBool <- apply(toBeClassifiedDataFrame, 1, function(sample)
  {
    return (sapply(ruleList, function(rule)
    {
      #single sample, single rule
      return (.check(rule,sample))
    }) )
  })

  # pick the rule with most samples from training set
  samplesCount <- ncol(samplesCoveredByrulesMatrixBool)
  samplesrulesNumbers <- vector(mode = "numeric", length = samplesCount)
  for(i in 1:samplesCount)
  {
    ithSampleruleCoverMask <- samplesCoveredByrulesMatrixBool[,i]
    ruleThatWinsIndex <- which(ithSampleruleCoverMask)[which.max(rulesCoveredSamplesCount[ithSampleruleCoverMask])]
    samplesrulesNumbers[[i]] <- ruleThatWinsIndex
  }

  samplesrulesNumberIter <- 0
  result <- apply(toBeClassifiedDataFrame, 1, function(sample)
  {
    # sample is a single row from data frame now. its class is "numeric" !!
    samplesrulesNumberIter <<- samplesrulesNumberIter + 1

    tempSingleSampleDf <- rbind(sample)
    colnames(tempSingleSampleDf) <- colnames(toBeClassifiedDataFrame)

    winningruleIndex <- samplesrulesNumbers[[samplesrulesNumberIter]]# this sample winning rule index

    if(winningruleIndex < 1L)
    { # this sample wasnt classified (no rule covered the sample)
      return (NaN)
    }

    variableWeWereClassifingOn <- ruleList[[winningruleIndex]]$consequent$consequentKey
    classifiedAs <- ruleList[[winningruleIndex]]$consequent$consequentValue

    return(classifiedAs)
  })

  return (result)
}

.strip <- function(dataframe)
{
  #strip data from rows with Na values
  dataframe <- dataframe[complete.cases(dataframe),]
  #strip data from rows with empty factor values
  dataframe <- dataframe[rowSums(dataframe[,colnames(dataframe)]=='')==0,]
  return (dataframe)
}

####################################################################################################################################################################################
####################################################################################################################################################################################
#############################################                                                                             ##########################################################
#############################################                                 LOGGING                                     ##########################################################
#############################################                                                                             ##########################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

logMsg <- function(x, printLog = FALSE)
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

.test <- function()
{
  ##  TEST1

  # prepare data
  categoricalTestDataSingle <- .categoricalTestDataSingle()
  categoricalTestDataMultiple <- .categoricalTestDataMultiple()
  continuousTestDataSingle <- .continuousTestDataSingle()
  continuousTestDataMultiple <- .continuousTestDataMultiple()

  # tests
  .continuousSelectorTest(continuousTestDataSingle, continuousTestDataMultiple)
  .categoricalselectorTest(categoricalTestDataSingle, categoricalTestDataMultiple)
  .categoricalComplexTest(categoricalTestDataSingle, categoricalTestDataMultiple)

  ##  TEST2

  # prepare data
  training_data <- as.data.frame(matrix( c( "c1Z", "c2Z", "c3Z", "c4Z",
                                            "c1Z", "c2Y", "c3Z", "c4Z",
                                            "c1Z", "c2Y", "c3Z", "c4Z",
                                            "c1Z", "c2Y", "c3Y", "c4Y",
                                            "c1X", "c2Y", "c3Z", "c4X"), nrow=5, ncol=4, byrow = TRUE))

  names(training_data) <- c("c1", "c2", "c3", "c4")

  complex <- cmplex()
  complex[[1]] <- categoricalselector(decisionVariable = "c1", possibleValues = c("c1Z"))
  complex[[2]] <- categoricalselector(decisionVariable = "c2", possibleValues = c("c2Y"))
  ruleList <- list(rule(complex = complex, consequent = consequent(consequentKey = "c4", consequentValue = "c4Z")))
  ruleSet <- ruleset(trainingDataFrame = training_data, ruleList = ruleList)

  ruleSetPruned <- prune.ruleset(ruleSet = ruleSet, pruningDataFrame = training_data, printLog = TRUE)
  if(length(ruleSetPruned$ruleList)!=1){
    stop("test 2 failed")
  }
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
  continuousSelector.sample.single <- continuousselector(decisionVariable = colnames(df.single)[1], cutpoint = df.single[1,2]-1, relation = "<")

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
  continuousSelector.sample.multiple <- continuousselector(decisionVariable = colnames(df.multiple)[3L], cutpoint = df.multiple[2,3]-1, relation = ">=")

  # launch tested function
  testResult <- .check(selector = continuousSelector.sample.multiple, sample = df.multiple)
  expectedTestResult <- c(FALSE, TRUE)

  # check test results
  if(!(all(testResult == expectedTestResult) ))
  {
    stop(paste(test.name, " ", test.name.subtitle.2, " failed!"))
  }

}

.categoricalselectorTest <-function(df.single, df.multiple)
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

  categoricalselector.single.sample.single.1 <- categoricalselector(decisionVariable = colnames(df.single)[3L], possibleValues = c(as.character(df.single[1L, 3L])))
  categoricalselector.single.sample.single.2 <- categoricalselector(decisionVariable = colnames(df.single)[2L], possibleValues = c(fakeData))
  categoricalselector.multiple.sample.single <- categoricalselector(decisionVariable = colnames(df.single)[2L], possibleValues = c(as.character(df.single[1L, 2L]), fakeData))

  # launch tested function
  testResult <- .check(selector = categoricalselector.single.sample.single.1, sample = df.single)
  expectedTestResult <- TRUE

  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.1.1, " failed!"))
  }

  # launch tested function
  testResult <- .check(selector = categoricalselector.single.sample.single.2, sample = df.single)
  expectedTestResult <- FALSE
  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.1.2, " failed!"))
  }

  # launch tested function
  testResult <- .check(selector = categoricalselector.multiple.sample.single, sample = df.single)
  expectedTestResult <- TRUE

  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.2, " failed!"))
  }

  ############################################################################
  ## MULTPILE SAMPLE
  ############################################################################

  categoricalselector.single.sample.multiple <- categoricalselector(decisionVariable = colnames(df.multiple)[4L], possibleValues = c(as.character(df.multiple[6L, 4L])))
  categoricalselector.multiple.sample.multiple <- categoricalselector(decisionVariable = colnames(df.multiple)[1L], possibleValues = c(as.character(df.multiple[1L, 1L]), as.character(df.multiple[3L, 1L])))

  # launch tested function
  testResult <- .check(selector = categoricalselector.single.sample.multiple, sample = df.multiple)
  expectedTestResult <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)

  # check test results
  if(!(all(testResult == expectedTestResult)))
  {
    stop(paste(test.name, " ", test.name.subtitle.3, " failed!"))
  }

  # launch tested function
  testResult <- .check(selector = categoricalselector.multiple.sample.multiple, sample = df.multiple)
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

  categoricalselector.sample.single.1 <- categoricalselector(decisionVariable = colnames(df.single)[4L], possibleValues = c(as.character(df.single[1L, 4L])))
  categoricalselector.sample.single.2 <- categoricalselector(decisionVariable = colnames(df.single)[4L], possibleValues = c(as.character(fakeData)))

  complex.single <- cmplex()
  complex.single[[1]] <- categoricalselector.sample.single.1

  complex.multiple <- cmplex()
  complex.multiple[[1]] <- categoricalselector.sample.single.1
  complex.multiple[[2]] <- categoricalselector.sample.single.2

  complex.multiple.2 <- cmplex()
  complex.multiple.2[[1]] <- categoricalselector(decisionVariable = colnames(df.multiple)[3L], possibleValues = c(as.character(df.multiple[2L, 3L]), as.character(df.multiple[5L, 3L])))
  complex.multiple.2[[2]] <- categoricalselector(decisionVariable = colnames(df.multiple)[4L], possibleValues = c(as.character(df.multiple[2L, 4L]), as.character(df.multiple[6L, 4L])))

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
