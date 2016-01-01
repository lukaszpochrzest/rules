libDir <- "D:/TMP/RShit/"

#setwd( workingDir )
installe1071Bayes <- function()
{
	install.packages( "e1071", lib = libDir )
	library( e1071, lib.loc = libDir )
}

loadBayesLibs <-function()
{
  library( e1071, lib.loc = libDir )
}

#buildNaiveBayes <- function( classes, attributes )
#{
#	model <- naiveBayes(Class ~ ., data = HouseVotes84, laplace = 3)
#	return (model)
#}

#predictWithNaiveBayes <- function( model, data )
#{
#	prediction <- predict( model$finalModel, data )
#	return( prediction$class )
#}

#makeTestDataset <- function( csvDataset, numColumn )
#{
#	x = csvDataset[,-numColumn]
#	y = csvDataset[,numColumn]
#	return( list( classes = y, attributes = x ) )
#}

logAll <- function( value1, value2, printLog = FALSE )
{
  if(printLog)
  {
    for( i in 1 : length(value1))
    {
      print( value1[i])
      print( value2[i])
    }
  }
}

bayesError <- function( model, dataset )
{
  realClasses <- dataset[,ncol(dataset)]
  toClassify <- dataset[,1:( ncol(dataset) - 1 )]
  
  predictions <- predict( object = model, newdata = toClassify, type = "class" )
  
  #logAll( realClasses, predictions, TRUE )
  
  comparisionList <- cbind( predictions, realClasses )
  #print( comparisionList )
  
  error <- 0
  overallNumberOfClassificationsDone <- 0
  apply( comparisionList, 1, function(sample)
  {
    overallNumberOfClassificationsDone <<- overallNumberOfClassificationsDone + 1
    
    classifiedAs <- sample[1]
    shouldBeClassifiedAs <- sample[2]
    
    
    if(is.factor(classifiedAs) || is.character(classifiedAs))
    {# "categorical"
      
      #print( "class" )
      if(!(shouldBeClassifiedAs == classifiedAs))
      {
        error <<- error + 1
      }
    }
    else
    { # "continuous"
      
      print( "anova" )
      error <<- error + (classifiedAs - shouldBeClassifiedAs)^2
    }
    
  })
  
  # compute classification error
  if(overallNumberOfClassificationsDone > 0L)
  {
    error <- error/overallNumberOfClassificationsDone    
  }
  
  return (error)
}


