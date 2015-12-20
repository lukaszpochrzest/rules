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

makeTestDataset <- function( csvDataset, numColumn )
{
	x = csvDataset[,-numColumn]
	y = csvDataset[,numColumn]
	return( list( classes = y, attributes = x ) )
}
