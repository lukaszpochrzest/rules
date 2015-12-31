
filePrefix <- "datasets/winequality"

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

rpartFormula <- as.formula(paste("as.factor(", className, ") ~ ."))
