import random


def makeDataset( fileName, resultName ):
    originFile = open( fileName, 'r' )
    
    trainingFile = open( resultName + ".training", 'w' )
    pruningFile = open( resultName + ".pruning", 'w' )
    testFile = open( resultName + ".test", 'w' )

    random.seed()

    header = originFile.readline()
    csvHeader = header.replace( ",", ";" )
	
    trainingFile.write( csvHeader )
    pruningFile.write( csvHeader )
    testFile.write( csvHeader )

    for line in originFile:
        randNum = random.randrange( 1, 100 )
        csvLine = line.replace( ",", ";" )
        if randNum < 33:
            trainingFile.write( csvLine )
        elif randNum < 66:
            pruningFile.write( csvLine )
        else:
            testFile.write( csvLine )
    

