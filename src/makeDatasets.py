import random


def makeDataset( fileName, resultName ):
    originFile = open( fileName, 'r' )
    
    trainingFile = open( resultName + ".training", 'w' )
    pruningFile = open( resultName + ".pruning", 'w' )
    testFile = open( resultName + ".test", 'w' )

    random.seed()

    header = originFile.readline()
    trainingFile.write( header )
    pruningFile.write( header )
    testFile.write( header )

    for line in originFile:
        randNum = random.randrange( 1, 100 )
        if randNum < 33:
            trainingFile.write( line )
        elif randNum < 66:
            pruningFile.write( line )
        else:
            testFile.write( line )
    

