setwd("/Users/David/Desktop/PhD/r")
projectDir = getwd()


codeDir = file.path(projectDir, 'code')
dataDir = file.path(projectDir, 'data')
outputDir = file.path(projectDir, 'output')


VERBOSE=TRUE

if (VERBOSE)
  print("Verbose SET")