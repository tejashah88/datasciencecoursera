# A helper function to pad extra zeros, mainly for reading CSV files that are
# numbered with a consistent number of digits, such as 001.csv
padZeroLeft <- function(inputStr, numChars) {
  while (nchar(inputStr) < numChars)
    inputStr <- paste('0', inputStr, sep='')
  inputStr
}

# import "complete.R" to get the complete function to access the number of
# complete cases for a given set of monitor IDs
source("complete.R")

# A function that reads all monitor ID records with a minimum number of complete
# cases, and calculates the "correlation" between sulfate and nitrate
corr <- function(directory, threshold = 0) {
  corList <- numeric()
  
  # extract all monitor records with a minimum number of complete cases
  numMonitors <-length(list.files("specdata/"))
  completeCaseFrame <- complete(directory, 1:numMonitors)
  desiredCases <- completeCaseFrame[completeCaseFrame$nobs >= threshold, ]
  desiredCaseIDs <- desiredCases$id
  
  # iterate through each record and calculate the "correlation" between the 2 pollutants
  for (caseID in desiredCaseIDs) {
    # read the tabular data and clean it
    filepath <- paste(directory, '/', padZeroLeft(caseID, 3), '.csv', sep='')
    data <- read.csv(filepath)
    cleanData = data[complete.cases(data), ]
    
    # skip instances where there are NO complete cases, in which cor() will fail
    if (nrow(cleanData) != 0) {
      corResult <- cor(cleanData$sulfate, cleanData$nitrate)
      corList <- append(corList, corResult)
    }
  }
  
  corList
}