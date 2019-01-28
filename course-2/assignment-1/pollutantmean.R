# A helper function to pad extra zeros, mainly for reading CSV files that are
# numbered with a consistent number of digits, such as 001.csv
padZeroLeft <- function(inputStr, numChars) {
  while (nchar(inputStr) < numChars)
    inputStr <- paste('0', inputStr, sep='')
  inputStr
}

# A function that calculates the mean of a given pollutant (either sulfate or
# nitrate) within a given list of monitor data tables.
pollutantmean <- function(directory, pollutant, ids = 1:332) {
  total <- 0
  count <- 0
  
  for (id in ids) {
    # read the tabular data
    filepath <- paste(directory, '/', padZeroLeft(id, 3), '.csv', sep='')
    data <- read.csv(filepath)
    
    # select valid data based on the pollutant
    selectedRawData <- data[, pollutant]
    validDataIndicies <- !is.na(selectedRawData)
    validData <- selectedRawData[validDataIndicies]
    
    # add up the total and count of the pollutant data
    total <- total + sum(validData)
    count <- count + length(validData)
  }
  
  total / count
}