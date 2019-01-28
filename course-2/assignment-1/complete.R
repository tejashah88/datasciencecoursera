# A helper function to pad extra zeros, mainly for reading CSV files that are
# numbered with a consistent number of digits, such as 001.csv
padZeroLeft <- function(inputStr, numChars) {
  while (nchar(inputStr) < numChars)
    inputStr <- paste('0', inputStr, sep='')
  inputStr
}

# A function that outputs a data frame of complete cases for each monitor ID
complete <- function(directory, ids = 1:332) {
  nobs <- numeric()
  
  for (id in ids) {
    # read the tabular data
    filepath <- paste(directory, '/', padZeroLeft(id, 3), '.csv', sep='')
    data <- read.csv(filepath)
    
    # weed out the incomplete cases and sum them up
    completeCases <- complete.cases(data)
    nobs <- append(nobs, sum(completeCases))
  }
  
  # construct the data frame
  completeFrame <- data.frame(ids, nobs)
  colnames(completeFrame) <- c('id', 'nobs')
  
  completeFrame
}