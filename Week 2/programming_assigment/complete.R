complete <- function(directory, id = 1:332) {
  
  ## Get a list of filenames
  filenames <- list.files(path=directory, pattern="*.csv")
  
  ## Initialize variables
  ids <-vector()
  counts = vector()
  
  ## Loop over the passed id's
  for(i in id) {
    
    ## Pad the i to create a filename
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    ## Load the data
    data <- read.csv(filepath)
    
    ## Store the id
    ids <- c(ids, i)
    
    ## Calculate and store the count of complete cases
    completeCases <- data[complete.cases(data),]
    counts <- c(counts, nrow(completeCases))
  }
  
  ## Return the data frame
  data.frame(id=ids, nobs=counts)
}