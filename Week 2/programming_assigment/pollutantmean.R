pollutantmean <- function(directory, pollutant, id = 1:332){
  df <- data.frame()
  all_files <- dir(directory)
  for(i in id){
    csv <- read.csv(file.path(directory, all_files[i]))
    df <- rbind(df, csv[pollutant])
  }
  
  mean(df[, pollutant], na.rm = T)
}