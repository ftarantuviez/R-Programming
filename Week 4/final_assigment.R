best <- function(state_ch, outcome_n){
  df <- read.csv("data-assigment/outcome-of-care-measures.csv", colClasses = "character")
  df <- df[df["State"] == state_ch,]
  var_to_use <- NULL
  if(outcome_n == "heart attack"){
    var_to_use <- df["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
  } else if(outcome_n == "heart failure"){
    var_to_use <- df["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
  } else {
    var_to_use <- df["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]
  }
  
  var_to_use <- var_to_use[,1]
  var_to_use_bool <- var_to_use == "Not Available"
  var_to_use[var_to_use_bool] = NA
  var_to_use <- as.numeric(var_to_use)
  min_val = min(var_to_use, na.rm = T)
  ans <- df[var_to_use == min_val,]$Hospital.Name
  ans[!is.na(ans)]
}


rankhospital <- function(state, outcome_n, num = "best"){
  df <- read.csv("data-assigment/outcome-of-care-measures.csv", colClasses = "character")
  df <- split(df, df$State)[state]
  df <- do.call(rbind.data.frame, df)
  row.names(df) <- NULL
  
  var_to_use <- NULL
  if(outcome_n == "heart attack"){
    var_to_use <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if(outcome_n == "heart failure"){
    var_to_use <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else {
    var_to_use <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  #browser()
  df[var_to_use] <- as.numeric(df[var_to_use][[1]])
  df <- df[order(df[var_to_use]),]
  nth <- if(num == "best") df[var_to_use][1,] else df[var_to_use][num,]
  
  if(num == "worst") {
    temp_df <- df[!is.na(df[var_to_use]),][var_to_use]
    nth <- temp_df[length(temp_df[,]),]
  }
  df_splitted <- split(df, df[var_to_use],)
  df_splitted <- df_splitted[as.character(nth)][[1]]
  df_splitted <- df_splitted[order(df_splitted["Hospital.Name"]),]
  df_splitted$Hospital.Name
  
}


rankall <- function(outcome, num = "best"){
  ## Reading the outcome data
  data <- read.csv("data-assigment/outcome-of-care-measures.csv", colClasses = "character")
  ## Selecting the columns that are reqiured and naming them
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## Validating the outcome string
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcomes == FALSE) stop("invalid outcome")
  
  ## Validating the num value
  if(num != "best" && num != "worst" && num%%1 != 0) stop("invalid num")
  
  ## Grab only those rows whose data is reqiured.
  data <- data[data[outcome] != 'Not Available', ]
  
  ## Ordering the data in ascending order, first according to the names 
  ## column and then according to their ranks for the specific outcome column
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ## Helper functiont to process the num argument
  getHospByRank <- function(df, s, n) {
    df <- df[df$state==s, ]
    vals <- df[, outcome]
    if(n == "best"){
      rowNum <- which.min(vals)
    }else if(n == "worst" ){
      rowNum <- which.max(vals)
    }else{
      rowNum <- n
    }
    df[rowNum, ]$name
  }
  
  ## For each state, find the hospital of the given rank
  states <- data[, 2]
  states <- unique(states)
  newdata <- data.frame("hospital"=character(), "state"=character())
  for(st in states) {
    hosp <- getHospByRank(data, st, num)
    newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) 
  ## state name
  newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
  newdata
}