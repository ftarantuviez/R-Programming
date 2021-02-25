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
  
  attach(df)
  var_to_use <- NULL
  if(outcome_n == "heart attack"){
    var_to_use <- Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  } else if(outcome_n == "heart failure"){
    var_to_use <- Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  } else {
    var_to_use <- Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  df <- df[order(as.numeric(var_to_use)),]
  browser()
  nth <- if(num == "best") 1 else num
  
  if(num == "worst") {
    temp_df <- sort(as.numeric(var_to_use))
    nth <- length(temp_df)
  }
  detach(df)
  
  df$Hospital.Name[nth]
  
}