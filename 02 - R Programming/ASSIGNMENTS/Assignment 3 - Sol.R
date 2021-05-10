library("data.table")
setwd(file.path(getwd())) 

# ================================================================================================
# ================ Part 1 - Plot the 30-day mortality rates for heart attack =====================
# ================================================================================================

outcome <- data.table::fread('outcome-of-care-measures.csv')
outcome[, (11) := lapply(.SD, as.numeric), .SDcols = (11)]
outcome[, lapply(.SD, hist, xlab= "Deaths", main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", 
                 col="lightblue"), .SDcols = (11)]


# ================================================================================================
# ====================== Part 2 - Finding the best hospital in a state ===========================
# ================================================================================================

best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  
  names(data)[1] <- "Name"
  names(data)[2] <- "State"
  names(data)[3] <- "Heart attack"
  names(data)[4] <- "Heart failure"
  names(data)[5] <- "Pneumonia"
  
  outcomes = c("Heart attack", "Heart failure", "Pneumonia")    # Vector of all of the possible outcomes
  states <- data[, 2]        # Get a vector of the states in 'data' at column 2
  states <- unique(states)  
  
  if (outcome %in% outcomes == FALSE)  {stop("invalid outcome")}
  if (state %in% states == FALSE)  {stop("invalid state")}
  
  data <- data[data$State==state & data[outcome]!='Not Available', ] # Get only the rows with our state value
  
  vals <- data[, outcome]     # Get the values of our specific outcome
  rowNum <- which.min(vals)   # Get the index of the minimum value 
  data[rowNum, ]$Name         # Return hospital name in that state with lowest 30-day death rate
}


# ================================================================================================
# ===================== Part 3 - Ranking hospitals by outcome in a state =========================
# ================================================================================================

rankhospital <- function(state, outcome, num) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  
  names(data)[1] <- "Name"
  names(data)[2] <- "State"
  names(data)[3] <- "Heart attack"
  names(data)[4] <- "Heart failure"
  names(data)[5] <- "Pneumonia"
  
  outcomes = c("Heart attack", "Heart failure", "Pneumonia")    # Vector of all of the possible outcomes
  states <- data[, 2]          # Get a vector of the states in 'data' at column 2
  states <- unique(states)
  
  if (outcome %in% outcomes == FALSE)                           {stop("invalid outcome")}
  if (state %in% states == FALSE)                               {stop("invalid state")}
  if( num != "best" && num != "worst" && numeric(num)%%1 != 0 ) {stop("invalid num")}
  
  data <- data[data$State==state & data[outcome]!='Not Available', ] # Get only the rows with our state value
  
  # Order the data by first name and then by outcome
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data["Name"], decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  vals <- data[, outcome]      # Get the values of our specific outcome
  
  if( num == "best" ) {rowNum <- which.min(vals)} 
  else if( num == "worst" ) {rowNum <- which.max(vals)} 
  else {rowNum <- num}
  
  data[rowNum, ]$Name    ## Return hospital name with the given rank
}


# ================================================================================================
# ======================== Part 4 - Ranking hospitals in all states ==============================
# ================================================================================================

rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  
  names(data)[1] <- "Name"
  names(data)[2] <- "State"
  names(data)[3] <- "Heart attack"
  names(data)[4] <- "Heart failure"
  names(data)[5] <- "Pneumonia"
  
  outcomes = c("Heart attack", "Heart failure", "Pneumonia")
  if( outcome %in% outcomes == FALSE )                   {stop("invalid outcome")}
  if( num != "best" && num != "worst" && num%%1 != 0 )   {stop("invalid num")}
  
  data <- data[data[outcome]!='Not Available', ]  # Grab only rows with data in our outcome
  
  # Order the data by first name and then by outcome
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$Name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  # For each state, find the hospital of the given rank
  states <- data[, 2]
  states <- unique(states)
  newdata <- data.frame("hospital"=character(), "state"=character())
  for (st in states) {
    df <- data[data$State==st, ]
    vals <- df[, outcome]
    
    if( num == "best" )        {rowNum <- which.min(vals)} 
    else if( num == "worst" )  {rowNum <- which.max(vals)} 
    else                       {rowNum <- num}
    
    sf <- df[rowNum, ]$Name
    newdata <- rbind(newdata, data.frame(hospital=sf, state=st))
  }
  
  # Return a data frame with the hospital names and the associated state names
  newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
  return (newdata)
}


# ================================================================================================
# ================================= Testing of Functions =========================================
# ================================================================================================

best("TX", "Heart attack")
best("TX", "Heart failure")
best("MD", "Heart attack")
best("MD", "Pneumonia")
best("BB", "Heart attack")
best("NY", "Hert attack")

rankhospital("TX", "Heart failure", 4)
rankhospital("MD", "Heart attack", "worst")
rankhospital("MN", "Heart attack", 5000)

head(rankall("Heart attack", 20), 10)
tail(rankall("Pneumonia", "worst"), 3)
tail(rankall("Heart failure"), 10)
