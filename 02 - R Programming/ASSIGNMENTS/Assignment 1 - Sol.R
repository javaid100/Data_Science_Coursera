# ==================================================================================================
# ================================== Assignment 01 - Task 1 ========================================
# ==================================================================================================

pollutantmean <- function(directory, pollutant, id = 1:332) {
  setwd(file.path(getwd(), directory))           # setting the directory
  total = 0                             
  observations = 0                      
  
  # Looping through the directory's files specified in the 'id' argument
  for (i in id) {
    if (i <10) {
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    else if (i>=10 & i<100) {
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    else {
      data <- read.csv(paste(as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    data = na.omit(data)                        # getting rid of all the "NA" values
    observations = observations + nrow(data)    # cumulative addition of the complete observations

    if (pollutant == "sulfate") 
      {total = total + sum(data$sulfate)}
    else 
      {total = total + sum(data$nitrate)}
  }
  
  setwd("..")           # reset directory path
  return (total/observations)
}

p1 <- pollutantmean("specdata", "sulfate", 1:10)
p2 <- pollutantmean("specdata", "nitrate", 70:72)
p3 <- pollutantmean("specdata", "nitrate", 1:23)

sprintf("Mean of the pollutant across all of the selected monitor IDs: %f", p1)
sprintf("Mean of the pollutant across all of the selected monitor IDs: %f", p2)
sprintf("Mean of the pollutant across all of the selected monitor IDs: %f", p3)

# ==================================================================================================
# ================================== Assignment 01 - Task 2 ========================================
# ==================================================================================================

complete <- function(directory, id = 1:332) {
  setwd(file.path(getwd(), directory))           # setting the directory
  dataframe = NULL
  
  # Looping through the directory's files specified in the 'id' argument
  for (i in id) {
    if (i <10) {
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    else if (i>=10 & i<100) {
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    else {
      data <- read.csv(paste(as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    data = na.omit(data)
    data = as.matrix(data)                          # make it a matrix to easily fill each successive row 
    dataframe = rbind(dataframe, c(i,nrow(data)))   # each row contains the monitor ID, and its observed cases 
  }
  
  setwd("..")                           # reset directory path
  dataframe = data.frame(dataframe)     # from matrix to data frame
  
  names(dataframe) = c('id', 'nobs')    # set the column names of the data frame
  return (dataframe)
}

c1 <- complete("specdata", 1)
c2 <- complete("specdata", 1:10)
print(c1)
print(c2)

# ==================================================================================================
# ================================== Assignment 01 - Task 3 ========================================
# ==================================================================================================

corr <- function(directory, threshold = 0) {
  setwd(file.path(getwd(), directory))           # setting the directory
  correlationVector = NULL       
  
  # Looping through the directory's files specified in the 'id' argument
  for (i in 1:332) {
    if (i <10) {
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    else if (i>=10 & i<100) {
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    else {
      data <- read.csv(paste(as.character(i), ".csv", sep=""), 
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    data = na.omit(data)
    
    ## If the number of complete observed cases meets the requirement, find the correlation between the 
    ## pollutants and store the results in the correlation vector
    if (nrow(data) > threshold) {
      x = data[, c("sulfate")]
      y = data[, c("nitrate")]
      correlationVector = c(correlationVector, cor(x, y))
    }
  }
  
  setwd("..") 
  return (correlationVector)
}

cr <- corr("specdata", 150)
head(cr);        summary(cr)
cr <- corr("specdata", 400)
head(cr);        summary(cr)