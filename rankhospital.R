# 3. Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.
# > head(texas)
# Hospital.Name Rate Rank
# 3935 FORT DUNCAN MEDICAL CENTER 8.1 1
# 4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
# 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
# 3954 DETAR HOSPITAL NAVARRO 8.7 4
# 4010 METHODIST HOSPITAL,THE 8.8 5
# 3962 MISSION REGIONAL MEDICAL CENTER 8.8 6
# Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
# (8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
# scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
# manner (i.e. where one vector is used to break ties in another vector).
# The function should use the following template.
# rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
# }
# The function should check the validity of its arguments. If an invalid state value is passed to rankhospital,
# the function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to rankhospital, the function should throw an error via the stop function with
# the exact message “invalid outcome”.
# Here is some sample output from the function.
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# 3
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
# Save your code for this function to a file named rankhospital.R.

rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
    data_part <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    data_states <- unique(data_part[,7]) # State data extraction
    data_outcome <- list("heart attack", "heart failure", "pneumonia")
    data_part <- data_part[, c(2, 7, 11, 17, 23)]
    data_part[, 3] <- as.numeric(data_part[, 3])
    data_part[, 4] <- as.numeric(data_part[, 4])
    data_part[, 5] <- as.numeric(data_part[, 5]) # changing numeric data into numeric type
    # name, state, heart_attack, heart_failure, pneumonia
    
    ## Check that state and outcome are valid
    if(!is.element(state, data_states)){ # check invalid state
        stop("invalid state")
    }else if(!is.element(outcome, data_outcome)){ # compare with possible outcome
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    # If the number given by num is larger than the number of hospitals in that state, then the function should return NA.
    
    ## choose data
    # choose state
    data_part <- data_part[data_part$State == state, ] 
    # select data according to "outcome"
    if(outcome == "heart attack"){ # column K(11)
        # remove NA
        data_part <- data_part[!is.na(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
        # order from lowest
        data_part <- data_part[order(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data_part$Hospital.Name), ]
        data_part <- data_part[, c(1, 4)] # hospital_name, Heart_attack_data
    }
    else if(outcome == "heart failure"){ # column Q(17)
        # remove NA
        data_part <- data_part[!is.na(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
        # order from lowest
        data_part <- data_part[order(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, data_part$Hospital.Name), ]
        data_part <- data_part[, c(1, 4)] # hospital_name, Heart_failure_data
    }
    else if(outcome == "pneumonia"){ # column W(23)
        # remove NA
        data_part <- data_part[!is.na(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
        # order from lowest
        data_part <- data_part[order(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, data_part$Hospital.Name), ]
        data_part <- data_part[, c(1, 5)] #hospital_name, Pneumonia_data

    }else{
        stop("ERROR CODE 1. not exact outcome code")
    }
    
    ## handling "num"
    if(num == 'best'){
        return(data_part[1, 1])
    }
    else if(num == 'worst'){
        return(data_part[length(data_part$Hospital.Name), 1])
    }
    else if(class(1) == 'numeric'){
        if(num > length(data_part$Hospital.Name)){
            return(NA)
        }
        else{
            return(data_part[num, 1])
        }
    }
    else{
        stop("ERROR CODE 2. not exact num")
    }
}