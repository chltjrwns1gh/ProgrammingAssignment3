# 2. Finding the best hospital in a state
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).
# The function should use the following template.
# best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
# }
# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.

# Here is some sample output from the function.
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
# >
#     Save your code for this function to a file named best.R

best <- function(state, outcome) {
    ## Read outcome data
    data_part <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data_states <- unique(data_part[,7]) # State data extraction
    data_outcome <- list("heart attack", "heart failure", "pneumonia")
    data_part <- data_part[, c(2, 7, 11, 17, 23)]
    data_part[, 3] <- as.numeric(data_part[, 3])
    data_part[, 4] <- as.numeric(data_part[, 4])
    data_part[, 5] <- as.numeric(data_part[, 5])
    # name, state, heart_attack, heart_failure, pneumonia
    
    ## Check that state and outcome are valid
    if(!is.element(state, data_states)){ # check invalid state
        stop("invalid state")
    }else if(!is.element(outcome, data_outcome)){ # compare with possible outcome
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death rate
    # Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
    # be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
    # and “f” are tied for best, then hospital “b” should be returned).
    
    
    if(outcome == "heart attack"){ # column K(11)
        data_part <- data_part[data_part$State == state, ] # choosing state
        data_part <- data_part[!is.na(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ] # removing NA
        data_part <- data_part[order(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data_part$Hospital.Name), ] # ordering from lowest
        best_hospital_character <- data_part[1,1] # returning hospital_name
    }
    else if(outcome == "heart failure"){ # column Q(17)
        data_part <- data_part[data_part$State == state, ] # choosing state
        data_part <- data_part[!is.na(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ] # removing NA
        data_part <- data_part[order(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, data_part$Hospital.Name), ] # ordering from lowest
        best_hospital_character <- data_part[1,1] # returning hospital_name
    }
    else if(outcome == "pneumonia"){ # column W(23)
        data_part <- data_part[data_part$State == state, ] # choosing state
        data_part <- data_part[!is.na(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ] # removing NA
        data_part <- data_part[order(data_part$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, data_part$Hospital.Name), ] # ordering from lowest
        best_hospital_character <- data_part[1,1] # returning hospital_name
        
    }else{
        stop("unknown outcome")
    }
    return(best_hospital_character)
}
