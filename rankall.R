# 4. Ranking hospitals in all states
# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.
# Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.
# The function should use the following template.
# rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
# }
# NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call
# the rankhospital function from the previous section.
# The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
# the function should throw an error via the stop function with the exact message “invalid outcome”. The num
# variable can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are better).
# If the number given by num is larger than the number of hospitals in that state, then the function should
# return NA.
# Here is some sample output from the function.
# > source("rankall.R")
# > head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
# > tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY
# Save your code for this function to a file named rankall.R.

rankall <- function(outcome, num = "best") {
    # initializing return value df
    df = data.frame(hospital = c(), state = c())
    
    ## Read outcome data
    data_part <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data_states <- sort(unique(data_part[,7])) # State data extraction
    data_outcome <- list("heart attack", "heart failure", "pneumonia")
    data_part <- data_part[, c(2, 7, 11, 17, 23)]
    data_part[, 3] <- as.numeric(data_part[, 3]) # error appears d/t NA
    data_part[, 4] <- as.numeric(data_part[, 4]) # error appears d/t NA
    data_part[, 5] <- as.numeric(data_part[, 5]) # error appears d/t NA
    # name, state, heart_attack, heart_failure, pneumonia
    
    ## Check that state and outcome are valid
    if(!is.element(outcome, data_outcome)){ # compare with possible outcome
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    for(state in data_states){
        ## choose data
        # select state
        temp_data_part <- data_part[data_part$State == state, ] 
        # select data according to "outcome"
        if(outcome == "heart attack"){ # column K(11)
            temp_data_part <- temp_data_part[, c(1, 3)] # hospital_name, Heart_attack_data
            # remove NA
            temp_data_part <- temp_data_part[!is.na(temp_data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
            # order from lowest
            temp_data_part <- temp_data_part[order(temp_data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, temp_data_part$Hospital.Name), ]
        }
        else if(outcome == "heart failure"){ # column Q(17)
            temp_data_part <- temp_data_part[, c(1, 4)] # hospital_name, Heart_failure_data
            # remove NA
            temp_data_part <- temp_data_part[!is.na(temp_data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
            # order from lowest
            temp_data_part <- temp_data_part[order(temp_data_part$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, temp_data_part$Hospital.Name), ]
        }
        else if(outcome == "pneumonia"){ # column W(23)
            temp_data_part <- temp_data_part[, c(1, 5)] #hospital_name, Pneumonia_data
            # remove NA
            temp_data_part <- temp_data_part[!is.na(temp_data_part$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
            # order from lowest
            temp_data_part <- temp_data_part[order(temp_data_part$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, temp_data_part$Hospital.Name), ]
            
        }else{
            stop("ERROR CODE 1. not exact outcome code")
        }
        
        ## handling "num"
        if(num == 'best'){
            h_name <- temp_data_part[1, 1]
        }
        else if(num == 'worst'){
            h_name <- temp_data_part[length(temp_data_part$Hospital.Name), 1]
        }
        else if(class(1) == 'numeric'){
            if(num > length(temp_data_part$Hospital.Name)){
                h_name <- NA
            }
            else{
                h_name <- temp_data_part[num, 1]
            }
        }
        else{
            stop("ERROR CODE 2. not exact num")
        }
        
        # add to df
        row <- data.frame(h_name, state)
        names(row) <- c("hospital", "state")
        rownames(row) <- c(state)
        df <- rbind(df, row)
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    return(df)
}