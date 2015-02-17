rankall <- function (outcome, num = "best") {
    ## read data from the file
    data <- read.csv ("outcome-of-care-measures.csv", colClasses = "character")
    valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    
    ## checking the validity of the arguments state&outcome
    if (sum(sapply (outcome, function (elt) elt == valid_outcome)) == 0) {
        stop ("invalid outcome")
    }
    
    ## prepare data
    col <- if (outcome == valid_outcome [1]) {
        11
    } else if (outcome == valid_outcome [2]){
        17
    } else {
        23
    }
    #print (col)
    data[ , col] <- as.numeric(data[ , col])
    data_valid <- data[complete.cases(data[ , col]), ]
    
    num <- if (num == "best") {
        1
    } else {
        num
    }
    
    ## return a data frame with the hospital names and the states
    ## with the given rank of 30-day death rate of outcome
    
    sorted <- data_valid[order(data_valid$State, data_valid[, col], data_valid[, 2]), ]
    splited_sorted <- split(sorted, sorted$State)
    
    res <- data.frame(hospital = length(splited_sorted), state = sort(unique(sorted$State)))
    
    
    for (i in seq_len(length(splited_sorted))) {
        state_data <- splited_sorted[[i]]
        #res[i,2] <- state_data[1,7]
        
        
        if (num == "worst") {
            res[i,1] <- state_data[nrow(state_data), 2] 
        } else {
            res[i,1] <- state_data[num, 2] 
        }
        
        #print(num)
    }
    
    return (res)
    #print (res)
    
    
}
