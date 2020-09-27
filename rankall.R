rankall <- function(outcome, num = "best"){
    #read data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #create the dataframe to be returned
    ranks <- data.frame(hospital = NA, state = unique(data$State))
    ranks <- ranks[order(ranks$state),]
    
    #order the data according to the given outcome and then by the hospital names alphabetically
    if(outcome == "heart attack"){
        data[,11] <- as.numeric(data[,11])
        orderedData <- data[order(data[,11], data[,2], na.last = NA),]
    }
    else if(outcome == "heart failure"){
        data[,17] <- as.numeric(data[,17])
        orderedData <- data[order(data[,17], data[,2], na.last = NA),]
    }
    else if(outcome == "pneumonia"){
        data[,23] <- as.numeric(data[,23])
        orderedData <- data[order(data[,23], data[,2], na.last = NA),]
    }
    else{
        stop("invalid outcome")
    }
    
    #get the name of the hospital that has the ranking specified for each state
    for (state in 1:54) {
        fromState <- orderedData[which(orderedData$State == ranks$state[state]), names(orderedData)]
        if(num == "best"){
            ranks[state, 1] <- fromState[1,2]
        }
        else if(num == "worst"){
            ranks[state, 1] <- fromState[nrow(fromState),2]
        }
        else if(num > nrow(fromState)){
            ranks[state, 1] <- NA
        }
        else{
            ranks[state, 1] <- fromState[num,2]
        }
    }
    
    ranks
}