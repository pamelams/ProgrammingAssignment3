rankhospital <- function(state, outcome, num = "best"){
    #read data
    preData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if(!(state %in% preData$State )){
        stop("invalid state")
    }
    #extract the data of the given state
    data <- preData[which(preData$State == state), names(preData)]
    
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
    
    #get the name of the hospital that has the ranking specified
    if(num == "best"){
        orderedData[1,2]
    }
    else if(num == "worst"){
        orderedData[nrow(orderedData),2]
    }
    else if(num > nrow(orderedData)){
        NA
    }
    else{
        orderedData[num,2]
    }
}
