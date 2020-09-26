best <- function(state, outcome){
    #read data
    preData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if(!(state %in% preData$State )){
        stop("invalid state")
    }
    #extract the data of the given state
    data <- preData[which(preData$State == state), names(preData)]
    
    #extract the column of the given outcome
    if(outcome == "heart attack"){
        outcomeCol <- as.numeric(data[,11])
    }
    else if(outcome == "heart failure"){
        outcomeCol <- as.numeric(data[,17])
    }
    else if(outcome == "pneumonia"){
        outcomeCol <- as.numeric(data[,23])
    }
    else{
        stop("invalid outcome")
    }
    
    lowest <- min(outcomeCol, na.rm = TRUE) #find the lowest rate
    hospitals <- sapply(outcomeCol, function(x) x == lowest) 
    lowestHospitals <- which(hospitals) #find the indices of the hospitals with the lowest rate
    hospitalNames <- c() #vector with the names of the hospitals with the lowest rate
    for (i in lowestHospitals) {
        hospitalNames <- c(hospitalNames, data$Hospital.Name[i]) 
    }
    hospitalNames <- sort(hospitalNames) #sort the names of the hospitals alphabetically
    hospitalNames[1] #return the first name
}