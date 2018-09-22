rankall <- function(outcome, num = "best") {
        care_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character") 
        
        outcomedata <- c("heart attack", "heart failure", "pneumonia")
        statedata <- care_data[,'State']
        
        #Check state
        if (sum(statedata==state) == 0){
                stop("invalid state")
        }
        
        #Check outcome
        if (sum(outcomedata==outcome) == 0){
                stop("invalid outcome")
        }
        lower_data <- tolower(statedata)
        dup_data <- duplicated(lower_data)
        all_state <- statedata[!dup_data]
        all_state <- all_state[order(all_state)]
        thehospital<- lapply(all_state,rankhospital,outcome,num)
        thehospital <- as.character(thehospital)
        data_rankall <- data.frame('hospital' = thehospital, 'state' = all_state, row.names = all_state)
        data_rankall
}