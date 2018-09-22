best <- function(state, outcome){
        ##Read outcome data
        care_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ##Check that state and outcome are valid
        
        #set subset to compare with params
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
        
        ##Return hospital name in that state with lowest 30-day death
        ##rate
        thestate <- care_data[,'State']==state
        thestate_data <- care_data[thestate,]
        
        if(outcome == "heart attack"){
                compare_col <- 11
        }
        if(outcome == "heart failure"){
                compare_col <- 17
        }
        if(outcome == "pneumonia"){
                compare_col <- 23
        }
        
        ha_rate <- as.numeric(thestate_data[,compare_col])
        min_ha_rate <- min(ha_rate,na.rm=TRUE)
        min_row <- which(ha_rate==min_ha_rate)

        hospital_name<- thestate_data[min_row,2]
        hospital_name[order(hospital_name)]
        
}