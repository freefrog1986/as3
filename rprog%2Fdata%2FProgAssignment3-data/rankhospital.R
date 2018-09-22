rankhospital <- function(state, outcome, num = "best"){
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
        hospital_name <- thestate_data[,2]
        outcome_name <- thestate_data[,compare_col]
        thehospital <- hospital_name[order(ha_rate,hospital_name,na.last = NA)]
        outcome_name <- outcome_name[order(ha_rate,hospital_name,na.last = NA)]
        if(num=="best"){
                num<-1
        }
        if(num=="worst"){
                num<-length(thehospital)
        }
        
        thehospital[num]
}