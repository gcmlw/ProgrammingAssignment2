best <- function(state, outcome) {
        ## Read outcome data
        
        ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        
        states <- ds[, "State"]
        if ( !(state %in% states)) {
                error <- paste("Error in best( ", state , ", " , outcome, ") : invalid state" )
                return(error)
        }
        
        
        validoutcomes <- c("heart attack","heart failure","pneumonia")
        if ( !(outcome %in% validoutcomes)) {
                error <- paste("Error in best( ", state , ", " , outcome, ") : invalid outcome" )
                return(error)
        }
        
        # 11, 17, 23
        ds[, 11] <- as.numeric(ds[, 11])
        ds[, 17] <- as.numeric(ds[, 17])
        ds[, 23] <- as.numeric(ds[, 23])
        # 2 Hospital Name
        # 7 State
        hospitalName <- ""
        res1 <- ds[which(ds$State==state),]
        if(outcome == "heart attack") {
                #11
                min1 <- min(res1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = T)
                res2 <- res1[which(res1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min1),]
                res3 <- res2[order(res2$Hospital.Name),]
                hospitalName <- res3[1,2]
        } else if (outcome == "heart failure") {
                #17
                min1 <- min(res1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = T)
                res2 <- res1[which(res1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min1),]
                res3 <- res2[order(res2$Hospital.Name),]
                hospitalName <- res3[1,2]
        } else if (outcome == "pneumonia") {
                #23
                min1 <- min(res1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = T)
                res2 <- res1[which(res1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min1),]
                res3 <- res2[order(res2$Hospital.Name),]
                hospitalName <- res3[1,2]
        }
        hospitalName
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        

        
        
        
}