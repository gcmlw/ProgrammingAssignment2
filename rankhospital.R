rankhospital <- function(state, outcome, num = "best") {
        ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        
        states <- ds[, "State"]
        if ( !(state %in% states)) {
                error <- paste("Error in best( ", state , ", " , outcome, ") : invalid state" )
                stop(error)
        }
        
        
        validoutcomes <- c("heart attack","heart failure","pneumonia")
        if ( !(outcome %in% validoutcomes)) {
                error <- paste("Error in best( ", state , ", " , outcome, ") : invalid outcome" )
                stop(error)
        }
        
        # 11, 17, 23
        ds[, 11] <- as.numeric(ds[, 11])
        ds[, 17] <- as.numeric(ds[, 17])
        ds[, 23] <- as.numeric(ds[, 23])
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        
        hospitalName <- ""
        res1 <- ds[which(ds$State==state),]
        if(outcome == "heart attack") {
                #11
                res2 <- res1[!is.na(res1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                res3 <- res2[order(res2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,res2$Hospital.Name),]
        } else if (outcome == "heart failure") {
                #17
                res2 <- res1[!is.na(res1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                res3 <- res2[order(res2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,res2$Hospital.Name),]
        } else if (outcome == "pneumonia") {
                #23
                res2 <- res1[!is.na(res1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                res3 <- res2[order(res2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,res2$Hospital.Name),]
        } else {
                stop("")
        }
        if(is.character(num)) {
                if(num == "worst") {
                        num <- nrow(res3)
                } else if(num == "best") {
                        num <- 1
                }
        }
        
        if(num <= nrow(res3)) {
                hospitalName <- res3[num,2]
        } else {
                hospitalName <- "NA"
        }
        hospitalName
}