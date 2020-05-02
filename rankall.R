rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid

        validoutcomes <- c("heart attack","heart failure","pneumonia")
        if ( !(outcome %in% validoutcomes)) {
                error <- paste("Error in best( ", state , ", " , outcome, ") : invalid outcome" )
                stop(error)
        }
        
        # 11, 17, 23
        ds[, 11] <- as.numeric(ds[, 11])
        ds[, 17] <- as.numeric(ds[, 17])
        ds[, 23] <- as.numeric(ds[, 23])
        
        dsByState <- split(ds, ds$State)

        ## For each state, find the hospital of the given rank
        if(outcome == "heart attack") {
                #11
                dsByStateOrdered <- lapply(dsByState, function(x) x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, x$Hospital.Name),] ) 
        } else if (outcome == "heart failure") {
                #17
                dsByStateOrdered <- lapply(dsByState, function(x) x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, x$Hospital.Name),] ) 
        } else if (outcome == "pneumonia") {
                #23
                dsByStateOrdered <- lapply(dsByState, function(x) x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, x$Hospital.Name),] ) 
        } else {
                stop("")
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        if(is.character(num)) {
                if(num == "worst") {
                        num <- nrow(res3)
                } else if(num == "best") {
                        num <- 1
                }
        }
        
        dsByStateFiltered <- lapply(dsByStateOrdered, function(x) x$Hospital.Name[num])
        stateList <- names(dsByStateFiltered)
        print(stateList)
        df <- as.data.frame(dsByStateFiltered,stringsAsFactors=FALSE)
        df <- rbind(df, stateList)
        row.names(df) <- c("Hospital.Name", "State")
        result <- t(df)
        result
}