rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        
        outcome_dat <- read.csv("outcome-of-care-measures.csv")
        pos_out <- data.frame(Ok = c("heart attack", "heart failure", "pneumonia"), pos = c(11,17,23))
        ## Check that state and outcome are valid
        
        if(!any(outcome_dat$State %in% state) ) {
                stop("invalid state")
        }
        if(!any(pos_out$Ok %in% outcome)) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        
        ind <- pos_out[which(pos_out$Ok == outcome), 2]
        sol_dat <- subset(outcome_dat, State == state, select = c(2, ind))
        suppressWarnings(sol_dat[,2] <- as.numeric(sol_dat[,2]))
        if(is.numeric(num)) {
                solution_base <- sol_dat[order(sol_dat[,2]),]
                solution_abc <- solution_base[order(solution_base[,1]),1]
                solution <- solution_abc[num]
        } else if(num =="best") {
                solution_base <- subset(sol_dat, sol_dat[,2] == min(sol_dat[,2], na.rm = TRUE), select = 1)
                solution <- head(solution_base[order(solution_base[,1]),], 1)
        
        } else if(num == "worst") {
                solution_base <- subset(sol_dat, sol_dat[,2] == max(sol_dat[,2], na.rm = TRUE), select = 1)
                solution <- tail(solution_base[order(solution_base[,1]),], 1)
        } else {
                stop("invalid num")
        }
        print(solution)
}