rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_dat <- read.csv("outcome-of-care-measures.csv")
        pos_out <- data.frame(Ok = c("heart attack", "heart failure", "pneumonia"), pos = c(11,17,23))
        ## Check that state and outcome are valid
        
        if(!any(pos_out$Ok %in% outcome)) {
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        ind <- pos_out[which(pos_out$Ok == outcome), 2]
        solution <- data.frame()
        for(i in unique(outcome_dat$State)){
        
                sol_dat <- subset(outcome_dat, State == i, select = c(2, ind))
                colnames(sol_dat) <- c("name", "percent")
                suppressWarnings(sol_dat[,2] <- as.numeric(sol_dat[,2]))
                if(is.numeric(num)) {
                        solution_base <- sol_dat[with(sol_dat, order(percent, name)),]
                        hosp_name <- solution_base[num,1]
                } else if(num =="best") {
                        solution_base <- subset(sol_dat, sol_dat[,2] == min(sol_dat[,2], na.rm = TRUE), select = 1)
                        hosp_name <- head(solution_base[order(solution_base[,1]),], 1)
                        
                } else if(num == "worst") {
                        solution_base <- subset(sol_dat, sol_dat[,2] == max(sol_dat[,2], na.rm = TRUE), select = 1)
                        hosp_name <- tail(solution_base[order(solution_base[,1]),], 1)
                } else {
                        stop("invalid num")
                }
                solution <- rbind(solution, c(hosp_name, i))
        }
        colnames(solution) <- c("hospital", "state")
        solution <- solution[order(solution$state), ]
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        solution
}