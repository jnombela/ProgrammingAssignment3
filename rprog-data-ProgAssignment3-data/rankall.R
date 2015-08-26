rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcomes <- c('heart attack','heart failure','pneumonia')
  if (sum(outcomes==outcome)==0)     ## outcome is not in short list
    stop ('invalid outcome')
  
  if ((!is.integer(as.integer(num)))& (num!='best') & (num!='worst'))
    stop ('invalid num')
  
  ## For each state, find the hospital of the given rank
  data <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
  states <- unique(outcomeFile[,7])
  states <- states [ order(states) ] 
  
  for (i in 1:length(states)){
    ss_state <- outcomeFile[,7] == states[i]       ## state subsetting
    outcomeFile_state <- outcomeFile[ss_state,]
    
                                                   ##column subsetting
    switch(outcome,
           'heart attack'={file <- outcomeFile_state[,c(2,11)]},
           'heart failure'={file <- outcomeFile_state[,c(2,17)]},
           'pneumonia'={file <- outcomeFile_state[,c(2,23)]}
    )
    
    if ((is.numeric(num)) && (num>nrow(file))){   ## discard if num > hospitals
      data[i,] <- c(NA,states[i])
      next
    }
    

    file[, 2] <- as.numeric(file[, 2])                ## numeric for ordering numbers
    file <- file[complete.cases(file),]
    ord_file <- file [ order(file[,2], file[,1]), ]   ## order file by rate and hosp. name
    
    n<-as.integer(num)                            ## transform num in row index
    switch(num,
           'best'= {n<-1},
           'worst'= {n<-nrow(ord_file)},
           n<-as.integer(num)
    )
        
    data[i,] <- c(ord_file[n,1],states[i])
    
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data
}