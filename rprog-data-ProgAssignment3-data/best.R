best <- function(state, outcome) {
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(outcomeFile[,7])  ## gets unique list of states in file
  if (sum(states==state)==0)         ## state is not present
      stop ('invalid state') 
  
  outcomes <- c('heart attack','heart failure','pneumonia')
  if (sum(outcomes==outcome)==0)     ## outcome is not in short list
      stop ('invalid outcome')
    
  ## Return hospital name in that state with lowest 30-day death rate
  ss_state <- outcomeFile[,7] == state       ## state subsetting
  outcomeFile_state <- outcomeFile[ss_state,]
  
  switch(outcome,
         'heart attack'={file <- outcomeFile_state[,c(2,11)]},
         'heart failure'={file <- outcomeFile_state[,c(2,17)]},
         'pneumonia'={file <- outcomeFile_state[,c(2,23)]}
         )
  file[, 2] <- as.numeric(file[, 2])                ## numeric for ordering numbers
  ord_file <- file [ order(file[,2], file[,1]), ] 
  ##print (head(ord_file,10))
  ord_file[1,1]
}
 