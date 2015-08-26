rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(outcomeFile[,7])  ## gets unique list of states in file
  if (sum(states==state)==0)         ## state is not present
    stop ('invalid state') 
  
  outcomes <- c('heart attack','heart failure','pneumonia')
  if (sum(outcomes==outcome)==0)     ## outcome is not in short list
    stop ('invalid outcome')
  
  if ((!is.integer(as.integer(num)))& (num!='best') & (num!='worst'))
    stop ('invalid num')
 
  ## Return hospital name in that state with the given rank 30-day death rate
  ss_state <- outcomeFile[,7] == state       ## state subsetting
  outcomeFile_state <- outcomeFile[ss_state,]
  
  switch(outcome,
         'heart attack'={file <- outcomeFile_state[,c(2,11)]},
         'heart failure'={file <- outcomeFile_state[,c(2,17)]},
         'pneumonia'={file <- outcomeFile_state[,c(2,23)]}
  )
  
  if ((is.numeric(num)) && (num>nrow(file)))   ## discard if num > hospitals
    return (NA)
  
  file[, 2] <- as.numeric(file[, 2])                ## numeric for ordering numbers
  file <- file[complete.cases(file),]
  ord_file <- file [ order(file[,2], file[,1]), ]   ## order file by rate and hosp. name
  ##print (head(ord_file,10))
  
  
  n<-as.integer(num)
  switch(num,
         'best'= {n<-1},
         'worst'= {n<-nrow(ord_file)},
         n<-as.integer(num)
  )
  
  ord_file[n,1]
  
}






