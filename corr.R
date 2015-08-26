corr <- function(directory, threshold = 0){
  
  comp <- complete(directory,1:332)
  vcor <- vector()
  
  for (i in 1:nrow(comp)){
    if (comp[i,"nobs"] < threshold) {
      next()
    }
    name <- paste(".\\",directory,"\\",formatC(i,width = 3,flag="0"),".csv",sep="")
    f <- read.csv(name)
    d <- f[complete.cases(f),]
    if (nrow(d) >0) {
      vcor <- c(vcor,cor(d[,2],d[,3]))
    }

  }
  vcor
}