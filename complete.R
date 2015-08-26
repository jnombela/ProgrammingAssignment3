complete <- function(directory, id = 1:332){
  datos <- data.frame(id=integer(), nobs=integer())
  ##dimnames(datos)<-c("id", "nobs")
  ##colnames(datos) <- c("id", "nobs")
  
  for (i in 1:length(id)){
    name <- paste(".\\",directory,"\\",formatC(id[i],width = 3,flag="0"),".csv",sep="")
    f <- read.csv(name)
    d <- f[complete.cases(f),]
    ##NROW(na.omit(dataset))
    ##datos <- rbind(c(i,nrow(d)))
    ##datos[i,] <- c(id[i],nrow(d))
    datos[i,] <- c(id[i],nrow(na.omit(f)))
  }
  datos
}