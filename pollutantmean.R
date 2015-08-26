pollutantmean <- function(directory, pollutant, id = 1:332) {
  datos = vector()

    for (i in 1:length(id)){
    name = paste(".\\",directory,"\\",formatC(id[i],width = 3,flag="0"),".csv",sep="")
    f = read.csv(name)
    d = f[,pollutant]
    na = is.na(d)
    datos = c(datos,d[!na])
  }
  format(mean(datos),digits=4)
}