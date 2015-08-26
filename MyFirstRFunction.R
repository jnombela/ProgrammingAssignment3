add2 <- function (x,y){
  x + y # no hace falta que hagamos return, porque R, por defecto, devuelve la última sentencia
}

above10 <- function (x){
  sel <- x>10
  x[sel]
}

above <- function (x, n=10) {
  sel <- x>n
  x[sel]
}

columnMean <- function (m, removeNA=TRUE){
  numcol <- ncol(m)
  means <- numeric(numcol)
  for (i in 1:numcol){
    means[i] <- mean(m[,i], na.rm = removeNA)
  }
  means
  
}
  