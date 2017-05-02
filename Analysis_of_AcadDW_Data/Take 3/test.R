

test.matrix <- matrix(nrow = 10, ncol=5)
test.matrix[,1] <-1:10
test.matrix[,2] <-11:20
test.matrix[,3] <- 15*test.matrix[,1]
test.matrix[,4] <- 2*test.matrix[,1]

test.fx <- function(x){
  x <- x+1001
  return(x)
}

test.matrix[,5]<- sapply(test.matrix[,2], test.fx)

test.matrix[,5] <- test.matrix[,1] %*% test.matrix[,2]