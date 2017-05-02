
### store merged matrix as subject.similarity.matrix


max.for.complete <- 0
max.i <- 0
max.j <- 0

#for(i in 1:nrow(subject.similarity.matrix)-1) {
for(i in 1:1) {
  for(j in (i+1):nrow(subject.similarity.matrix)) {
    print(paste("i =", i, ", j =", j))
    temp.for.max.for.complete <- sum(complete.cases(subject.similarity.matrix[i:j, i:j]))
    if(max.for.complete < temp.for.max.for.complete && temp.for.max.for.complete == j-i+1) {
      max.for.complete <- temp.for.max.for.complete
      max.i <- i
      max.j <- j
    }
  }
}

# Run the followning line again(One time)
complete.ssm <- subject.similarity.matrix[max.i:max.j, max.i:max.j]
save(complete.ssm, file = "complete.ssm.RData")
