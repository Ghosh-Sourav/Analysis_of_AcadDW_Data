
### store merged matrix as subject.similarity.matrix

# GREEDY ALGO: May not yield optimal result

max.v.set <- c()

seed_count <- 0
for (seed in c(1, sample(2:nrow(subject.similarity.matrix), 10))) {
  seed_count <- seed_count +1
  v.set <- c(seed)
  for(i in 1:nrow(subject.similarity.matrix)) {
    print(paste("seed_count = ", seed_count, "seed = ", seed, ", i =", i))
    # If introducing ith subject to v.set keeps clique complete, add ith subject to v.set
    if (sum(complete.cases(subject.similarity.matrix[unique(c(v.set, i)), unique(c(v.set, i))])) == length(unique(c(v.set, i)))) {
      v.set <- unique(c(v.set, i))
    }
  }

  if(length(v.set) > length(max.v.set)) {
    max.v.set <- v.set
  }

}
v.set <- max.v.set

print(v.set)
print(length(v.set))

# Run the followning line again(One time)
complete.ssm <- subject.similarity.matrix[v.set, v.set]
save(complete.ssm, file = "complete.ssm.RData")
