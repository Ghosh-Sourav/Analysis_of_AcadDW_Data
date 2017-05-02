
previous.na.count <- sum(is.na(subject.similarity.matrix))

# data.from.db <- read.csv("Similarity201703091104/Similarity201703091104.csv")
mismatch.matrix <- data.frame(sub1 = character(), sub2 = character(),
                       current.similarity = numeric(),
                       similarity.from.db = numeric(),
                       stringsAsFactors = FALSE)

exist.and.match  <- 0
exist.and.not.match <- 0
not.exist <- 0

mismatch.difference.total <- 0
total.data.to.process <- nrow(data.from.db)

for (i in 1:nrow(data.from.db)) {
  print(paste("Processing row", i, "(", (i / total.data.to.process) * 100, "% )"))
  subno1 <- as.character(data.from.db[i, "subno1"])
  subno2 <- as.character(data.from.db[i, "subno2"])
  similarity <- as.numeric(data.from.db[i, "similarity"])

  current.val <- subject.similarity.matrix[subno1, subno2]

  if(!is.na(current.val)) {
    if(similarity == current.val) {
      exist.and.match <- exist.and.match + 1
    } else {
      exist.and.not.match <- exist.and.not.match + 1
      print(paste("subno1=", subno1, "subno2=", subno2, "current.val=", current.val, "similarity=", similarity))
      mismatch.difference.total <- mismatch.difference.total + abs(similarity - current.val)
      mismatch.matrix[nrow(mismatch.matrix)+1, ] <- list(subno1, subno2, current.val, similarity)
      subject.similarity.matrix[subno1, subno2] <- subject.similarity.matrix[subno2, subno1] <- similarity #OVERWRITE
    }
  } else {
    not.exist <- not.exist + 1
    subject.similarity.matrix[subno1, subno2] <- subject.similarity.matrix[subno2, subno1] <- similarity
  }
}

print(paste("Mismatch average = ", (mismatch.difference.total/exist.and.not.match)))

current.na.count <- sum(is.na(subject.similarity.matrix))
print(paste("Verified = ", (previous.na.count == (current.na.count + 2 * not.exist)))) # Verification

save(subject.similarity.matrix, file = "subject.similarity.matrix.final.RData")

