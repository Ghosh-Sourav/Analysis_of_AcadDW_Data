

subject.similarity.matrix.dept <- matrix(nrow = document.count, ncol = document.count)
rownames(subject.similarity.matrix.dept) <- colnames(subject.similarity.matrix.dept) <- rownames(filtered_subject_details)

count.s1 <- count.s2 <- 0
for (s1 in s.list) {
  count.s1 <- count.s1 + 1
  count.s2 <- 0
  for (s2 in s.list) {
    count.s2 <- count.s2 + 1
    print(paste("[Checking Dept]:", count.s1, "(", (count.s1/document.count * 100), "%)", " > ", count.s2, "(", (count.s2/document.count * 100), "%)","..."))
    if(paste(filtered_subject_details[s1, "offer_dept"]) == paste(filtered_subject_details[s2, "offer_dept"])) {
      subject.similarity.matrix.dept[s1,s2] <- 1
    } else {
      subject.similarity.matrix.dept[s1,s2] <- 0
    }
  }
}

save(subject.similarity.matrix.dept, file = paste("subject.similarity.matrix.dept", ".RData"))

