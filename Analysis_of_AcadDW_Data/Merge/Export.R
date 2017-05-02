# # Preferable to run on server
#
# subject.similarity.matrix.score <- subject.similarity.matrix
#
# # Creating matrix for export to CSV for database
# count <- 0
# match <- 0
# max.count <- nrow(subject.similarity.matrix.score) * ncol(subject.similarity.matrix.score)
# export.to.csv.for.db <- data.frame(sub1 = character(), sub2 = character(), similarity = numeric(), stringsAsFactors = FALSE)
# for(i in rownames(subject.similarity.matrix.score)) {
#   for (j in colnames(subject.similarity.matrix.score)) {
#     count <- count + 1
#     print(paste("i =", i, ", j =", j, "processed = ", count, "(", (count/max.count * 100), "% )"))
#     if(i < j && !is.na(subject.similarity.matrix.score[i, j])) {
#       match <- match + 1
#       export.to.csv.for.db[nrow(export.to.csv.for.db)+1, ] <- list(i, j, subject.similarity.matrix.score[i, j])
#     }
#   }
# }

export.to.csv.for.db <- as.data.frame(as.table(subject.similarity.matrix), stringsAsFactors = FALSE)
colnames(export.to.csv.for.db) <- c("sub1", "sub2", "similarity")

export.to.csv.for.db <- export.to.csv.for.db[export.to.csv.for.db$sub1 < export.to.csv.for.db$sub2 &
                                               !is.na(export.to.csv.for.db$similarity), ]

getwd()
write.csv(export.to.csv.for.db, file = "export.to.csv.for.db.csv")
