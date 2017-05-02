# This is a subset of For.ngram.R
# Created for execution on server without tm library support

load("~/InterDSA/intermediate. 2 .RData")
setwd("~/InterDSA")

library("lsa")
library("NLP")
# library("tm")
library("sos")
library("stringr")
library("ngram")


utf <- matrix(nrow = length(filtered_subject_details[,2]),
              ncol = length(vocabulary))
ntf <- matrix(nrow = length(filtered_subject_details[,2]),
              ncol = length(vocabulary))
tf.idf <- matrix(nrow = length(filtered_subject_details[,2]),
                 ncol = length(vocabulary))

colnames(tf.idf) <- colnames(ntf) <- colnames(utf) <- vocabulary
rownames(tf.idf) <- rownames(ntf) <- rownames(utf) <- rownames(filtered_subject_details)

clean.syllabus <- gsub("\n|\t|\\s+"," ",filtered_subject_details$syllabus)
for(term in vocabulary) {
  print(paste("[Calc Unnorm-TF] Processing: ", term, "..."))
  utf[,term] <- str_count(clean.syllabus, term)
}

for(document in rownames(filtered_subject_details)) {
  print(paste("[Calc Norm-TF] Processing: ", document, "..."))
  ntf[document,] <- utf[document,] / str_count( filtered_subject_details[document,]$syllabus, "\\S+")
}

idf <- vector(mode = "numeric", length = length(vocabulary))
names(idf) <- vocabulary

for(term in vocabulary) {
  print(paste("[Calc IDF & TF.IDF] Processing: ", term, "..."))
  no.of.docs.having.term <- sum(utf[,term]>0, na.rm = TRUE)
  idf[term] <- 1 + log(document.count / no.of.docs.having.term)
  tf.idf[,term] <- ntf[,term] * idf[term]
}

subject.similary.matrix <- matrix(nrow = document.count, ncol = document.count)
rownames(subject.similary.matrix) <- colnames(subject.similary.matrix) <- rownames(filtered_subject_details)


# count.todo <- length(s.list) * length(s.list)
# count.completed <- 0
# for(s1 in s.list) {
#   for(s2 in s.list) {
#     if (s1 < s2) {
#       print(paste("[Calc CosSim: Init] Completed: ", count.completed, "--",(count.completed/count.todo*100), "% ... "))
#       subject.similary.matrix[s1,s2] <- subject.similary.matrix[s2,s1] <- (tf.idf[s1,] %*% tf.idf[s2,])
#       count.completed <- count.completed + 2
#     } else if (s1 == s2) {
#       print(paste("[Calc CosSim: Init] Completed: ", count.completed, "--",(count.completed/count.todo*100), "% ... "))
#       subject.similary.matrix[s1,s2] <- (tf.idf[s1,] %*% tf.idf[s2,])
#       count.completed <- count.completed + 1
#     }
#   }
# }

subject.similary.matrix <- tf.idf %*% t(tf.idf)


for(s1 in s.list) {
  print(paste("[Calc CosSim: R-wise Norm] Completed: ", s1, " ... "))
  subject.similary.matrix[s1,] <- subject.similary.matrix[s1,] / sqrt(subject.similary.matrix[s1,s1])
}

for(s2 in s.list) {
  print(paste("[Calc CosSim: C-wise Norm] Completed: ", s1, " ... "))
  subject.similary.matrix[,s2] <- subject.similary.matrix[,s2] / subject.similary.matrix[s2,s2]
}

if(N.for.ngram==1) {
  subject.similary.matrix.n1 <- subject.similary.matrix
} else if(N.for.ngram==2) {
  subject.similary.matrix.n2 <- subject.similary.matrix
} else if(N.for.ngram==3) {
  subject.similary.matrix.n3 <- subject.similary.matrix
}


save.image(paste("cossim.complete.", N.for.ngram, ".RData"))
