
library("cluster")
library("reshape2")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 3")

csv.similarity <- read.csv2(file = "../Data/Similarity.csv",
                          header = TRUE, sep = "|", na.strings = "#@@^@^DUMMYNOTAPPLICABLE@!$%!$")

csv.similarity$subno1 <- as.character(paste(csv.similarity$subno1))
csv.similarity$subno2 <- as.character(paste(csv.similarity$subno2))
csv.similarity$similarity <- as.double(paste(csv.similarity$similarity))

sublist1 <- sort(unique(csv.similarity$subno1))
sublist2 <- sort(unique(csv.similarity$subno2))
sublist <- sort(unique(c(sublist1, sublist2)))

subject.similarity.matrix.score <- matrix(nrow = length(sublist), ncol = length(sublist))
rownames(subject.similarity.matrix.score) <- colnames(subject.similarity.matrix.score) <- sublist

for(i in 1:nrow(csv.similarity)) {
  subno1 <- csv.similarity[i, "subno1"]
  subno2 <- csv.similarity[i, "subno2"]
  subject.similarity.matrix.score[subno1, subno2] <- subject.similarity.matrix.score[subno2, subno1] <- csv.similarity[i, "similarity"]
}

for(i in 1:nrow(subject.similarity.matrix.score)) {
  subject.similarity.matrix.score[i, i] <- 1
}

subject.similarity.matrix.score2 <- na.omit(subject.similarity.matrix.score)
