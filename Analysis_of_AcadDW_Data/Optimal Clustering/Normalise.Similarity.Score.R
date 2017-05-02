
load("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Merge/Backup_20170325_Final/subject.similarity.matrix.final.RData")


avg.score <- (sum(subject.similarity.matrix) / length(subject.similarity.matrix))
max.score <- max(subject.similarity.matrix) # 1

subject.similarity.matrix.score <- (subject.similarity.matrix - avg.score) / (max.score - avg.score)
subject.similarity.matrix.score[subject.similarity.matrix.score < 0] <- 0
