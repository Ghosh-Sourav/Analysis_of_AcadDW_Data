
setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Merge")
# set backup folder as working directory

# load files one by one manually; then run the appropriate command from the following lines:
ssm.0 <- subject.similarity.matrix
ssm.1 <- subject.similarity.matrix
ssm.2 <- subject.similarity.matrix
ssm.3 <- subject.similarity.matrix
ssm.4 <- subject.similarity.matrix
ssm.5 <- subject.similarity.matrix
ssm.6 <- subject.similarity.matrix
ssm.7 <- subject.similarity.matrix

subject.similarity.matrix <- ssm.0

for (i in 1:nrow(subject.similarity.matrix)) {
  for (j in 1: ncol(subject.similarity.matrix)) {
    if(is.na(subject.similarity.matrix[i,j])) {
      if(!is.na(ssm.1[i, j])) {
        subject.similarity.matrix[i,j] <- ssm.1[i,j]
      } else if(!is.na(ssm.2[i, j])) {
        subject.similarity.matrix[i,j] <- ssm.2[i,j]
      }  else if(!is.na(ssm.3[i, j])) {
        subject.similarity.matrix[i,j] <- ssm.3[i,j]
      }  else if(!is.na(ssm.4[i, j])) {
        subject.similarity.matrix[i,j] <- ssm.4[i,j]
      }  else if(!is.na(ssm.5[i, j])) {
        subject.similarity.matrix[i,j] <- ssm.5[i,j]
      }  else if(!is.na(ssm.6[i, j])) {
        subject.similarity.matrix[i,j] <- ssm.6[i,j]
      }  else if(!is.na(ssm.7[i, j])) {
        subject.similarity.matrix[i,j] <- ssm.7[i,j]
      }
    }
    print(paste("i =", i, ", j =", j, ", val =", subject.similarity.matrix[i,j]))
  }
}

save(subject.similarity.matrix, file = "subject.similarity.matrix.merged.RData")

