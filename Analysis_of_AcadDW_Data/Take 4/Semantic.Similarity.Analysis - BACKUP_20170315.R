
library("cluster")
library("reshape2")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 4")

subject.similarity.matrix.score <- complete.ssm

# Creating distance matrix
inverse.score <- 1/subject.similarity.matrix.score - 1
inverse.score[subject.similarity.matrix.score == 0] <- 10e+40
inverse.score[is.na(inverse.score)] <- 10e+40
subject.distance.matrix <- as.dist(inverse.score)

# clusters.tree <- agnes(subject.distance.matrix)
clusters.tree <- hclust(subject.distance.matrix)


is.reqd.dendrogram <- TRUE
is.reqd.qgraph <- FALSE

if(is.reqd.dendrogram) {
  #png(filename = "clusterPlot.png",  width = 20, height = 20, units = 'in', res = 1500)
  #pdf(file = "~/InterDSA/clusterPlot_ngramless.pdf",  width = 200, height = 20, pointsize = 2)
  pdf(file = "clusterPlot.pdf",  width = 200, height = 20, pointsize = 2)
  plot(as.dendrogram(clusters.tree))
  dev.off()
}

if(is.reqd.qgraph) {
  library(qgraph)
  pdf(file = "qgraph.pdf",  width = 200, height = 200, pointsize = 2)
  # qgraph(subject.similarity.matrix.score[1:50,1:50], layout='spring', vsize=3)
  # qgraph(subject.similarity.matrix.score[1:400,1:400], layout='spring',  minimum=1e-2, maximum=1, details=TRUE)
  qgraph(subject.similarity.matrix.score, layout='spring',  details=TRUE)
  # use standalone=TRUE/FALSE and filetype="tex" for LaTeX usage
  dev.off()
}


dept.list.unique <- sort(unique(substr(rownames(subject.similarity.matrix.score), 1, 2)))

no.of.clusters <- length(dept.list.unique)
clusters.info.list <- cutree(clusters.tree, k = no.of.clusters)
# View(table(clusters.info))
# dept.sub.split <- colsplit(names(clusters.info.list), pattern = "\\.", names = c("dept","sub"))
# clusters.info.df <- data.frame(dept =  dept.sub.split$dept, sub = dept.sub.split$sub, cluster = clusters.info.list)
clusters.info.df <- data.frame(dept =  substr(names(clusters.info.list),1,2), sub = names(clusters.info.list), cluster = clusters.info.list)

dept.cluster.matrix <- matrix(nrow = length(dept.list.unique), ncol = no.of.clusters)
rownames(dept.cluster.matrix) <- dept.list.unique
colnames(dept.cluster.matrix) <- paste("", 1:no.of.clusters, sep = "")

for(each.dept in rownames(dept.cluster.matrix)) {
  for(each.cluster in colnames(dept.cluster.matrix)) {
    dept.cluster.matrix[each.dept, each.cluster] <- nrow(
      clusters.info.df[
        clusters.info.df$dept == each.dept
        & clusters.info.df$cluster == each.cluster,
        ]
    )
  }
}

# optionally use t() to maintain semantics
dept.distribution.percentage <- apply(dept.cluster.matrix, 1, function(x) 100*x/sum(x))
cluster.composition.percentage <- apply(dept.cluster.matrix, 2, function(x) 100*x/sum(x))

write.csv2(clusters.info.df, file = "clusters.info.df.csv", row.names = TRUE, col.names = TRUE)
write.csv2(dept.cluster.matrix, file = "dept.cluster.matrix.csv", row.names = TRUE, col.names = TRUE)
write.csv2(dept.distribution.percentage, file = "dept.distribution.percentage.csv", row.names = TRUE, col.names = TRUE)
write.csv2(cluster.composition.percentage, file = "cluster.composition.percentage.csv", row.names = TRUE, col.names = TRUE)

# count <- 0
# list.of.subjects <- c()
# for(each.subject in rownames(clusters.info.df[clusters.info.df$cluster==7,])){
#   print(each.subject)
#   each.subject <- names(subject.list.alias[subject.list.alias==each.subject])[1]
#   print(each.subject)
#   list.of.subjects <- c(list.of.subjects, each.subject)
#   count<- count+1
# }
#
# temp.filtered_subject_details <- filtered_subject_details[list.of.subjects, ]
# temp.filtered_subject_details$sub_alias <- c(1:138)
#
# for(each.subject in rownames(temp.filtered_subject_details)) {
#   print(each.subject)
#   temp.filtered_subject_details[each.subject, "sub_alias"] <- subject.list.alias[each.subject]
# }
#
# write.csv2(temp.filtered_subject_details[, c(1,3,2)], file = "subjects.of.cluster.7.csv", sep = ",")












