# Forked from "Take 4/Semantic.Similarity.Analysis.hclust.R"

# Required:
#   subject.similarity.matrix.score


# NEED TO SET THIS MANUALLY!!!
optimal.no.of.clusters <- 63+35
print(paste("manually set optimal.no.of.clusters =", optimal.no.of.clusters))


library("cluster")
library("reshape2")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Hierarchical Clustering Based on Optimal No of Clusters as Input")

source("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Read CSV Data/ReadAndClean.Syllabus.from.CSV.R", echo = TRUE)


# *** CLEANING NaN ROWS & COLUMNS : BEGIN ***

rc.full.NaN <- apply(X = subject.similarity.matrix.score, MARGIN = 1, FUN = function(x) all(is.na(x)))
subject.similarity.matrix.score <- subject.similarity.matrix.score[!rc.full.NaN, !rc.full.NaN]

# *** CLEANING NaN ROWS & COLUMNS : END ***




# Creating distance matrix
#--inverse.score <- 1/subject.similarity.matrix.score - 1
#--inverse.score[subject.similarity.matrix.score == 0] <- 10e+200
inverse.score <- 1 - subject.similarity.matrix.score
subject.distance.matrix <- as.dist(inverse.score)



# Get list of unique depts.
# dept.list.unique <- sort(unique(substr(rownames(subject.similarity.matrix.score), 1, 2)))
dept.list.unique <- sort(unique(as.character(csv.syllabus$dept_code)))




# Set no of clusters
no.of.clusters <- optimal.no.of.clusters

# Form the clusters
clusters.tree <- hclust(subject.distance.matrix)


# Plot the hierarchical clustering
# pdf(file = "hierarchical.clustering_best_partition.pdf", width = 200, height = 20, pointsize = 2)
# plot(clusters.tree)
# rect.hclust(clusters.tree, k = no.of.clusters, border = 2:(2+no.of.clusters-1))
# dev.off()


# Derive info from clusters in manipulatable form
clusters.info.list <- cutree(clusters.tree, k = no.of.clusters)
# clusters.info.list <- clusters.NbClust$Best.partition
clusters.info.df <- data.frame(dept =  substr(names(clusters.info.list),1,2), sub = names(clusters.info.list), cluster = clusters.info.list, stringsAsFactors=FALSE)
for (each.subject in rownames(clusters.info.df)) {
  clusters.info.df[each.subject, "dept"] <- as.character(csv.syllabus[each.subject, "dept_code"])
}




####################################################################
####################################################################


# *** Visualise the clustering results : BEGIN ***

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

write.csv(clusters.info.df, file = "clusters.info.df.csv", row.names = TRUE)
write.csv(dept.cluster.matrix, file = "dept.cluster.matrix.csv", row.names = TRUE)
write.csv(dept.distribution.percentage, file = "dept.distribution.percentage.csv", row.names = TRUE)
write.csv(cluster.composition.percentage, file = "cluster.composition.percentage.csv", row.names = TRUE)


# *** Visualise the clustering results : END ***



