
library("cluster")
library("reshape2")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 4 _ Data")

subject.similarity.matrix.score <- complete.ssm


# *** CLEANING NaN ROWS & COLUMNS : BEGIN ***

rc.full.NaN <- apply(X = subject.similarity.matrix.score, MARGIN = 1, FUN = function(x) all(is.na(x)))
subject.similarity.matrix.score <- subject.similarity.matrix.score[!rc.full.NaN, !rc.full.NaN]

# *** CLEANING NaN ROWS & COLUMNS : END ***




# Creating distance matrix
inverse.score <- 1/subject.similarity.matrix.score - 1
inverse.score[subject.similarity.matrix.score == 0] <- 10e+200
subject.distance.matrix <- as.dist(inverse.score)



# Get list of unique depts.
dept.list.unique <- sort(unique(substr(rownames(subject.similarity.matrix.score), 1, 2)))

# *** Get optimal no of clusters : BEGIN ***
# NOTE: totss = tot.withinss + betweenss
# NOTE: tot.withinss = sum(withinss)

library(factoextra)
library(NbClust)
# Method 1 : Using NbClust
clusters.NbClust <- NbClust(data = NULL, diss = subject.distance.matrix, distance = NULL,
                             min.nc = 6, max.nc = floor((nrow(subject.similarity.matrix.score) - 1)/2),
                             method = "ward.D2", index = "silhouette")

fviz_nbclust(clusters.NbClust, method = "wss", print.summary = TRUE)
# Note: Might use clusters.NbClust$Best.partition to set clusters.info.list

# Method 2: Using dissimilarity object
fviz_nbclust(x = subject.similarity.matrix.score, FUNcluster = pam, diss = subject.distance.matrix, method = "wss",
             k.max = 100,
             verbose = TRUE, print.summary = TRUE)

# ALL EXPERIMENTS
pdf(file = "OptimalCluster_pam_wss_stand.pdf", width = 200, height = 20, pointsize = 2)
fviz_nbclust(x = subject.similarity.matrix.score, FUNcluster = pam, diss = subject.distance.matrix, method = "wss",
             k.max = 120,
             verbose = TRUE, print.summary = TRUE, stand = TRUE)
dev.off()

pdf(file = "OptimalCluster_pam_silhouette.pdf", width = 200, height = 20, pointsize = 2)
fviz_nbclust(x = subject.similarity.matrix.score, FUNcluster = pam, diss = subject.distance.matrix, method = "silhouette",
             k.max = 120,
             verbose = TRUE, print.summary = TRUE)
dev.off()

pdf(file = "OptimalCluster_hcut_wss_3.pdf", width = 200, height = 20, pointsize = 2)
fviz_nbclust(x = subject.similarity.matrix.score, FUNcluster = hcut, diss = subject.distance.matrix, method = "wss",
             k.max = 25,
             verbose = TRUE, print.summary = TRUE, hc_method = "complete", stand = TRUE,graph = TRUE)
dev.off()

pdf(file = "OptimalCluster_hcut_silhouette.pdf", width = 200, height = 20, pointsize = 2)
fviz_nbclust(x = subject.similarity.matrix.score, FUNcluster = hcut, diss = subject.distance.matrix, method = "silhouette",
             k.max = 120,
             verbose = TRUE, print.summary = TRUE)
dev.off()

# 23-Mar-2017 on 730
pdf(file = "OptimalCluster_hcut_wss_3.pdf", width = 200, height = 20, pointsize = 2)
fviz_nbclust(x = subject.similarity.matrix.score, FUNcluster = hcut, diss = subject.distance.matrix, method = "wss",
             k.max = floor(nrow(subject.similarity.matrix.score-1)/2),
             verbose = TRUE, print.summary = TRUE, hc_method = "complete", stand = TRUE,graph = TRUE)
fviz_nbclust(x = clusters.NbClust, FUNcluster = hcut, diss = subject.distance.matrix, method = "wss",
             k.max = floor(nrow(subject.similarity.matrix.score-1)/2),
             verbose = TRUE, print.summary = TRUE, hc_method = "complete", stand = TRUE,graph = TRUE)
dev.off()

# Using Elbow method for hierarchical clustering
fviz_nbclust(subject.similarity.matrix.score, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)


# Using Average silhouette method for hierarchical clustering
require(cluster)
fviz_nbclust(subject.similarity.matrix.score, hcut, method = "silhouette",
             hc_method = "complete")

# MAY NEED TO SET THIS MANUALLY!!!
optimal.no.of.clusters <- as.integer(clusters.NbClust$Best.nc[1])    # 23
print(paste("manually set optimal.no.of.clusters =", optimal.no.of.clusters))
# *** Get optimal no of clusters : END ***


# Set no of clusters
no.of.clusters <- optimal.no.of.clusters

# Form the clusters
clusters.tree <- hclust(subject.distance.matrix)


# Plot the hierarchical clustering
pdf(file = "hierarchical.clustering_best_partition.pdf", width = 200, height = 20, pointsize = 2)
plot(clusters.tree)
rect.hclust(clusters.tree, k = no.of.clusters, border = 2:(2+no.of.clusters-1))
dev.off()


# Derive info from clusters in manipulatable form
clusters.info.list <- cutree(clusters.tree, k = no.of.clusters)
# clusters.info.list <- clusters.NbClust$Best.partition
clusters.info.df <- data.frame(dept =  substr(names(clusters.info.list),1,2), sub = names(clusters.info.list), cluster = clusters.info.list)


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



