
# required:
#  clusters.tree
#  inverse.score

library("cluster")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take Final")

min.nc <- 1
max.nc <- nrow(subject.similarity.matrix.score)

cluster.analysis.matrix <- matrix(nrow = length(min.nc:max.nc), ncol = 2)
rownames(cluster.analysis.matrix) <- cluster.analysis.matrix[, 1] <- min.nc:max.nc
colnames(cluster.analysis.matrix) <- c("no.of.clusters", "wss")

get.dissimilarity.squared <- function(x) {
  # paste(x[1],"-",x[2])
  diss.squared <- (inverse.score[x[1], x[2]] ^ 2)
}

for (K in max.nc:min.nc) {
  clusters.info.list <- cutree(clusters.tree, k = K)
  clusters.info.df <- data.frame(dept =  substr(names(clusters.info.list),1,2), sub = names(clusters.info.list), cluster = clusters.info.list)
  wss <- 0.0
  for (k in 1:K) {
    subjects.in.cluster.k <- sort(as.character(clusters.info.df[clusters.info.df$cluster == k, "sub"]))
    print(paste("K =", K, ", k =", k, ", #subjects =", length(subjects.in.cluster.k)))
    if (length(subjects.in.cluster.k) > 1) { # no contribution to wss if #subjects == 1
      wss <- wss + sum(combn(subjects.in.cluster.k, 2, get.dissimilarity.squared))
    }
    print(paste("-> cumulative.wss.for.K =", wss))
  }
  cluster.analysis.matrix[cluster.analysis.matrix[,"no.of.clusters"]==K, "wss"] <- wss
}

pdf(file = "cluster.analysis.plot.pdf", width = 80, height = 80)
plot(x = cluster.analysis.matrix[, "no.of.clusters"],
     y = cluster.analysis.matrix[, "wss"],
     type = "b",
     xlab = "No. of Clusters",
     ylab = "Within Sum of Squares",
     xaxt = "n"
    )
axis(side = 1, at = min.nc:max.nc, labels = TRUE, tick = TRUE)
dev.off()

