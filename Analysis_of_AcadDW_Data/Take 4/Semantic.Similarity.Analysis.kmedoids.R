
library("cluster")
library("reshape2")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 4")

subject.similarity.matrix.score <- complete.ssm


# *** CLEANING NaN ROWS & COLUMNS : BEGIN ***

rc.full.NaN <- apply(X = subject.similarity.matrix.score, MARGIN = 1, FUN = function(x) all(is.na(x)))
subject.similarity.matrix.score <- subject.similarity.matrix.score[!rc.full.NaN, !rc.full.NaN]

# *** CLEANING NaN ROWS & COLUMNS : END ***


# Creating distance matrix
inverse.score <- 1/subject.similarity.matrix.score - 1
inverse.score[subject.similarity.matrix.score == 0] <- 10e+20
subject.distance.matrix <- as.dist(inverse.score)


# Get list of unique depts.
dept.list.unique <- sort(unique(substr(rownames(subject.similarity.matrix.score), 1, 2)))

# *** Get optimal no of clusters : BEGIN ***
# NOTE: totss = tot.withinss + betweenss
# NOTE: tot.withinss = sum(withinss)

cluster.goodness.df <- data.frame(k=integer(), tot.withinss = numeric())
for(no.of.clusters in 1:(5*length(dept.list.unique))) {
  print(paste("Processing for no.of.clusters =", no.of.clusters))
  clusters.kmm <- kmeans(inverse.score, centers = no.of.clusters)
  cluster.goodness.df[nrow(cluster.goodness.df)+1, ] <- list(no.of.clusters, clusters.kmm$tot.withinss)
}

pdf(file = "cluster.goodness.df.pdf",  width = 10, height = 10, pointsize = 2)
plot.x <- cluster.goodness.df$k
plot.y <- cluster.goodness.df$tot.withinss
plot(plot.x, plot.y, type = "p")
abline(lm(plot.y~plot.x), col="red") # regression line (y~x)
lines(lowess(plot.x,plot.y), col="blue") # lowess line (x,y)
dev.off()

# TO THINK ABOUT:
# 42: Quick-TRANSfer stage steps exceeded maximum (= 21400)
# 43: did not converge in 10 iterations
# 44: Quick-TRANSfer stage steps exceeded maximum (= 21400)

# SET THIS MANUALLY!!!
optimal.no.of.clusters <- 23
print(paste("manually set optimal.no.of.clusters =", optimal.no.of.clusters))
# *** Get optimal no of clusters : END ***


# Set no of clusters
no.of.clusters <- optimal.no.of.clusters

# Form the clusters
clusters.kmedoids <- pam(inverse.score, k = optimal.no.of.clusters, diss = TRUE)

# Derive info from clusters in manipulatable form
clusters.info.list <- clusters.kmedoids$clustering
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









