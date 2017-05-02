
# To drop clusters which contains very low or extremely high number of subjects compared to other clusters
#
# Needed:
#   clusters.feature.matrix
#

clusters.feature.matrix.unfiltered <- clusters.feature.matrix    # for backup

cluster.subcount.list <-  c()

for (each.cluster in 1:no.of.clusters) {
  cluster.subcount.list <- c(cluster.subcount.list, nrow(clusters.info.df[clusters.info.df$cluster==each.cluster,]))
}

cluster.subcount.list.summary <- summary(cluster.subcount.list)
cluster.subcount.IQR <- as.numeric(cluster.subcount.list.summary["3rd Qu."]) - as.numeric(cluster.subcount.list.summary["1st Qu."])
cluster.subcount.min <- max (20, as.numeric(cluster.subcount.list.summary["1st Qu."]) - 1.5 * cluster.subcount.IQR)
cluster.subcount.max <- as.numeric(cluster.subcount.list.summary["3rd Qu."]) + 1.5 * cluster.subcount.IQR

clusters.feature.matrix <- clusters.feature.matrix[
                                                  as.numeric(clusters.feature.matrix[,1])>=cluster.subcount.min
                                                  &
                                                  as.numeric(clusters.feature.matrix[,1])<=cluster.subcount.max
                                                  ,
                                                 ]

