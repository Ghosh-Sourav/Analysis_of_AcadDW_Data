

no.of.clusters <- optimal.no.of.clusters

clusters.pam <- pam(x = subject.distance.matrix, diss = TRUE, k = no.of.clusters)

# Visualize pam clusters : NOT WORKING
fviz_cluster(clusters.pam, data = subject.similarity.matrix.score, stand = TRUE, geom = c("point", "text"),
             ellipse = TRUE, ellipse.type = "norm")
