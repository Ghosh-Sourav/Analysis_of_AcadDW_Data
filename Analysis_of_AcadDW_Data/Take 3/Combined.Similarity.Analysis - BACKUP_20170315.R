
library("cluster")
library("reshape2")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 3")

# Documents determined similar by bigram approach would also be determined as similar by unigram.
# So, bigram can be given qual importance as unigram.
# The resultant score would anyway get increased due to ~3 counts per bigram match.
#
# subject.similarity.matrix.score <- 0.5 * 0.6 * subject.similarity.matrix.n1 + 0.5 * 0.6 * subject.similarity.matrix.n2 + 0.0 * subject.similarity.matrix.n3 + 0.4 * subject.similarity.matrix.dept

# subject.similarity.matrix.score <- 0.5 * 1.0 * subject.similarity.matrix.n1 + 0.5 * 1.0 * subject.similarity.matrix.n2 + 0.0 * subject.similarity.matrix.n3 + 0.0 * subject.similarity.matrix.dept

# subject.similarity.matrix.score <- 1/3 * 1e-10 * subject.similarity.matrix.n1 + 1/3 * 1e-10 * subject.similarity.matrix.n2 + 1/3 * subject.similarity.matrix.n3 + (1.0 - 1e-10) * subject.similarity.matrix.dept

subject.similarity.matrix.score <- 1/3 * 1.0 * subject.similarity.matrix.n1 + 1/3 * 1.0 * subject.similarity.matrix.n2 + 1/3 * 1.0 * subject.similarity.matrix.n3 + 0.0 * subject.similarity.matrix.dept

#dummy score matrix
# subject.similarity.matrix.score <- matrix(nrow=20,ncol=20)
# for(x in 1:20) {
#   subject.similarity.matrix.score[x, x] <- 1
#   if(x<20) {
#     for(y in (x+1):20) {
#       val <- 1 / sample(x = 1:20, size = 1)
#       # if (val>=16) val <- Inf
#       subject.similarity.matrix.score[x, y] <- subject.similarity.matrix.score[y, x] <- val
#     }
#   }
# }

# *** ALIASING SUB AND DEPT NAMES : BEGIN ***

subject.list.original <- rownames(filtered_subject_details)  # subject names inherently unique
subject.list.alias <- paste("s", 1:length(subject.list.original), sep = "")
names(subject.list.alias) <- subject.list.original

dept.list.original.unique <- unique(paste(filtered_subject_details$offer_dept))
dept.list.alias.unique <- paste("d", 1:length(dept.list.original.unique), sep = "")
names(dept.list.alias.unique) <- dept.list.original.unique


for(sub.org in names(subject.list.alias)) {
  dept.org <- paste(filtered_subject_details[sub.org, 1])
  dept.ali <- dept.list.alias.unique[dept.org]
  subject.list.alias[sub.org] <- paste(dept.ali, subject.list.alias[sub.org], sep = ".")
}

# for Reverse lookup, use:
# ex. > subject.list.alias[subject.list.alias=="d25.s1566"]


rownames(subject.similarity.matrix.score) <- colnames(subject.similarity.matrix.score) <- subject.list.alias

# *** ALIASING SUB AND DEPT NAMES : END ***



# *** CLEANING NaN ROWS & COLUMNS : BEGIN ***

rc.full.NaN <- apply(X = subject.similarity.matrix.score, MARGIN = 1, FUN = function(x) all(is.na(x)))
subject.similarity.matrix.score <- subject.similarity.matrix.score[!rc.full.NaN, !rc.full.NaN]

# *** CLEANING NaN ROWS & COLUMNS : END ***


# Creating distance matrix
inverse.score <- 1/subject.similarity.matrix.score - 1
inverse.score[subject.similarity.matrix.score == 0] <- 10e+200
subject.distance.matrix <- as.dist(inverse.score)

# clusters.tree <- agnes(subject.distance.matrix)
clusters.tree <- hclust(subject.distance.matrix)


is.reqd.dendrogram <- FALSE
is.reqd.qgraph <- FALSE

if(is.reqd.dendrogram) {
  #png(filename = "clusterPlot.png",  width = 20, height = 20, units = 'in', res = 1500)
  #pdf(file = "~/InterDSA/clusterPlot_ngramless.pdf",  width = 200, height = 20, pointsize = 2)
  pdf(file = "clusterPlot_deptless_u33b33t33.pdf",  width = 200, height = 20, pointsize = 2)
  plot(as.dendrogram(clusters.tree))
  dev.off()
}

if(is.reqd.qgraph) {
  library(qgraph)
  pdf(file = "qgraph_deptless_u33b33t33_count400.pdf",  width = 200, height = 200, pointsize = 2)
  # qgraph(subject.similarity.matrix.score[1:50,1:50], layout='spring', vsize=3)
  # qgraph(subject.similarity.matrix.score[1:400,1:400], layout='spring',  minimum=1e-2, maximum=1, details=TRUE)
  qgraph(subject.similarity.matrix.score[1:400,1:400], layout='spring',  details=TRUE)
  # use standalone=TRUE/FALSE and filetype="tex" for LaTeX usage
  dev.off()
}

# temp.max<-0
# i.max<-0
# for(i in 1:2677){
#   temp.values <- sort(unique(subject.similarity.matrix.score[i,]))
#   if(temp.values[length(temp.values)-2]>temp.max) {
#     temp.max <- temp.values[length(temp.values)-2]
#     i.max <- i
#     print(paste("--",i))
#   }
# }
# temp.max
# temp.x<- sort(subject.similarity.matrix.score[i.max,])
# length(temp.x)
# temp.x[length(temp.x)-1]
# View(subject.similarity.matrix.score[i.max:i.max+1, temp.x[length(temp.x)]])


no.of.clusters <- length(dept.list.alias.unique)
clusters.info.list <- cutree(clusters.tree, k = no.of.clusters)
# View(table(clusters.info))
dept.sub.split <- colsplit(names(clusters.info.list), pattern = "\\.", names = c("dept","sub"))
clusters.info.df <- data.frame(dept =  dept.sub.split$dept, sub = dept.sub.split$sub, cluster = clusters.info.list)

dept.cluster.matrix <- matrix(nrow = length(dept.list.alias.unique), ncol = no.of.clusters)
rownames(dept.cluster.matrix) <- dept.list.alias.unique
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

count <- 0
list.of.subjects <- c()
for(each.subject in rownames(clusters.info.df[clusters.info.df$cluster==7,])){
  print(each.subject)
  each.subject <- names(subject.list.alias[subject.list.alias==each.subject])[1]
  print(each.subject)
  list.of.subjects <- c(list.of.subjects, each.subject)
  count<- count+1
}

temp.filtered_subject_details <- filtered_subject_details[list.of.subjects, ]
temp.filtered_subject_details$sub_alias <- c(1:138)

for(each.subject in rownames(temp.filtered_subject_details)) {
  print(each.subject)
  temp.filtered_subject_details[each.subject, "sub_alias"] <- subject.list.alias[each.subject]
}

write.csv2(temp.filtered_subject_details[, c(1,3,2)], file = "subjects.of.cluster.7.csv", sep = ",")



