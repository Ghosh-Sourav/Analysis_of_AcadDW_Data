

# Needed:
#   clusters.info.df
#

threshold.percentage.for.top.dept <- 10
#max.for.top.depts <- 10

library("tm")
library("ngram")
library("NLP")

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Cluster Nomenclature")

# Initialise clusters.feature.matrix
clusters.feature.matrix <- matrix(nrow = no.of.clusters, ncol = 4)
colnames(clusters.feature.matrix) <- c("cluster.no", "top.depts", "top.terms.not.uniquely.taken", "top.terms.uniquely.taken")
clusters.feature.matrix[,"cluster.no"] <- 1:no.of.clusters

source("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Read CSV Data/ReadAndClean.Syllabus.from.CSV.R", echo = TRUE)


# Populate clusters.feature.matrix
for(each.cluster in 1:no.of.clusters) {
  subjects.in.this.cluster <- as.character(clusters.info.df[clusters.info.df$cluster==each.cluster,"sub"])
  bag.of.depts <- c()
  bag.of.unique.words <- c()
  bag.of.words <- c()
  for(each.subject in subjects.in.this.cluster) {
    print(paste("cluster.no =", each.cluster, ", subject =", each.subject))
    dept.from.subject <- as.character(csv.syllabus[each.subject,"dept_code"])
    # unique.words.from.subject <- sort(get.ngrams(ngram(csv.syllabus[each.subject,"full.syllabus"], n = 1))) # same outcome w/ unique
    words.from.subject <- sort(strsplit(csv.syllabus[each.subject,"full.syllabus"], split = "\\s+")[[1]])
    unique.words.from.subject <- unique(words.from.subject)
    bag.of.depts <- c(bag.of.depts, dept.from.subject)
    bag.of.words <- c(bag.of.words, words.from.subject)
    bag.of.unique.words <- c(bag.of.unique.words, unique.words.from.subject)
  }
  bag.of.words <- bag.of.words[bag.of.words!=""]
  bag.of.unique.words <- bag.of.unique.words[bag.of.unique.words!=""]

  # for dept
  dept.freq <- integer(length = length(unique(bag.of.depts)))
  names(dept.freq) <- sort(unique(bag.of.depts))
  for(each.dept in names(dept.freq)) {
    dept.freq[each.dept] <- sum(bag.of.depts==each.dept)
  }
  dept.freq <- rev(sort(dept.freq))
  # if (length(dept.freq) >= 3) {
  #   clusters.feature.matrix[each.cluster, "top.depts"] <- gsub (as.String(names(dept.freq[1:3])),
  #                                                               pattern = "\n", replacement = ",")
  # } else if (length(dept.freq) == 2) {
  #   clusters.feature.matrix[each.cluster, "top.depts"] <- paste(gsub (as.String(names(dept.freq[1:2])),
  #                                                                     pattern = "\n", replacement = ","),
  #                                                               ",-", sep = "")
  # } else {
  #   clusters.feature.matrix[each.cluster, "top.depts"] <- paste(gsub (as.String(names(dept.freq[1])),
  #                                                                     pattern = "\n", replacement = ","),
  #                                                               ",-,-", sep = "")
  # }
  # selected.top.depts <- gsub (as.String(names(dept.freq[1:min(length(dept.freq),
  #                                                             max.for.top.depts)])),
  #                             pattern = "\n", replacement = ",")
  dept.freq <- (dept.freq / sum(dept.freq)) * 100   # stores percentage freq.
  selected.top.depts <- gsub (as.String(names(dept.freq[dept.freq >= threshold.percentage.for.top.dept])),
                              pattern = "\n", replacement = ",")
  print(selected.top.depts)
  clusters.feature.matrix[each.cluster, "top.depts"] <- as.character(selected.top.depts)

   # for non-uniquely taken bag
  term.freq <- integer(length = length(unique(bag.of.words)))
  names(term.freq) <- sort(unique(bag.of.words))
  count <- 0
  max.count <- length(term.freq)
  for(each.term in names(term.freq)) {
    count <- count + 1
    print(paste("cluster.no =", each.cluster, "Calc term.freq for ", each.term, count, "(", (count/max.count)*100, "% )"))
    term.freq[each.term] <- sum(bag.of.words==each.term)
    if (nchar(each.term) > 45) {
      term.freq[each.term] <- 0
      print(paste("Dropping ", each.term))
    }
  }
  term.freq <- rev(sort(term.freq))
  clusters.feature.matrix[each.cluster, "top.terms.not.uniquely.taken"] <- gsub (as.String(names(term.freq[term.freq>0][1:20])),
                                                                                 pattern = "\n", replacement = ",")
  # for uniquely taken bag
  term.freq <- integer(length = length(unique(bag.of.unique.words)))
  names(term.freq) <- sort(unique(bag.of.unique.words))
  count <- 0
  max.count <- length(term.freq)
  for(each.term in names(term.freq)) {
    count <- count + 1
    print(paste("cluster.no =", each.cluster, "Calc term.freq for ", each.term, count, "(", (count/max.count)*100, "% )"))
    term.freq[each.term] <- sum(bag.of.unique.words==each.term)
    if (nchar(each.term) > 45) {
      term.freq[each.term] <- 0
      print(paste("Dropping ", each.term))
    }
  }
  term.freq <- rev(sort(term.freq))
  clusters.feature.matrix[each.cluster, "top.terms.uniquely.taken"] <- gsub (as.String(names(term.freq[term.freq>0][1:20])),
                                                                             pattern = "\n", replacement = ",")
}


temp.clusters.feature.matrix <- clusters.feature.matrix
temp.clusters.feature.matrix[,3:4] <- gsub(",NA","",temp.clusters.feature.matrix[,3:4])
#temp.clusters.feature.matrix[,3:4] <- paste(substr(temp.clusters.feature.matrix[,3:4], 0, 45), "..", sep = "")
temp.clusters.feature.matrix[,2:4] <- gsub(",",", ",temp.clusters.feature.matrix[,2:4])
colnames(temp.clusters.feature.matrix) <- gsub("\\.","",colnames(temp.clusters.feature.matrix))
write.csv2(temp.clusters.feature.matrix, file = "clusters_feature_matrix.csv", row.names = FALSE, quote = FALSE)

