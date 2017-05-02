
# This file is to look for support/confidence for rules such that:
#   d1 -> d2 (support: s%, confidence: c%)
#   ex.:
#   CS -> EC (s: s%, c: c%)

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Association Rule Mining")

top.dept.cluster.matrix <- matrix(nrow = length(dept.list.unique), ncol = no.of.clusters, data = FALSE)
rownames(top.dept.cluster.matrix) <- dept.list.unique
colnames(top.dept.cluster.matrix) <- paste("", 1:no.of.clusters, sep = "")

for(each.cluster in 1:no.of.clusters) {
  top.depts <- as.character(clusters.feature.matrix[each.cluster, "top.depts"])
  top.depts <- unlist(strsplit(top.depts, split = ","))

  for (each.dept in top.depts) {
    top.dept.cluster.matrix[each.dept, each.cluster] <- TRUE
  }
}

# Support, confidence, and lift matrix description:
#   Rule: (a -> c)  : support = support.matrix[a, c]
#                   : confidence = confidence.matrix[a, c]
#                   : lift = lift.matrix[a, c] = confidence.matrix[a, c] / expected.confidence.list[c]


support.matrix <- matrix(nrow = length(dept.list.unique), ncol = length(dept.list.unique))
rownames(support.matrix) <- colnames(support.matrix) <- dept.list.unique

confidence.matrix <- matrix(nrow = length(dept.list.unique), ncol = length(dept.list.unique))
rownames(confidence.matrix) <- colnames(confidence.matrix) <- dept.list.unique

lift.matrix <- matrix(nrow = length(dept.list.unique), ncol = length(dept.list.unique))
rownames(lift.matrix) <- colnames(lift.matrix) <- dept.list.unique

expected.confidence.list <- numeric(length = length(dept.list.unique))
names(dept.list.unique) <- dept.list.unique

for (dept.c in names(dept.list.unique)) {
  expected.confidence.list[dept.c] <- (sum(top.dept.cluster.matrix[dept.c, ]) /
                                        (ncol(top.dept.cluster.matrix))) * 100
}

for (dept.a in rownames(support.matrix)) {
  for (dept.c in colnames(support.matrix)) {
    support.matrix[dept.a, dept.c] <- (sum(top.dept.cluster.matrix[dept.a, ] & top.dept.cluster.matrix[dept.c, ]) /
                                        (ncol(top.dept.cluster.matrix))) * 100
    confidence.matrix[dept.a, dept.c] <- (sum(top.dept.cluster.matrix[dept.a, ] & top.dept.cluster.matrix[dept.c, ]) /
                                            (sum(top.dept.cluster.matrix[dept.a, ]))) * 100
    lift.matrix[dept.a, dept.c] <- confidence.matrix[dept.a, dept.c] / expected.confidence.list[dept.c]
  }
}

# *** CLEANING NaN ROWS OF CONFIDENCE AND LIFT MATRIX : BEGIN ***

r.full.NaN <- apply(X = confidence.matrix, MARGIN = 1, FUN = function(x) all(is.nan(x)))
clean.confidence.matrix <- confidence.matrix[!r.full.NaN, ]

r.full.NaN <- apply(X = lift.matrix, MARGIN = 1, FUN = function(x) all(is.nan(x)))
clean.lift.matrix <- lift.matrix[!r.full.NaN, ]

# *** CLEANING NaN ROWS OF CONFIDENCE AND LIFT MATRIX : END ***


threshold.support <- 1#sum(support.matrix) / length(support.matrix)
threshold.confidence <- 50 # sum(clean.confidence.matrix) / length(clean.confidence.matrix)
threshold.lift <- 1 # max(1, sum(clean.lift.matrix) / length(clean.lift.matrix))


association.rules.manual.df <- data.frame(antecedent = character(),
                                          consequent = character(),
                                          support = numeric(),
                                          confidence = numeric(),
                                          lift = numeric(),
                                          stringsAsFactors = FALSE)

count <- 0
for (dept.a in rownames(support.matrix)) {
  for (dept.c in rownames(support.matrix)) {
    if (dept.a != dept.c &&
        support.matrix[dept.a, dept.c] >= threshold.support &&
        !is.nan(confidence.matrix[dept.a, dept.c]) &&
        confidence.matrix[dept.a, dept.c] >= threshold.confidence &&
        !is.nan(lift.matrix[dept.a, dept.c]) &&
        lift.matrix[dept.a, dept.c] >= threshold.lift
        ) {
      count <- count + 1
      print(paste("Rule ", count,": ", dept.a, "->", dept.c,
                  ", s =", support.matrix[dept.a, dept.c],
                  "%, c =", confidence.matrix[dept.a, dept.c],
                  "%, l =", lift.matrix[dept.a, dept.c],
                  "%"
                  )
            )
      association.rules.manual.df[nrow(association.rules.manual.df) + 1, ] <- list(dept.a,
                                                                           dept.c,
                                                                           support.matrix[dept.a, dept.c],
                                                                           confidence.matrix[dept.a, dept.c],
                                                                           lift.matrix[dept.a, dept.c]
                                                                           )
    }
  }
}

association.rules.manual.df <- association.rules.manual.df[order(-association.rules.manual.df$confidence), ] # Sorts in DESC order by confidence

write.csv(association.rules.manual.df, file = "association.rule.manual.df.csv", row.names = FALSE)

