
# This file is to look for support/confidence for rules such that:
#   d1 -> d2 (support: s%, confidence: c%)
#   ex.:
#   CS -> EC (s: s%, c: c%)

library(arules)
library(arulesViz)
library(stringr)

setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Association Rule Mining")

transactions.df <- clusters.feature.matrix[ , c("cluster.no","top.depts")]
transactions.df[,"top.depts"] <- gsub("NA", "_N_A",transactions.df[,"top.depts"])
write.table(transactions.df, file = "transactions.tab", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)

txn <- read.transactions(file = "transactions.tab", format="basket", sep=",", cols = 1)


rules <-  apriori(txn, parameter = list(support = 0.02,
                                        confidence = 0.3,
                                        minlen = 2,
                                        maxlen = 2
                                        )
                  )
rules <- sort(rules, by=c("confidence", "lift", "support"))

inspect(rules)

association.rules.df <- as(rules, "data.frame")
association.rules.df$support <- association.rules.df$support * 100
association.rules.df$confidence <- association.rules.df$confidence * 100
association.rules.df$rules <- gsub("_N_A", "NA", association.rules.df$rules)
association.rules.df[, c("antecedent", "consequent")] <- str_split_fixed(association.rules.df$rules, " => ", 2)
association.rules.df <- association.rules.df[, c("antecedent", "consequent", "support", "confidence", "lift")]
association.rules.df <- association.rules.df[order(-association.rules.df$confidence, -association.rules.df$support, -association.rules.df$lift), ]

association.rules.df.alias <- association.rules.df

library(readr)
depts_35 <- read_csv("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Data/depts_35.csv", na = "_DUMMY_") # still no tosolved


for(i in 1:nrow(depts_35)){
  association.rules.df.alias$antecedent <- gsub(paste("\\{",depts_35[i,"code"],"\\}", sep=""), paste("\\{",depts_35[i,"alias"],"\\}", sep=""), association.rules.df.alias$antecedent)
  association.rules.df.alias$consequent <- gsub(paste("\\{",depts_35[i,"code"],"\\}", sep=""), paste("\\{",depts_35[i,"alias"],"\\}", sep=""), association.rules.df.alias$consequent)
}

write.csv(association.rules.df.alias, quote = FALSE, file = "association_rules.csv", row.names = FALSE)

# write.csv(association.rules.df, quote = FALSE,
#                                 file = paste("association.rules.df.",
#                                              optimal.no.of.clusters,
#                                              ".",
#                                              threshold.percentage.for.top.dept,
#                                              ".csv",
#                                              sep=""),
#           row.names = FALSE)

