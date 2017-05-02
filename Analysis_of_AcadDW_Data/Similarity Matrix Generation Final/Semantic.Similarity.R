#!/usr/bin/Rscript
# SYNTAX:
# ./Semantic.Similarity.R {value.for.step} {file.to.load} {i} {pass} >> {value.for.step}.log

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 4) {
  stop("4 args reqd.", call.=FALSE)
}

# setwd("/home/mtech/15CS60R16/Semantic Similarity/Script")
setwd("/home/mt1/15CS60R16/HadoopMachineDump/Semantic Similarity/Script")

#     *** Semantic Similarity : BEGIN ***
library(httr)
#set_config(use_proxy(url="http://10.3.100.207",port=8080))
Sys.unsetenv("http_proxy")

url <- "http://localhost:8080/StsService/GetStsSim"
url.old <- "http://swoogle.umbc.edu/StsService/GetStsSim"
url2 <- "http://swoogle.umbc.edu/SimService/GetSimilarity"

calc.sim <- function(p1, p2) {
  if(nchar(p1)+nchar(p2) <= 6000) {
    response <- POST(url,
                    query = list(
                      operation = "api",
                      phrase1 = p1,
                      phrase2 = p2
                    )
  )
  as.double(gsub("\r\n","",content(response, as = "text")))
  } else {
   print(paste("size too long = ", (nchar(p1)+nchar(p2)) ))
   NA 
  }
}
#     *** Semantic Similarity : END ***

# Read CSV file
csv.syllabus <- read.csv2(file = "../Data/Syllabus.csv",
                          header = TRUE, sep = "|", na.strings = "#@@^@^DUMMYNOTAPPLICABLE@!$%!$")
# Derive full syllabus
csv.syllabus$full.syllabus <- paste(
                                csv.syllabus$subno,
                                csv.syllabus$subname,
                                csv.syllabus$prereq,
                                csv.syllabus$syllabus,
                                sep = " \n "
                              )
csv.syllabus$subno <- as.character(csv.syllabus$subno)
csv.syllabus$syllabus <- as.character(csv.syllabus$syllabus)

# Inititalise similarity matrix
subject.similarity.matrix <- matrix(nrow = nrow(csv.syllabus), ncol = nrow(csv.syllabus))
rownames(subject.similarity.matrix) <- colnames(subject.similarity.matrix) <- csv.syllabus$subno

for(i in 1:nrow(subject.similarity.matrix)) {
  subject.similarity.matrix[i, i] <- 1.0
}

value.for.step <- as.integer(args[1])
#load("~/Semantic Similarity/Script/subject.similarity.matrix.1.230.RData")
load(args[2])
i <- as.integer(args[3])
pass <- as.integer(args[4])

calc.loop <- function(step) {
  for(i in i:nrow(subject.similarity.matrix)) {
    if(i>230 || i%%10==0) {
      save(subject.similarity.matrix, file=paste("ssm.step.",step,".pass.",pass,".i.",sprintf("%04d",i),".RData", sep=""))
    }
    if(i%%8==step) {
      for(j in 1:(i-1)) {
        if(j>0) {
          print(paste("step = ",step,", pass =", pass, ", i =", i, ", j =", j))
          syll.1 <- csv.syllabus$full.syllabus[i]
          syll.2 <- csv.syllabus$full.syllabus[j]
          # print(paste("length of syll.1 = ", length(syll.1), ", length of syll.2 = ", length(syll.2)))
          if(is.na(subject.similarity.matrix[i,j])) {
            sim.val <- try(calc.sim(syll.1, syll.2))
            if(!inherits(sim.val, "try-error"))  {
              subject.similarity.matrix[i,j] <- subject.similarity.matrix[j,i] <- sim.val
              print(subject.similarity.matrix[i,j])
            } else {
              print("Sleeping for 5 minutes...")
              Sys.sleep(5*60)
            }
          } else {
            print(paste(subject.similarity.matrix[i,j], "exists!"))
          }
        }
      }
    }
  }
}



while (sum(is.na(subject.similarity.matrix)) > 0) {
  calc.loop(value.for.step)
  pass <- pass + 1
}


# calc.loop.rev <- function() {
#   for(i in nrow(subject.similarity.matrix):i) {
#     if(i>230 || i%%10==0) {
#       save(subject.similarity.matrix, file=paste("subject.similarity.matrix.rev.",pass,".",i,".RData", sep=""))
#     }
#     for(j in 1:(i-1)) {
#       if(j>0) {
#         print(paste("pass =", pass, ",i =", i, ",j =", j))
#         syll.1 <- csv.syllabus$full.syllabus[i]
#         syll.2 <- csv.syllabus$full.syllabus[j]
#         if(is.na(subject.similarity.matrix[i,j])) {
#           sim.val <- try(calc.sim(syll.1, syll.2))
#           if(!inherits(sim.val, "try-error"))  {
#             subject.similarity.matrix[i,j] <- subject.similarity.matrix[j,i] <- sim.val
#             print(subject.similarity.matrix[i,j])
#           } else {
#             print("Not Sleeping for 2 minutes...")
#             #Sys.sleep(120)
#           }
#         } else {
#           print(paste(subject.similarity.matrix[i,j], "exists!"))
#         }
#       }
#     }
#   }
# }
# 
# while (sum(is.na(subject.similarity.matrix)) > 0) {
#   calc.loop.rev()
# }

# df.syllabus.compare <- data.frame(subno1=character(), subno2=character(), syllabus1=character(), syllabus2=character())
#
# for(i in 1:nrow(csv.syllabus)) {
#   for(j in 1:(i-1)) {
#     if(j>0) {
#       print(paste(csv.syllabus$subno[i], "x", csv.syllabus$subno[j]))
#       rbind(df.syllabus.compare,
#             c(
#               csv.syllabus$subno[i],
#               csv.syllabus$subno[j],
#               csv.syllabus$full.syllabus[i],
#               csv.syllabus$full.syllabus[i]
#             )
#       )
#     }
#   }
# }

