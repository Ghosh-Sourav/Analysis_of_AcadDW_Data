
library("lsa")
library("NLP")
library("tm")
library("sos")
library("stringr")
library("ngram")


setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 3")
custom.stopwords <- c()
source('D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 3/Analysis.CustomStopWords.R', echo=TRUE)

#########################################

subject_details <- read.csv("D:/OneDrive/Files/Study Materials/M.Tech/M.Tech Project/ERP Data/subject_details.csv", sep=";", comment.char="#", na.strings="")

# out of 5969 subjects, 2683 have syllabus text, 451 have pre_req, 2739 have at least either syllabus or pre_req

filtered_subject_details <- as.data.frame(subject_details[!is.na(subject_details$syllabus), c("subno", "offer_dept", "syllabus")])
rownames(filtered_subject_details) <- filtered_subject_details$subno
filtered_subject_details <- filtered_subject_details[,c("offer_dept", "syllabus")]

filtered_subject_details[,2] <- iconv(filtered_subject_details[,2], "latin1", "ASCII", sub=" ")
filtered_subject_details[,2] <- tolower(filtered_subject_details[,2])
filtered_subject_details[,2] <- removePunctuation(filtered_subject_details[,2], preserve_intra_word_dashes = FALSE)
custom.stopwords <- removePunctuation(tolower(custom.stopwords), preserve_intra_word_dashes = FALSE)

filtered_subject_details[,2] <- removeNumbers(filtered_subject_details[,2])
filtered_subject_details[,2] <- removeWords(filtered_subject_details[,2], words = custom.stopwords)
filtered_subject_details[,2] <- wordStem(filtered_subject_details[,2])


document.count <- nrow(filtered_subject_details)


s.list <- rownames(filtered_subject_details)

#s1.list <- c("fe56caf4578eb1f54402f868d77a24db","fb56fa706a28cc3b6a990e6b0d82d6bc")
#s2.list <- c("fb56fa706a28cc3b6a990e6b0d82d6bc","fe56caf4578eb1f54402f868d77a24db")

#s.list <- s.list[1:10]

clean.syllabus <- gsub("\n|\t|\\s+"," ",filtered_subject_details$syllabus)

N.for.ngram <- 1
source("For.ngram.R", echo = TRUE)
N.for.ngram <- 2
source("For.ngram.R", echo = TRUE)
N.for.ngram <- 3
#source("For.ngram.R", echo = TRUE)

source("For.dept.R", echo = TRUE)

