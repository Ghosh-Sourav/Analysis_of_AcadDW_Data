
library("lsa")
library("NLP")
library("tm")
library("sos")


setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Analysis Bin")

subject_details <- read.csv("D:/OneDrive/Files/Study Materials/M.Tech/M.Tech Project/ERP Data/subject_details.csv", sep=";", comment.char="#", na.strings="")

# out of 5969 subjects, 2683 have syllabus text, 451 have pre_req, 2739 have at least either syllabus or pre_req

filtered_subject_details <- as.data.frame(subject_details[!is.na(subject_details$syllabus), c("subno", "syllabus")])

#lapply(filtered_subject_details[,2], function(y){ sub(x=y, pattern = "\t", replacement = "")})
#lapply(filtered_subject_details[,2], function(y){ sub(x=y, pattern = "\n", replacement = "")})
filtered_subject_details[,2] <- tolower(filtered_subject_details[,2])
filtered_subject_details[,2] <- removeWords(filtered_subject_details[,2], words = stopwords())
filtered_subject_details[,2] <- removePunctuation(filtered_subject_details[,2], preserve_intra_word_dashes = TRUE)
filtered_subject_details[,2] <- removeNumbers(filtered_subject_details[,2])
filtered_subject_details[,2] <- wordStem(filtered_subject_details[,2])
filtered_subject_details[,2] <- iconv(filtered_subject_details[,2], "latin1", "ASCII", sub=" ")

#corpus <- Corpus(VectorSource(filtered_subject_details$syllabus))
#term.document.matrix <- TermDocumentMatrix(corpus)

document.count <- nrow(filtered_subject_details)

content <- ""

for(syllabus in filtered_subject_details$syllabus) {
  content <- paste(content, syllabus)
}

vocabulary <- unique(sort(unlist(strsplit(content, " |\n|\t|-"))))
if(vocabulary[1]=="") vocabulary <- vocabulary[-1]

write(vocabulary, file = "vocabulary")

