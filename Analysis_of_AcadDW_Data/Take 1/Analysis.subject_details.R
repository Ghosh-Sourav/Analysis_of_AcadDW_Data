
library("lsa")
library("NLP")
library("tm")
library("sos")
library("stringr")
library("ngram")


setwd("D:/Analysis Bin")
custom.stopwords <- c()
source('D:/OneDrive/Files/Study Materials/M.Tech/Summer Project/Projects/DataAnalyis/Analysis.CustomStopWords.R', echo=TRUE)

#########################################

subject_details <- read.csv("D:/OneDrive/Files/Study Materials/M.Tech/M.Tech Project/ERP Data/subject_details.csv", sep=";", comment.char="#", na.strings="")

# out of 5969 subjects, 2683 have syllabus text, 451 have pre_req, 2739 have at least either syllabus or pre_req

filtered_subject_details <- as.data.frame(subject_details[!is.na(subject_details$syllabus), c("subno", "syllabus")])
rownames(filtered_subject_details) <-filtered_subject_details$subno

filtered_subject_details[,2] <- iconv(filtered_subject_details[,2], "latin1", "ASCII", sub=" ")
filtered_subject_details[,2] <- tolower(filtered_subject_details[,2])
filtered_subject_details[,2] <- removePunctuation(filtered_subject_details[,2], preserve_intra_word_dashes = FALSE)
custom.stopwords <- removePunctuation(tolower(custom.stopwords), preserve_intra_word_dashes = FALSE)

filtered_subject_details[,2] <- removeNumbers(filtered_subject_details[,2])
filtered_subject_details[,2] <- removeWords(filtered_subject_details[,2], words = custom.stopwords)
filtered_subject_details[,2] <- wordStem(filtered_subject_details[,2])


document.count <- nrow(filtered_subject_details)

vocabulary <- c()

for(syllabus in filtered_subject_details$syllabus) {
  syllabus <- gsub("\n|\t", " ", syllabus)
  new.ngrams <- get.ngrams(ngram(syllabus, n=1))
  vocabulary <- c(vocabulary, new.ngrams)
}

vocabulary <- sort(unique(vocabulary))

#vocabulary <- unique(sort(unlist(strsplit(content, " |\n|\t"))))

if(vocabulary[1]=="") vocabulary <- vocabulary[-1]

write(vocabulary, file = "vocabulary")

utf <- matrix(nrow = length(filtered_subject_details[,2]),
             ncol = length(vocabulary))
ntf <- matrix(nrow = length(filtered_subject_details[,2]),
       ncol = length(vocabulary))
tf.idf <- matrix(nrow = length(filtered_subject_details[,2]),
              ncol = length(vocabulary))

colnames(tf.idf) <- colnames(ntf) <- colnames(utf) <- strsplit(vocabulary," ")
rownames(tf.idf) <- rownames(ntf) <- rownames(utf) <- filtered_subject_details[,1]


for(term in vocabulary) {
  #print(paste("[Calc Unnorm-TF] Processing: ", term, "..."))
  utf[,term] <- str_count(filtered_subject_details$syllabus, term)
}

for(document in filtered_subject_details$subno) {
  print(paste("[Calc Norm-TF] Processing: ", document, "..."))
  ntf[document,] <- utf[document,] / str_count( filtered_subject_details[filtered_subject_details$subno==document,]$syllabus, "\\S+")
}

idf <- vector(mode = "numeric", length = length(vocabulary))
names(idf) <- vocabulary

for(term in vocabulary) {
  print(paste("[Calc IDF & TF.IDF] Processing: ", term, "..."))
  no.of.docs.having.term <- sum(utf[,term]>0, na.rm = TRUE)
  idf[term] <- 1 + log(document.count / no.of.docs.having.term)
  tf.idf[,term] <- ntf[,term] * idf[term]
}

subject.similary.matrix <- matrix(nrow = document.count, ncol = document.count)
colnames(subject.similary.matrix) <- filtered_subject_details$subno
rownames(subject.similary.matrix) <- filtered_subject_details$subno

s1.list <- rownames(subject.similary.matrix)
s2.list <- rownames(subject.similary.matrix)

#s1.list <- c("fe56caf4578eb1f54402f868d77a24db")
#s2.list <- c("fb56fa706a28cc3b6a990e6b0d82d6bc")

count <- 0
for(s1 in s1.list) {
  for(s2 in s2.list) {
    if (s1 <= s2) {
      dot.product <- 0
      s1.mod.squared <- 0
      s2.mod.squared <- 0
      for(term in vocabulary) {
        print(paste("[Calc CosSim] Processing: ", (count/7198489*100), "%", s1, "&", s2, term, "... "))
        dot.product <- dot.product + tf.idf[s1,term] * tf.idf[s2,term]
        s1.mod.squared <- s1.mod.squared + tf.idf[s1,term]*tf.idf[s1,term]
        s2.mod.squared <- s2.mod.squared + tf.idf[s2,term]*tf.idf[s2,term]
      }
      s1.mod <- sqrt(s1.mod.squared)
      s2.mod <- sqrt(s2.mod.squared)

      subject.similary.matrix[s1,s2] <- subject.similary.matrix[s2,s1] <- dot.product / (s1.mod *s2.mod)

      if(s1==s2)
        count <- count + 1
      if(s1<s2)
        count <- count + 2
    }
  }
}

save.image("cossim.complete.RData")




