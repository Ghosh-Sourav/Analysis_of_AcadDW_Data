
# Read CSV file (na.strings needed to avoid dropping CSV dept. fields with "NA")
csv.syllabus <- read.csv2(file = "D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Data/Syllabus.csv",
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
rownames(csv.syllabus) <- csv.syllabus$subno


# Cleaning syllabus
source('D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 3/Analysis.CustomStopWords.R', echo=TRUE)
csv.syllabus$full.syllabus <- iconv(csv.syllabus$full.syllabus, "latin1", "ASCII", sub=" ")
csv.syllabus$full.syllabus <- tolower(csv.syllabus$full.syllabus)
csv.syllabus$full.syllabus <- removePunctuation(csv.syllabus$full.syllabus, preserve_intra_word_dashes = FALSE)
csv.syllabus$full.syllabus <- removeNumbers(csv.syllabus$full.syllabus)
custom.stopwords <- removePunctuation(tolower(custom.stopwords), preserve_intra_word_dashes = FALSE)
csv.syllabus$full.syllabus <- removeWords(csv.syllabus$full.syllabus, words = custom.stopwords)
# csv.syllabus$full.syllabus <- wordStem(csv.syllabus$full.syllabus)    # For stemming to root words
# Next line:        Remove first terms from full.syllabus as they contain trimmed subject code (dept. code),
#             and,  Remove other 1 or 2 character terms from full.syllabus
csv.syllabus$full.syllabus <- gsub(" *\\b[[:alpha:]]{1,2}\\b *"," ",csv.syllabus$full.syllabus) # remove 1-2 char words
csv.syllabus$full.syllabus <- gsub("\n|\t|\\s+"," ",csv.syllabus$full.syllabus)
