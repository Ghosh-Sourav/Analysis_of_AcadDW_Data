
setwd("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Take 3")

#     *** Semantic Similarity : BEGIN ***
library(httr)
set_config(use_proxy(url="10.3.100.207",port=8080))

url <- "http://swoogle.umbc.edu/StsService/GetStsSim"
url2 <- "http://swoogle.umbc.edu/SimService/GetSimilarity"

calc.sim <- function(p1, p2) {
  if(nchar(p1)+nchar(p2) <= 2000) {
    response <- GET(url,
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



for(i in 1:10) {
  for(j in 1:(i-1)) {
    if(j>0) {
      print(paste("i =", i, "j =", j))
      syll.1 <- csv.syllabus$full.syllabus[i]
      syll.2 <- csv.syllabus$full.syllabus[j]
      subject.similarity.matrix[i,j] <- subject.similarity.matrix[j,i] <- calc.sim(syll.1, syll.2)
    }
  }
}

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


# Experiments for GET vs POST
# NA reason :
## "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>414 Request-URI Too Large</title>\n</head><body>\n<h1>Request-URI Too Large</h1>\n<p>The requested URL's length exceeds the capacity\nlimit for this server.<br />\n</p>\n<hr>\n<address>Apache/2.2.3 (Red Hat) Server at swoogle.umbc.edu Port 80</address>\n</body></html>\n"
## "<HTML>\n<HEAD>\n<TITLE>Inactivity Timeout</TITLE>\n</HEAD>\n\n<BODY BGCOLOR=\"white\" FGCOLOR=\"black\">\n<H1>Inactivity Timeout</H1>\n<HR>\n\n<FONT FACE=\"Helvetica,Arial\"><B>\nDescription: Too much time has passed without sending any data for document. \n</B></FONT>\n<HR>\n</BODY>\n"

# EXPR. 1
x <- csv.syllabus[nchar(csv.syllabus$full.syllabus)>3000, "subno"]
x.1 <- csv.syllabus[(csv.syllabus$subno)==x[1], "full.syllabus"]
x.2 <- csv.syllabus[(csv.syllabus$subno)==x[2], "full.syllabus"]
calc.sim(x.1, x.2) ## GIVING NA coerced in both cases: GET and POST
calc.sim(substr(x.1,1,2500), substr(x.2,1,2500)) ## Not GIVING NA coerced in both cases: GET and POST
calc.sim(substr(x.1,1,2750), substr(x.2,1,2750)) ## GIVING NA coerced in both cases: GET and POST
# EXPR. 2
x <- csv.syllabus[nchar(csv.syllabus$full.syllabus)>8000, "subno"]
x.1 <- csv.syllabus[(csv.syllabus$subno)==x[1], "full.syllabus"]
calc.sim(x.1, x.1) ## GIVING NA coerced in both cases: GET and POST
calc.sim(substr(x.1,1,2), substr(x.1,1,3000)) # Not giving NA
calc.sim(substr(x.1,1,2), substr(x.1,1,6000)) # Giving NA

summary(nchar(csv.syllabus$full.syllabus))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 42.0   536.0   772.0   918.2  1134.0  8916.0
boxplot(nchar(csv.syllabus$full.syllabus))
# shows around 2000 for cut off

