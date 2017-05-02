
source("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Read CSV Data/ReadAndClean.Syllabus.from.CSV.R", echo = TRUE)


load("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Merge/Backup_20170325_Final/subject.similarity.matrix.final.RData")
load("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Cossim SemSim Comparison/saved.subject.similary.matrix.n1.RData")
load("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Cossim SemSim Comparison/saved.subject.similary.matrix.n2.RData")
load("D:/OneDrive/Files/Study Materials/M.Tech/Semester 4/Workspace/Analysis Bin/Document Similarity Matrix/Cossim SemSim Comparison/saved.subject.similary.matrix.n3.RData")

ssm.n1 <- subject.similary.matrix.n1
ssm.n123 <- (1/3)*subject.similary.matrix.n1 + (1/3)*subject.similary.matrix.n2 + (1/3)*subject.similary.matrix.n3
ssm.sem <- subject.similarity.matrix


sub.list <- sort(colnames(ssm.sem)) # common for all

sub.chosen <- sort(unique(c("CS60007", base::sample(sub.list[sub.list!="CS60007"], 9))))




comp.df <- data.frame(subj = character(),
                      unig = character(),
                      cosi = character(),
                      sema = character(),
                      stringsAsFactors = FALSE
                      )

simpleCap <- function(x) {
  x <- gsub(",","",x)
  x <- tolower(x)
  s <- strsplit(x, "-| ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


for(sub in sub.chosen) {

  val.sub <- paste(simpleCap(as.character(csv.syllabus[csv.syllabus$subno==sub, "subname"])), " (", sub, ")", sep="")

  val.n1 <- character()
  list.n1 <- names(rev(sort(ssm.n1[sub, ]))[2:6])
  for (matched.sub in list.n1) {
    cur.match <- paste(simpleCap(as.character(csv.syllabus[csv.syllabus$subno==matched.sub, "subname"])), " (", matched.sub, ")", sep="")
    if(length(val.n1) > 0) {
      val.n1 <- paste(val.n1, cur.match, sep = ", ")
    } else {
      val.n1 <- cur.match
    }
  }


  val.n123 <- character()
  list.n123 <- names(rev(sort(ssm.n123[sub, ]))[2:6])
  for (matched.sub in list.n123) {
    cur.match <- paste(simpleCap(as.character(csv.syllabus[csv.syllabus$subno==matched.sub, "subname"])), " (", matched.sub, ")", sep="")
    if(length(val.n123) > 0) {
      val.n123 <- paste(val.n123, cur.match, sep = ", ")
    } else {
      val.n123 <- cur.match
    }
  }


  val.sem <- character()
  list.sem <- names(rev(sort(ssm.sem[sub, ]))[2:6])
  for (matched.sub in list.sem) {
    cur.match <- paste(simpleCap(as.character(csv.syllabus[csv.syllabus$subno==matched.sub, "subname"])), " (", matched.sub, ")", sep="")
    if(length(val.sem) > 0) {
      val.sem <- paste(val.sem, cur.match, sep = ", ")
    } else {
      val.sem <- cur.match
    }
  }

  for(i in 1:5) {

    v.n1 <- strsplit(val.n1, ", ")[[1]][i]
    v.n123 <- strsplit(val.n123, ", ")[[1]][i]
    v.sem <- strsplit(val.sem, ", ")[[1]][i]

    val.sub <- gsub("&","and",val.sub)
    v.n1 <- gsub("&","and",v.n1)
    v.n123 <- gsub("&","and",v.n123)
    v.sem <- gsub("&","and",v.sem)

    val.sub <- gsub("Ii","II",val.sub)
    v.n1 <- gsub("Ii","II",v.n1)
    v.n123 <- gsub("Ii","II",v.n123)
    v.sem <- gsub("Ii","II",v.sem)

    # v.n1 <- paste(i,".", v.n1)
    # v.n123 <- paste(i,".", v.n123)
    # v.sem <- paste(i,".", v.sem)

    comp.df[nrow(comp.df)+1, ] <- list(as.character(val.sub), as.character(v.n1), as.character(v.n123), as.character(v.sem))
    comp.df[nrow(comp.df)+1, ] <- list(as.character(" "), as.character(" "), as.character(" "), as.character(" "))
    val.sub <- ""
  }
  comp.df[nrow(comp.df)+1, ] <- list(as.character(" "), as.character(" "), as.character(" "), as.character(" "))

}

#View(comp.df)
write.csv2(comp.df, file = "sim_comp.csv", row.names = FALSE, quote = FALSE)

