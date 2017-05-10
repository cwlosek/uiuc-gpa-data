library(shiny)
library(readxl)

# Grade Distribution Data File
# 2nd itteration

data("data1", package = "uiucGrades")
fs1516 = data1
fs1516 = fs1516[,-1]

colnames(fs1516)[3:16] = c("Course Title",
                           "A+", "A", "A-",
                           "B+", "B", "B-",
                           "C+", "C", "C-",
                           "D+", "D", "D-", "F")

fs1516$Subject = as.character(fs1516$Subject)
fs1516$`Course Title` = as.character(fs1516$`Course Title`)

# Professor Data File

data("data2", package = "uiucGrades")
fs1516_prof = data2
fs1516_prof = fs1516_prof[,-1]

colnames(fs1516_prof)[3:17] = c("Course Title", "Primary Instructor",
                                "A+", "A", "A-",
                                "B+", "B", "B-",
                                "C+", "C", "C-",
                                "D+", "D", "D-", "F")

fs1516_prof$Subject = as.character(fs1516_prof$Subject)
fs1516_prof$`Course Title` = as.character(fs1516_prof$`Course Title`)
fs1516_prof$FirstLast = as.character(fs1516_prof$FirstLast)

# Geneds Data Frames

gedIndex = rowSums(fs1516[, 19:26]) > 0L
index = which(gedIndex)

geneds = fs1516[index,]
geneds[, 19:26] = ifelse(geneds[,19:26] == 1, "Yes", "No")
genedsTable = cbind(geneds[, c(1, 2, 3, 18, 27, 19, 20, 22, 23, 24, 25, 26)])



longNames = c("Percentage of 4.00's",
              "Advanced Composition",
              "Non-Western Culture",
              "Western-Comparative Culture",
              "Humanities & the Arts",
              "Natural Science & Technology",
              "Quantitative Reasoning",
              "Social & Behavioral Sciences")


shortNames = c("% of 4.00's", "ACP", "CNW", "CW",
               "HUM", "NAT", "QR", "SBS")

colnames(genedsTable)[5:12] = longNames

# Code for use later

barplotGradeNames = c("F", "D-", "D", "D+",
                      "C-", "C", "C+",
                      "B-", "B", "B+",
                      "A-", "A", "A+")
