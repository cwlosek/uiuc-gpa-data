# Grade Distribution File

fs1517 = read.csv("https://www.dropbox.com/s/9tdukopjly2up2z/GPA%20DATA_fs1517.csv?raw=1")
fs1517 = fs1517[,-1]

colnames(fs1517)[3:16] = c("Course Title",
                           "A+", "A", "A-",
                           "B+", "B", "B-",
                           "C+", "C", "C-",
                           "D+", "D", "D-", "F")

fs1517$Subject = as.character(fs1517$Subject)
fs1517$`Course Title` = as.character(fs1517$`Course Title`)

# Professor Data File

fs1517_prof = read.csv("https://www.dropbox.com/s/rm6ey6k6z7wd9ts/GPA%20DATA_fs1517_prof.csv?raw=1")
fs1517_prof = fs1517_prof[,-1]

colnames(fs1517_prof)[3:17] = c("Course Title", "Primary Instructor",
                                "A+", "A", "A-",
                                "B+", "B", "B-",
                                "C+", "C", "C-",
                                "D+", "D", "D-", "F")

fs1517_prof$Subject = as.character(fs1517_prof$Subject)
fs1517_prof$`Course Title` = as.character(fs1517_prof$`Course Title`)
fs1517_prof$FirstLast = as.character(fs1517_prof$FirstLast)

# Geneds Data Frames

gedIndex = rowSums(fs1517[, 19:26]) > 0L
index = which(gedIndex)

geneds = fs1517[index,]
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

barplotGradeNames = c("F", "D-", "D", "D+",
                      "C-", "C", "C+",
                      "B-", "B", "B+",
                      "A-", "A", "A+")
