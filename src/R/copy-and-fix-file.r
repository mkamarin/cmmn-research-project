# Script to copy and fix the CMMN survey file to the analysis directory
#
# This script copies the LimeSurvey result file (results-survey338792.csv) to a temporary input file (in-survey-data-file.csv).
# This is done to solve a LimeSurvey problem when using Answer codes with convert Y to 1 and N to 2.
# If you ask LimeSurvey to do the conversion (convert Y to 1 and N to 2), 2 will be used for both "N" and NA,
# so, we will be unable to distinguished between an "No" from a "No Answer".
#
# Input file:
#     results-survey338792.csv - file generated by LimeSurvey containing all the survey data
#
# Output file:
#     in-survey-data-file.csv - file required by CMMN-Convert-File.Rmd to generate a clean data set 
#

args <- commandArgs(trailingOnly = TRUE)
my.working.dir <- gsub("\\","/", as.character(args[1]), fixed = TRUE)
my.input.file  <- gsub("\\","/", as.character(args[2]), fixed = TRUE)

#print(paste("work dir  :", my.working.dir, "(", as.character(args[1]), ")"))
#print(paste("input file:", my.input.file,  "(", as.character(args[2]), ")"))


setwd(my.working.dir)
print(paste("Working dir:", getwd()))
print(paste("input file :", my.input.file))

# Read the cvs file generated by LimeSurvey
data <- read.csv(my.input.file, header = TRUE, sep = ",", quote = "'\"",
                 na.strings=c("", "\"\""), stringsAsFactors=FALSE)

#str(data, list.len = 400)

# Replace "Y" to 1 and "N" to 2.
# This may not be the mst efficient way to do this, but it works.
data$Consent[data$Consent=="Y"] <- 1
data$Consent[data$Consent=="N"] <- 2
data$Role.R1.[data$Role.R1.=="Y"] <- 1
data$Role.R1.[data$Role.R1.=="N"] <- 2
data$Role.R2.[data$Role.R2.=="Y"] <- 1
data$Role.R2.[data$Role.R2.=="N"] <- 2
data$Role.R3.[data$Role.R3.=="Y"] <- 1
data$Role.R3.[data$Role.R3.=="N"] <- 2
data$Role.R4.[data$Role.R4.=="Y"] <- 1
data$Role.R4.[data$Role.R4.=="N"] <- 2
data$Role.R5.[data$Role.R5.=="Y"] <- 1
data$Role.R5.[data$Role.R5.=="N"] <- 2
data$Role.R6.[data$Role.R6.=="Y"] <- 1
data$Role.R6.[data$Role.R6.=="N"] <- 2
data$Role.R7.[data$Role.R7.=="Y"] <- 1
data$Role.R7.[data$Role.R7.=="N"] <- 2
data$Role.R8.[data$Role.R8.=="Y"] <- 1
data$Role.R8.[data$Role.R8.=="N"] <- 2
data$Role.R9.[data$Role.R9.=="Y"] <- 1
data$Role.R9.[data$Role.R9.=="N"] <- 2
data$Role.R10.[data$Role.R10.=="Y"] <- 1
data$Role.R10.[data$Role.R10.=="N"] <- 2
data$Bias.B1.[data$Bias.B1.=="Y"] <- 1
data$Bias.B1.[data$Bias.B1.=="N"] <- 2
data$Bias.B2.[data$Bias.B2.=="Y"] <- 1
data$Bias.B2.[data$Bias.B2.=="N"] <- 2
data$Bias.B3.[data$Bias.B3.=="Y"] <- 1
data$Bias.B3.[data$Bias.B3.=="N"] <- 2
data$Bias.B4.[data$Bias.B4.=="Y"] <- 1
data$Bias.B4.[data$Bias.B4.=="N"] <- 2
data$Bias.B5.[data$Bias.B5.=="Y"] <- 1
data$Bias.B5.[data$Bias.B5.=="N"] <- 2
data$Bias.B6.[data$Bias.B6.=="Y"] <- 1
data$Bias.B6.[data$Bias.B6.=="N"] <- 2
data$Bias.B7.[data$Bias.B7.=="Y"] <- 1
data$Bias.B7.[data$Bias.B7.=="N"] <- 2
data$Bias.B8.[data$Bias.B8.=="Y"] <- 1
data$Bias.B8.[data$Bias.B8.=="N"] <- 2
data$Bias.B9.[data$Bias.B9.=="Y"] <- 1
data$Bias.B9.[data$Bias.B9.=="N"] <- 2
data$Bias.B10.[data$Bias.B10.=="Y"] <- 1
data$Bias.B10.[data$Bias.B10.=="N"] <- 2
data$Notation.None.[data$Notation.None.=="Y"] <- 1
data$Notation.None.[data$Notation.None.=="N"] <- 2
data$Notation.BPMN.[data$Notation.BPMN.=="Y"] <- 1
data$Notation.BPMN.[data$Notation.BPMN.=="N"] <- 2
data$Notation.EPC.[data$Notation.EPC.=="Y"] <- 1
data$Notation.EPC.[data$Notation.EPC.=="N"] <- 2
data$Notation.UMLAD.[data$Notation.UMLAD.=="Y"] <- 1
data$Notation.UMLAD.[data$Notation.UMLAD.=="N"] <- 2
data$Notation.UML.[data$Notation.UML.=="Y"] <- 1
data$Notation.UML.[data$Notation.UML.=="N"] <- 2
data$Notation.CMMN.[data$Notation.CMMN.=="Y"] <- 1
data$Notation.CMMN.[data$Notation.CMMN.=="N"] <- 2
data$Tutorial[data$Tutorial=="Y"] <- 1
data$Tutorial[data$Tutorial=="N"] <- 2
data$Experience[data$Experience=="Y"] <- 1
data$Experience[data$Experience=="N"] <- 2
data$Model1Q2a[data$Model1Q2a=="Y"] <- 1
data$Model1Q2a[data$Model1Q2a=="N"] <- 2
data$Model1Q3a[data$Model1Q3a=="Y"] <- 1
data$Model1Q3a[data$Model1Q3a=="N"] <- 2
data$Model1Q4a[data$Model1Q4a=="Y"] <- 1
data$Model1Q4a[data$Model1Q4a=="N"] <- 2
data$Model1Q5a.SQ001.[data$Model1Q5a.SQ001.=="Y"] <- 1
data$Model1Q5a.SQ001.[data$Model1Q5a.SQ001.=="N"] <- 2
data$Model1Q5a.SQ002.[data$Model1Q5a.SQ002.=="Y"] <- 1
data$Model1Q5a.SQ002.[data$Model1Q5a.SQ002.=="N"] <- 2
data$Model1Q5a.SQ003.[data$Model1Q5a.SQ003.=="Y"] <- 1
data$Model1Q5a.SQ003.[data$Model1Q5a.SQ003.=="N"] <- 2
data$Model1Q5a.SQ004.[data$Model1Q5a.SQ004.=="Y"] <- 1
data$Model1Q5a.SQ004.[data$Model1Q5a.SQ004.=="N"] <- 2
data$Model1Q5a.SQ005.[data$Model1Q5a.SQ005.=="Y"] <- 1
data$Model1Q5a.SQ005.[data$Model1Q5a.SQ005.=="N"] <- 2
data$Model1Q5a.SQ006.[data$Model1Q5a.SQ006.=="Y"] <- 1
data$Model1Q5a.SQ006.[data$Model1Q5a.SQ006.=="N"] <- 2
data$Model2Q2a[data$Model2Q2a=="Y"] <- 1
data$Model2Q2a[data$Model2Q2a=="N"] <- 2
data$Model2Q3a[data$Model2Q3a=="Y"] <- 1
data$Model2Q3a[data$Model2Q3a=="N"] <- 2
data$Model2Q4a[data$Model2Q4a=="Y"] <- 1
data$Model2Q4a[data$Model2Q4a=="N"] <- 2
data$Model2Q5a.SQ001.[data$Model2Q5a.SQ001.=="Y"] <- 1
data$Model2Q5a.SQ001.[data$Model2Q5a.SQ001.=="N"] <- 2
data$Model2Q5a.SQ002.[data$Model2Q5a.SQ002.=="Y"] <- 1
data$Model2Q5a.SQ002.[data$Model2Q5a.SQ002.=="N"] <- 2
data$Model2Q5a.SQ003.[data$Model2Q5a.SQ003.=="Y"] <- 1
data$Model2Q5a.SQ003.[data$Model2Q5a.SQ003.=="N"] <- 2
data$Model2Q5a.SQ004.[data$Model2Q5a.SQ004.=="Y"] <- 1
data$Model2Q5a.SQ004.[data$Model2Q5a.SQ004.=="N"] <- 2
data$Model2Q5a.SQ005.[data$Model2Q5a.SQ005.=="Y"] <- 1
data$Model2Q5a.SQ005.[data$Model2Q5a.SQ005.=="N"] <- 2
data$Model2Q5a.SQ006.[data$Model2Q5a.SQ006.=="Y"] <- 1
data$Model2Q5a.SQ006.[data$Model2Q5a.SQ006.=="N"] <- 2
data$Model3Q2a[data$Model3Q2a=="Y"] <- 1
data$Model3Q2a[data$Model3Q2a=="N"] <- 2
data$Model3Q3a[data$Model3Q3a=="Y"] <- 1
data$Model3Q3a[data$Model3Q3a=="N"] <- 2
data$Model3Q4a[data$Model3Q4a=="Y"] <- 1
data$Model3Q4a[data$Model3Q4a=="N"] <- 2
data$Model3Q5a.SQ001.[data$Model3Q5a.SQ001.=="Y"] <- 1
data$Model3Q5a.SQ001.[data$Model3Q5a.SQ001.=="N"] <- 2
data$Model3Q5a.SQ002.[data$Model3Q5a.SQ002.=="Y"] <- 1
data$Model3Q5a.SQ002.[data$Model3Q5a.SQ002.=="N"] <- 2
data$Model3Q5a.SQ003.[data$Model3Q5a.SQ003.=="Y"] <- 1
data$Model3Q5a.SQ003.[data$Model3Q5a.SQ003.=="N"] <- 2
data$Model3Q5a.SQ004.[data$Model3Q5a.SQ004.=="Y"] <- 1
data$Model3Q5a.SQ004.[data$Model3Q5a.SQ004.=="N"] <- 2
data$Model3Q5a.SQ005.[data$Model3Q5a.SQ005.=="Y"] <- 1
data$Model3Q5a.SQ005.[data$Model3Q5a.SQ005.=="N"] <- 2
data$Model3Q5a.SQ006.[data$Model3Q5a.SQ006.=="Y"] <- 1
data$Model3Q5a.SQ006.[data$Model3Q5a.SQ006.=="N"] <- 2
data$Model4Q2a[data$Model4Q2a=="Y"] <- 1
data$Model4Q2a[data$Model4Q2a=="N"] <- 2
data$Model4Q3a[data$Model4Q3a=="Y"] <- 1
data$Model4Q3a[data$Model4Q3a=="N"] <- 2
data$Model4Q4a[data$Model4Q4a=="Y"] <- 1
data$Model4Q4a[data$Model4Q4a=="N"] <- 2
data$Model4Q5a.SQ001.[data$Model4Q5a.SQ001.=="Y"] <- 1
data$Model4Q5a.SQ001.[data$Model4Q5a.SQ001.=="N"] <- 2
data$Model4Q5a.SQ002.[data$Model4Q5a.SQ002.=="Y"] <- 1
data$Model4Q5a.SQ002.[data$Model4Q5a.SQ002.=="N"] <- 2
data$Model4Q5a.SQ003.[data$Model4Q5a.SQ003.=="Y"] <- 1
data$Model4Q5a.SQ003.[data$Model4Q5a.SQ003.=="N"] <- 2
data$Model4Q5a.SQ004.[data$Model4Q5a.SQ004.=="Y"] <- 1
data$Model4Q5a.SQ004.[data$Model4Q5a.SQ004.=="N"] <- 2
data$Model4Q5a.SQ005.[data$Model4Q5a.SQ005.=="Y"] <- 1
data$Model4Q5a.SQ005.[data$Model4Q5a.SQ005.=="N"] <- 2
data$Model4Q5a.SQ006.[data$Model4Q5a.SQ006.=="Y"] <- 1
data$Model4Q5a.SQ006.[data$Model4Q5a.SQ006.=="N"] <- 2
data$Model5Q2a[data$Model5Q2a=="Y"] <- 1
data$Model5Q2a[data$Model5Q2a=="N"] <- 2
data$Model5Q3a[data$Model5Q3a=="Y"] <- 1
data$Model5Q3a[data$Model5Q3a=="N"] <- 2
data$Model5Q4a[data$Model5Q4a=="Y"] <- 1
data$Model5Q4a[data$Model5Q4a=="N"] <- 2
data$Model5Q5a.SQ001.[data$Model5Q5a.SQ001.=="Y"] <- 1
data$Model5Q5a.SQ001.[data$Model5Q5a.SQ001.=="N"] <- 2
data$Model5Q5a.SQ002.[data$Model5Q5a.SQ002.=="Y"] <- 1
data$Model5Q5a.SQ002.[data$Model5Q5a.SQ002.=="N"] <- 2
data$Model5Q5a.SQ003.[data$Model5Q5a.SQ003.=="Y"] <- 1
data$Model5Q5a.SQ003.[data$Model5Q5a.SQ003.=="N"] <- 2
data$Model5Q5a.SQ004.[data$Model5Q5a.SQ004.=="Y"] <- 1
data$Model5Q5a.SQ004.[data$Model5Q5a.SQ004.=="N"] <- 2
data$Model5Q5a.SQ005.[data$Model5Q5a.SQ005.=="Y"] <- 1
data$Model5Q5a.SQ005.[data$Model5Q5a.SQ005.=="N"] <- 2
data$Model5Q5a.SQ006.[data$Model5Q5a.SQ006.=="Y"] <- 1
data$Model5Q5a.SQ006.[data$Model5Q5a.SQ006.=="N"] <- 2
data$Model6Q2a[data$Model6Q2a=="Y"] <- 1
data$Model6Q2a[data$Model6Q2a=="N"] <- 2
data$Model6Q3a[data$Model6Q3a=="Y"] <- 1
data$Model6Q3a[data$Model6Q3a=="N"] <- 2
data$Model6Q4a[data$Model6Q4a=="Y"] <- 1
data$Model6Q4a[data$Model6Q4a=="N"] <- 2
data$Model6Q5a.SQ001.[data$Model6Q5a.SQ001.=="Y"] <- 1
data$Model6Q5a.SQ001.[data$Model6Q5a.SQ001.=="N"] <- 2
data$Model6Q5a.SQ002.[data$Model6Q5a.SQ002.=="Y"] <- 1
data$Model6Q5a.SQ002.[data$Model6Q5a.SQ002.=="N"] <- 2
data$Model6Q5a.SQ003.[data$Model6Q5a.SQ003.=="Y"] <- 1
data$Model6Q5a.SQ003.[data$Model6Q5a.SQ003.=="N"] <- 2
data$Model6Q5a.SQ004.[data$Model6Q5a.SQ004.=="Y"] <- 1
data$Model6Q5a.SQ004.[data$Model6Q5a.SQ004.=="N"] <- 2
data$Model6Q5a.SQ005.[data$Model6Q5a.SQ005.=="Y"] <- 1
data$Model6Q5a.SQ005.[data$Model6Q5a.SQ005.=="N"] <- 2
data$Model6Q5a.SQ006.[data$Model6Q5a.SQ006.=="Y"] <- 1
data$Model6Q5a.SQ006.[data$Model6Q5a.SQ006.=="N"] <- 2
data$Model1Q2b[data$Model1Q2b=="Y"] <- 1
data$Model1Q2b[data$Model1Q2b=="N"] <- 2
data$Model1Q3b[data$Model1Q3b=="Y"] <- 1
data$Model1Q3b[data$Model1Q3b=="N"] <- 2
data$Model1Q4b[data$Model1Q4b=="Y"] <- 1
data$Model1Q4b[data$Model1Q4b=="N"] <- 2
data$Model1Q5b.SQ001.[data$Model1Q5b.SQ001.=="Y"] <- 1
data$Model1Q5b.SQ001.[data$Model1Q5b.SQ001.=="N"] <- 2
data$Model1Q5b.SQ002.[data$Model1Q5b.SQ002.=="Y"] <- 1
data$Model1Q5b.SQ002.[data$Model1Q5b.SQ002.=="N"] <- 2
data$Model1Q5b.SQ003.[data$Model1Q5b.SQ003.=="Y"] <- 1
data$Model1Q5b.SQ003.[data$Model1Q5b.SQ003.=="N"] <- 2
data$Model1Q5b.SQ004.[data$Model1Q5b.SQ004.=="Y"] <- 1
data$Model1Q5b.SQ004.[data$Model1Q5b.SQ004.=="N"] <- 2
data$Model1Q5b.SQ005.[data$Model1Q5b.SQ005.=="Y"] <- 1
data$Model1Q5b.SQ005.[data$Model1Q5b.SQ005.=="N"] <- 2
data$Model1Q5b.SQ006.[data$Model1Q5b.SQ006.=="Y"] <- 1
data$Model1Q5b.SQ006.[data$Model1Q5b.SQ006.=="N"] <- 2
data$Model2Q2b[data$Model2Q2b=="Y"] <- 1
data$Model2Q2b[data$Model2Q2b=="N"] <- 2
data$Model2Q3b[data$Model2Q3b=="Y"] <- 1
data$Model2Q3b[data$Model2Q3b=="N"] <- 2
data$Model2Q4b[data$Model2Q4b=="Y"] <- 1
data$Model2Q4b[data$Model2Q4b=="N"] <- 2
data$Model2Q5b.SQ001.[data$Model2Q5b.SQ001.=="Y"] <- 1
data$Model2Q5b.SQ001.[data$Model2Q5b.SQ001.=="N"] <- 2
data$Model2Q5b.SQ002.[data$Model2Q5b.SQ002.=="Y"] <- 1
data$Model2Q5b.SQ002.[data$Model2Q5b.SQ002.=="N"] <- 2
data$Model2Q5b.SQ003.[data$Model2Q5b.SQ003.=="Y"] <- 1
data$Model2Q5b.SQ003.[data$Model2Q5b.SQ003.=="N"] <- 2
data$Model2Q5b.SQ004.[data$Model2Q5b.SQ004.=="Y"] <- 1
data$Model2Q5b.SQ004.[data$Model2Q5b.SQ004.=="N"] <- 2
data$Model2Q5b.SQ005.[data$Model2Q5b.SQ005.=="Y"] <- 1
data$Model2Q5b.SQ005.[data$Model2Q5b.SQ005.=="N"] <- 2
data$Model2Q5b.SQ006.[data$Model2Q5b.SQ006.=="Y"] <- 1
data$Model2Q5b.SQ006.[data$Model2Q5b.SQ006.=="N"] <- 2
data$Model3Q2b[data$Model3Q2b=="Y"] <- 1
data$Model3Q2b[data$Model3Q2b=="N"] <- 2
data$Model3Q3b[data$Model3Q3b=="Y"] <- 1
data$Model3Q3b[data$Model3Q3b=="N"] <- 2
data$Model3Q4b[data$Model3Q4b=="Y"] <- 1
data$Model3Q4b[data$Model3Q4b=="N"] <- 2
data$Model3Q5b.SQ001.[data$Model3Q5b.SQ001.=="Y"] <- 1
data$Model3Q5b.SQ001.[data$Model3Q5b.SQ001.=="N"] <- 2
data$Model3Q5b.SQ002.[data$Model3Q5b.SQ002.=="Y"] <- 1
data$Model3Q5b.SQ002.[data$Model3Q5b.SQ002.=="N"] <- 2
data$Model3Q5b.SQ003.[data$Model3Q5b.SQ003.=="Y"] <- 1
data$Model3Q5b.SQ003.[data$Model3Q5b.SQ003.=="N"] <- 2
data$Model3Q5b.SQ004.[data$Model3Q5b.SQ004.=="Y"] <- 1
data$Model3Q5b.SQ004.[data$Model3Q5b.SQ004.=="N"] <- 2
data$Model3Q5b.SQ005.[data$Model3Q5b.SQ005.=="Y"] <- 1
data$Model3Q5b.SQ005.[data$Model3Q5b.SQ005.=="N"] <- 2
data$Model3Q5b.SQ006.[data$Model3Q5b.SQ006.=="Y"] <- 1
data$Model3Q5b.SQ006.[data$Model3Q5b.SQ006.=="N"] <- 2
data$Model4Q2b[data$Model4Q2b=="Y"] <- 1
data$Model4Q2b[data$Model4Q2b=="N"] <- 2
data$Model4Q3b[data$Model4Q3b=="Y"] <- 1
data$Model4Q3b[data$Model4Q3b=="N"] <- 2
data$Model4Q4b[data$Model4Q4b=="Y"] <- 1
data$Model4Q4b[data$Model4Q4b=="N"] <- 2
data$Model4Q5b.SQ001.[data$Model4Q5b.SQ001.=="Y"] <- 1
data$Model4Q5b.SQ001.[data$Model4Q5b.SQ001.=="N"] <- 2
data$Model4Q5b.SQ002.[data$Model4Q5b.SQ002.=="Y"] <- 1
data$Model4Q5b.SQ002.[data$Model4Q5b.SQ002.=="N"] <- 2
data$Model4Q5b.SQ003.[data$Model4Q5b.SQ003.=="Y"] <- 1
data$Model4Q5b.SQ003.[data$Model4Q5b.SQ003.=="N"] <- 2
data$Model4Q5b.SQ004.[data$Model4Q5b.SQ004.=="Y"] <- 1
data$Model4Q5b.SQ004.[data$Model4Q5b.SQ004.=="N"] <- 2
data$Model4Q5b.SQ005.[data$Model4Q5b.SQ005.=="Y"] <- 1
data$Model4Q5b.SQ005.[data$Model4Q5b.SQ005.=="N"] <- 2
data$Model4Q5b.SQ006.[data$Model4Q5b.SQ006.=="Y"] <- 1
data$Model4Q5b.SQ006.[data$Model4Q5b.SQ006.=="N"] <- 2
data$Model5Q2b[data$Model5Q2b=="Y"] <- 1
data$Model5Q2b[data$Model5Q2b=="N"] <- 2
data$Model5Q3b[data$Model5Q3b=="Y"] <- 1
data$Model5Q3b[data$Model5Q3b=="N"] <- 2
data$Model5Q4b[data$Model5Q4b=="Y"] <- 1
data$Model5Q4b[data$Model5Q4b=="N"] <- 2
data$Model5Q5b.SQ001.[data$Model5Q5b.SQ001.=="Y"] <- 1
data$Model5Q5b.SQ001.[data$Model5Q5b.SQ001.=="N"] <- 2
data$Model5Q5b.SQ002.[data$Model5Q5b.SQ002.=="Y"] <- 1
data$Model5Q5b.SQ002.[data$Model5Q5b.SQ002.=="N"] <- 2
data$Model5Q5b.SQ003.[data$Model5Q5b.SQ003.=="Y"] <- 1
data$Model5Q5b.SQ003.[data$Model5Q5b.SQ003.=="N"] <- 2
data$Model5Q5b.SQ004.[data$Model5Q5b.SQ004.=="Y"] <- 1
data$Model5Q5b.SQ004.[data$Model5Q5b.SQ004.=="N"] <- 2
data$Model5Q5b.SQ005.[data$Model5Q5b.SQ005.=="Y"] <- 1
data$Model5Q5b.SQ005.[data$Model5Q5b.SQ005.=="N"] <- 2
data$Model5Q5b.SQ006.[data$Model5Q5b.SQ006.=="Y"] <- 1
data$Model5Q5b.SQ006.[data$Model5Q5b.SQ006.=="N"] <- 2

# Save the fixed file to the correct location
write.csv(data,file = "in-survey-data-file.csv",row.names=FALSE, na="")

# done
print("done")
