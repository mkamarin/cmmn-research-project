# Script to run the CMMN survey analysis
#
library(knitr)
library(rmarkdown)

args <- commandArgs(trailingOnly = TRUE)
my.working.dir <- gsub("\\","/", as.character(args[1]), fixed = TRUE)
generated.path    <- gsub("\\","/", as.character(args[2]), fixed = TRUE)
data.path      <- gsub("\\","/", as.character(args[3]), fixed = TRUE)

setwd(generated.path)
print(paste("Working dir:",getwd()))

print(paste("Generated path:",generated.path))

print(paste("Data path:", data.path))

knit(paste0(my.working.dir,"/Basic-stats.Rnw"))
knit(paste0(my.working.dir,"/Results.Rnw"))
knit(paste0(my.working.dir,"/SLR-analysis.Rnw"))

# done
print("done")
