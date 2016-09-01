library(haven)
library(dplyr)

setwd("Users/alexengler/Desktop/DoT-Traffic/data/FARS2014NationalCSV")

acc <- read_sas("accident.sas7bdat")
dim(acc)
head(acc)

