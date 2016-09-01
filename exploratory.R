library(haven)
library(dplyr)

setwd("./FARS2014NationalSAS")


acc <- read_sas("accident.sas7bdat")
dim(acc)
head(acc)

