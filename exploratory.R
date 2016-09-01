library(haven)
library(dplyr)
library(ggplot2)


setwd("/Users/alexengler/Desktop/DoT-Traffic")

acc2011 <- read_sas("./data/FSAS2011/accident.sas7bdat")
acc2012 <- read_sas("./data/FSAS2012/accident.sas7bdat")
acc2013 <- read_sas("./data/FARS2013NationalSAS/accident.sas7bdat")
acc2014 <- read_sas("./data/FARS2014NationalSAS/accident.sas7bdat")
acc2015 <- read.csv("./data/FARS2015NationalCSV/accident.csv")

dim(acc2013)
dim(acc2014)
dim(acc2015)

## ST_CASE is Unique Identifier
table(table(acc2015$ST_CASE))

## By State:
as.data.frame(table(acc$STATE))

## Number Fatally Injured Persons (Range 1-10):
acc$FATALS

## Data Cleaning and Merging
colnames(acc2015)[!colnames(acc2015) %in% colnames(acc2014)]
acc2015_lim <- select(acc2015, -c(RUR_URB, FUNC_SYS, RD_OWNER))

colnames(acc2014)[!colnames(acc2014) %in% colnames(acc2015)]
acc2015_lim$ROAD_FNC <- NA

acc <- rbind(acc2011, acc2012, acc2013, acc2014, acc2015_lim)



agg <- acc %>% 
  group_by(STATE, YEAR) %>%
  summarize(tot = sum(FATALS))


ggplot(agg, aes(x=YEAR, y=STATE)) + 
  geom_tile(aes(fill = tot), colour = "white") 
  + scale_fill_gradient(low = "white", high = "steelblue")


