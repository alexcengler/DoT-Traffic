library(haven)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


setwd("/Users/alexengler/Desktop/DoT-Traffic")

acc2009 <- read_sas("./data/FSAS2009/accident.sas7bdat")
acc2010 <- read_sas("./data/FSAS2010/accident.sas7bdat")
acc2011 <- read_sas("./data/FSAS2011/accident.sas7bdat")
acc2012 <- read_sas("./data/FSAS2012/accident.sas7bdat")
acc2013 <- read_sas("./data/FARS2013NationalSAS/accident.sas7bdat")
acc2014 <- read_sas("./data/FARS2014NationalSAS/accident.sas7bdat")
acc2015 <- read.csv("./data/FARS2015NationalCSV/accident.csv")

dim(acc2013)
dim(acc2014)
dim(acc2015)

## Data Cleaning and Merging
colnames(acc2011)[!colnames(acc2011) %in% colnames(acc2010)]
acc2010_extra <- acc2010
acc2010_extra$PVH_INVL <- NA
acc2010_extra$PERNOTMVIT <- NA
acc2010_extra$PERMVIT <- NA

colnames(acc2015)[!colnames(acc2015) %in% colnames(acc2014)]
acc2015_lim <- select(acc2015, -c(RUR_URB, FUNC_SYS, RD_OWNER))

colnames(acc2014)[!colnames(acc2014) %in% colnames(acc2015)]
acc2015_lim$ROAD_FNC <- NA

acc <- rbind(acc2010_extra, acc2011, acc2012, acc2013, acc2014, acc2015_lim)

## Number Fatally Injured Persons (Range 1-10):
table(acc$FATALS)

## Merge on FIPS Code
fips <- read.csv("./data/fips_codes.csv")
fips <- fips[!duplicated(fips[2:3]),]

## Lost 12 rows come back to this later (hahah just kidding - ignore forever):
acc <- merge(fips, acc, by.x=c("State.FIPS.Code","County.FIPS.Code"), by.y=c("STATE","COUNTY"))

## ST_CASE is Unique Identifier (Only Unique to the Year)
table(table(acc$ST_CASE))

## By State:
as.data.frame(table(acc$STATE))


agg <- acc %>% 
  group_by(STATE, YEAR) %>%
  summarize(tot = sum(FATALS))


ggplot(agg, aes(x=YEAR, y=State.Abbreviation)) + 
  geom_tile(aes(fill = log10(tot)), colour = "white") + 
  scale_fill_distiller(palette="RdBu") +
  theme_classic()

ggplot(agg, aes(x=YEAR, y=tot, color=as.factor(State.Abbreviation))) + 
  geom_line() + theme_classic() + theme(legend.position="none")



