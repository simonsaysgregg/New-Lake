## Manipulation and assembly of data for analysis
## DATA period 2021-1989
## data location in PROJECT DIRECTORY Data
## dataset preparation
## 1st in processing order

## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
## Statistical analysis
require("statsr")        # Lots of stats stuff
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("TTR")
#require("xlsx")        # creates errors # Reads and writes to xlsx file
require("purrr")
require("tidyr")
require("fBasics")
require("pls")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Binding data files
## create vector of file date format for years 1989-2015
dates=c("89","90","91","92","93","94","95","96","97","98","99","00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15")
for (i in 1:length(dates)){
  # i=1
  date=dates[i]
  filename<-sprintf("./Data/colsom%s.txt",date)                                                                   ##Specify file where data is located
  use<-read.table(file=filename, sep="|", header=FALSE, quote = NULL, skipNul = TRUE)                     ##Read the file
  use=use[,-27]                                                                                           ##all but 27
  colnames(use)=c("COMMODITY.YEAR.ID","STATE.CODE","STATE.ABBREVIATION", "COUNTY.CODE", "COUNTY.NAME","COMMODITY.CODE", "COMMODITY.NAME", "INSURANCE.PLAN.CODE",
                  "INSURACE.PLAN.NAME.ABBREVIATION", "COVERAGE.CATEGORY", "STAGE.CODE", "CAUSE.OF.LOSS.CODE", "CAUSE.OF.LOSS.DESCRIPTION",
                  "MONTH.OF.LOSS", "MONTH.OF.LOSS.NAME", "YEAR.OF.LOSS", "POLICIES.EARNING.PERMIUM","POLICIES.INDEMNIFIED",
                  "NET.PLANTED.ACRES", "NET.ENDORSED.ACRES","LIABILITY", "TOTAL.PREMIUM", "PRODUCER.PAID.PREMIUM", 
                  "SUBSIDY", "STATE_PRIVATE.SUBSIDY", "ADDITIONAL.SUBSIDY","NET.DETERMINED.ACRES",
                  "INDEMNITY.AMOUNT", "LOSS.RATIO")                                                              ##Name all columns
  assign(paste0("List",i),use)                                                                                    ##Assign names for each list
}
total1=rbind(List1,List2,List3,List4,List5,List6,List7,List8,List9,List10,List11,List12,List13,List14,List15,List16,List17,List18,List19,List20,List21,List22,List23,List24,List25,List26)          ##Row bind all of the fingerprint files


## Binding data files
## create vector of file date format for years 2016-2021
dates=c("2016","2017","2018","2019","2020","2021")
for (i in 1:length(dates)){
  # i=1
  date=dates[i]
  filename<-sprintf("./Data/colsom_%s.txt",date)                                                                   ##Specify file where data is located
  use<-read.table(file=filename, sep="|", header=FALSE, quote = NULL, skipNul = TRUE)                     ##Read the file
  use=use[,-27]                                                                                           ##all but 27
  colnames(use)=c("COMMODITY.YEAR.ID","STATE.CODE","STATE.ABBREVIATION", "COUNTY.CODE", "COUNTY.NAME","COMMODITY.CODE", "COMMODITY.NAME", "INSURANCE.PLAN.CODE",
                  "INSURACE.PLAN.NAME.ABBREVIATION", "COVERAGE.CATEGORY", "STAGE.CODE", "CAUSE.OF.LOSS.CODE", "CAUSE.OF.LOSS.DESCRIPTION",
                  "MONTH.OF.LOSS", "MONTH.OF.LOSS.NAME", "YEAR.OF.LOSS", "POLICIES.EARNING.PERMIUM","POLICIES.INDEMNIFIED",
                  "NET.PLANTED.ACRES", "NET.ENDORSED.ACRES","LIABILITY", "TOTAL.PREMIUM", "PRODUCER.PAID.PREMIUM", 
                  "SUBSIDY", "STATE_PRIVATE.SUBSIDY", "ADDITIONAL.SUBSIDY","NET.DETERMINED.ACRES",
                  "INDEMNITY.AMOUNT", "LOSS.RATIO")                                                         ##Name all columns
  assign(paste0("List",i),use)                                                                                    ##Assign names for each list
}
total2=rbind(List1,List2,List3,List4,List5,List6)          ##Row bind all of the fingerprint files


## Bind total data files
DS <- bind_rows(total1,total2)

## Select counties in the Abermarle-Pammlico peninsula 
NC <- DS %>%
  subset(STATE.ABBREVIATION == "NC")

Onslow <- NC %>%
  subset(COUNTY.CODE == 133)

Jones <- NC %>%
  subset(COUNTY.CODE == 103)

Careret <- NC %>%
  subset(COUNTY.CODE == 31)

Craven <- NC %>%
  subset(COUNTY.CODE == 49)

Pamlico <- NC %>%
  subset(COUNTY.CODE == 137)

Beaufort <- NC %>%
  subset(COUNTY.CODE == 13)

Hyde <- NC %>%
  subset(COUNTY.CODE == 95)

## Bind county sets to make working data set
APCO <- bind_rows(Onslow, Jones, Careret, Craven, Pamlico, Beaufort, Hyde)

# REMOVE YEAR 1989
# NO CUSE OF LOSS DESCRIPTION, IF USEFUL CAN MATCH IT WITH LATER CODE VALUES
APCO_post89 <- APCO %>%
  subset(COMMODITY.YEAR.ID != 1989)


## cLEAN UP CUASE OF LOSS DATA BY CODE
COL <- APCO_post89[,c(12,13)]
COL_DIS <- distinct(COL)
COL_DIS.na <- COL_DIS[-c(37:42,47,54,59,63),]

COLD <- COL_DIS.na %>%
  arrange(CAUSE.OF.LOSS.CODE)

COLD_FINAL <- COLD[c(3,5,12,7,8,9,11,13,14,17,19,21,22,24,28,31,32,33,34,37,39,41,42,43,45,47,49,50,51,54,56),]

write.csv(COLD_FINAL, file = "./Data/COLD_FINAL.csv")
final <- read.csv(file = "./Data/COLD_FINAL.csv")
final <- final %>%
  select(CAUSE.OF.LOSS.CODE,
         CAUSE.OF.LOSS.DESCRIPTION)
## REPLACE LOSS CODE VALUE OF 01 TO 1
APCO_post89[APCO_post89 == "01"] <- "1"

## match values from clean up to processing spreadsheets
APCO_post89_match <- full_join(APCO_post89, final, by = "CAUSE.OF.LOSS.CODE")

APCO1 <- APCO_post89_match %>%
  distinct()




#######
## PRELIM ANALYSIS OF APCO DATA SET

## WORKING DATASET 
# APCO1


# subset by county

Onslow1 <- APCO1 %>%
  subset(COUNTY.CODE == 133)

Jones1 <- APCO1 %>%
  subset(COUNTY.CODE == 103)

Careret1 <- APCO1 %>%
  subset(COUNTY.CODE == 31)

Craven1 <- APCO1 %>%
  subset(COUNTY.CODE == 49)

Pamlico1 <- APCO1 %>%
  subset(COUNTY.CODE == 137)

Beaufort1 <- APCO1 %>%
  subset(COUNTY.CODE == 13)

Hyde1 <- APCO1 %>%
  subset(COUNTY.CODE == 95)

## Group county wide data by year then summarize

onw1_CROP <- Onslow1 %>%
  count(COMMODITY.YEAR.ID, COMMODITY.NAME)

onw1_loss <- Onslow1 %>%
  count(COMMODITY.YEAR.ID, CAUSE.OF.LOSS.DESCRIPTION.y)

ggplot(onw1_loss)+
  geom_point(aes(x = COMMODITY.YEAR.ID, y = n, color = CAUSE.OF.LOSS.DESCRIPTION.y))

ggplot(onw1_loss)+
  geom_line(aes(x = COMMODITY.YEAR.ID, y = n, color = CAUSE.OF.LOSS.DESCRIPTION.y))

