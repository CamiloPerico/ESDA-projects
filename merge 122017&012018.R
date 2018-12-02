
#read data of generation unit, mkt_price, EIC
gen122017 <- read.csv('generation122017 .csv', stringsAsFactors = F)
gen012018 <- read.csv('generation012018.csv', stringsAsFactors = F)

price122017_raw <- 'https://raw.githubusercontent.com/CamiloPerico/ESDA-projects/master/Prices_122017.csv'
price122017 <- read.csv(price122017_raw,header = T)
price012018_raw <-'https://raw.githubusercontent.com/CamiloPerico/ESDA-projects/master/Prices_012018.csv'
price012018 <- read.csv(price012018_raw,header = T)

EIC <- read.csv('EIC_Codes_Generation.csv',stringsAsFactors = F)

#rename mkt_price data
library(tidyverse)
names(price122017)[2:7] <- c("Record Type","Data Provider","Settlement Date", "Settlement Period","Price","Volumn")
names(price012018)[2:7] <- c("Record Type","Data Provider","Settlement Date", "Settlement Period","Price","Volumn")

#full——join EIC with generation unit & filter "WIND"
data012018 <- full_join(gen012018, EIC, by=c("Registered.Resource.EIC.Code"="Energy.Identification.Code"))
data012018_wind <- filter(data012018,data012018$BMRA_FUEL_TYPE=="WIND")
data012018_wind <- data012018_wind[c(1:37)]

#format DATE 
library(lubridate)
data012018_wind$Settlement.Date <- as.Date(data012018_wind$Settlement.Date,'%d/%m/%Y')
str(data012018_wind$Settlement.Date)
price012018$`Settlement Date` <- ymd(price012018$`Settlement Date`)

#merge three datasets together 
data012018_tidy <- full_join(data012018_wind,price012018,by=c("Settlement.Date"="Settlement Date","Settlement.Period"="Settlement Period"))

#repeat the work for data of 122017
data122017 <- full_join(gen122017,EIC, by=c("Registered.Resource.EIC.Code"="Energy.Identification.Code"))
data122017_wind <- filter(data122017,data122017$BMRA_FUEL_TYPE=="WIND")
data122017_wind <- data122017_wind[c(1:37)]

data122017_wind$Settlement.Date <- as.Date(data122017_wind$Settlement.Date,'%Y-%m-%d')
price122017$`Settlement Date` <- ymd(price122017$`Settlement Date`)
str(data122017_wind$Settlement.Date)
data122017_tidy <- full_join(data122017_wind,price122017,by=c("Settlement.Date"="Settlement Date","Settlement.Period"="Settlement Period"))

#select the data required for dataframe 122017
data122017_frame <- data122017_tidy[c(5,6,36,9,10,12,26,28,14,35,41,42)]
names(data122017_frame)[2] <- "Production Quality"
names(data122017_frame)[6] <- "EIC Code"
names(data122017_frame)[7] <- "EIC Name"
names(data122017_frame)[8] <- "EIC Responsable"
names(data122017_frame)[9] <- "BMU Units Market Generation"
names(data122017_frame)[10] <- "Fuel Type"
  
write.csv(data122017_frame,"data122017.csv")


#select the data required for dataframe 012018
data012018_frame <- data012018_tidy[c(5,6,36,9,10,12,26,28,14,35,41,42)]
names(data012018_frame)[2] <- "Production Quality"
names(data012018_frame)[6] <- "EIC Code"
names(data012018_frame)[7] <- "EIC Name"
names(data012018_frame)[8] <- "EIC Responsable"
names(data012018_frame)[9] <- "BMU Units Market Generation"
names(data012018_frame)[10] <- "Fuel Type"

write.csv(data012018_frame,"data012018.csv")



