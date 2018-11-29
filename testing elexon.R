#The following code is an example to extract data from elexon
#The code is based on the BMRS API and Data Push User Guide
#https://www.elexon.co.uk/guidance-note/bmrs-api-data-push-user-guide/

library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
#1. This first lines of code are to obtain 5.2.17 Half Hourly Outturn Generation by Fuel Type

service <- 'FuelHH' #The service is the main variable that links with the different requests
APIKey <- 'x11mhwta8mlnbb8'
FromDate <- '2018-11-13'
ToDate <- '2018-11-13'
ServiceType <- 'csv'
url <- capture.output(cat('https://api.bmreports.com/BMRS/', service, '/v1?APIKey=', APIKey,
                          '&FromDate=', FromDate,
                          '&ToDate=', ToDate,
                          '&ServiceType=', ServiceType, sep = ''))

#With the get function we make the request made on the URL
fuelHH_raw <- GET(url = url) 
names(fuelHH_raw)

fuelHH_csv <- rawToChar(fuelHH_raw$content) 
head(fuelHH_csv)

fuelHH_raw <- GET(url = url)
fuelHH_csv <- rawToChar(fuelHH_raw$content)
fuel_HH <- read.table(text = fuelHH_csv, sep=',', fill = TRUE) 
wind_generation <- fuel_HH$V8

ggplot(fuel_HH)+geom_line(aes(x=V3,y=V8))+ 
  geom_line(aes(x=V3,y=V4),col='red')+ 
  xlab("Settlement Period")+ ylab("Generation MwHr")


#----------------

#2. The second code is to get the following type of data:5.2.15 Generation By Fuel Type (Current)

service1 <- 'FUELINST' #In this case we change this variable
FromDate1 <- '2018-11-13'
ToDate1 <- '2018-11-13'
NGCBMUnitID <- '2__ABLBL000'
ServiceType <- 'csv'
url1 <- capture.output(cat('https://api.bmreports.com/BMRS/', service, '/v1?APIKey=', APIKey,
                          '&FromDate=', FromDate1,
                          '&ToDate=', ToDate1,
                          '&ServiceType=', ServiceType, sep = ''))

fuelINST_raw <- GET(url = url1) 
names(fuelINST_raw)

fuelINST_csv <- rawToChar(fuelINST_raw$content)
fuelINST_HH <- read.table(text = fuelINST_csv, sep=',', fill = TRUE) 
tail(fuelINST_HH)

#----------

#3. Wind generation alternative

service2 <- 'B1630' #In this case we change this variable
SettlementDate <- '2018-11-13'
Period <- '*'
ServiceType2 <- 'csv'
url2 <- capture.output(cat('https://api.bmreports.com/BMRS/', service2, '/v1?APIKey=', APIKey,
                           '&SettlementDate=', SettlementDate,
                           '&Period=', Period,
                           '&ServiceType=', ServiceType2, sep = ''))

fuelWind_raw <- GET(url = url2) 
names(fuelWind_raw)

fuelWind_csv <- rawToChar(fuelWind_raw$content)
fuelWind_HH <- read.table(text = fuelWind_csv, sep=',', fill = TRUE) 

#------------
#4. CP 

service3 <- 'WINDFORPK' #In this case we change this variable
FromDate3 <- '2018-11-13'
ToDate3 <- '2018-11-18'
ServiceType3 <- 'csv'
url3 <- capture.output(cat('https://api.bmreports.com/BMRS/', service3, '/v1?APIKey=', APIKey,
                           '&FromDate=', FromDate3,
                           '&ToDate=', ToDate3,
                           '&ServiceType=', ServiceType3, sep = ''))

CPWind_raw <- GET(url = url3) 
names(CPWind_raw)

CPWind_csv <- rawToChar(CPWind_raw$content)
CPWind_HH <- read.table(text = CPWind_csv, sep=',', fill = TRUE) 


#------------
#5. CP Aggregated Generation per type

service4 <- 'B1620' #In this case we change this variable
SettlementDate <- '2018-11-13'
Period <- '*'
ServiceType4 <- 'csv'
url4 <- capture.output(cat('https://api.bmreports.com/BMRS/', service4, '/v1?APIKey=', APIKey,
                           '&SettlementDate=', SettlementDate,
                           '&Period=', Period,
                           '&ServiceType=', ServiceType4, sep = ''))

Agg_Gen_raw <- GET(url = url4) 
names(CPWind_raw)

Agg_Gen_csv <- rawToChar(Agg_Gen_raw$content)
Agg_Gen__HH <- read.table(text = Agg_Gen_csv, sep=',', fill = TRUE) 

#------------
#6. B1610 - Actual Genreation Ouput per Generation Unit

service5 <- 'B1610' #In this case we change this variable
SettlementDate <- '2018-11-16'
Period <- '*'
ServiceType5 <- 'csv'
url5 <- capture.output(cat('https://api.bmreports.com/BMRS/', service5, '/v1?APIKey=', APIKey,
                           '&SettlementDate=', SettlementDate,
                           '&Period=', Period,
                           '&ServiceType=', ServiceType5, sep = ''))

Gen_Per_Unit_raw <- GET(url = url5) 
names(Gen_Per_Unit_raw)

Gen_Per_Unit_csv <- rawToChar(Gen_Per_Unit_raw$content)
Gen_Per_Unit__HH <- read.table(text = Gen_Per_Unit_csv, sep=',', fill = TRUE) 
Gen_Per_Unit__HH <- Gen_Per_Unit__HH[-c(1, 2, 3, 4),]


#### IMporting Market Index Data
service6 <- 'MID' #In this case we change this variable
FromSettlementDate <- '2017-12-25'
ToSettlementDate <- '2017-12-28'
Period <- '*'
ServiceType6 <- 'csv'
url6 <- capture.output(cat('https://api.bmreports.com/BMRS/', service6, '/v1?APIKey=', APIKey,
                           '&FromSettlementDate=', FromSettlementDate,
                           '&ToSettlementDate=', ToSettlementDate,
                           '&Period=', Period,
                           '&ServiceType=', ServiceType6, sep = ''))

Bal_Mkt_Price_raw <- GET(url = url6) 
names(Bal_Mkt_Price_raw)

Bal_Mkt_Price_csv <- rawToChar(Bal_Mkt_Price_raw$content)
Bal_Mkt_Price <- read.table(text = Bal_Mkt_Price_csv, sep=',', fill = TRUE) 
Bal_Mkt_Price$V3 <- ymd(Bal_Mkt_Price$V3)
Bal_Mkt_Price$V4 <- as.factor(Bal_Mkt_Price$V4)
Bal_Mkt_Price <- Bal_Mkt_Price[-c((nrow(Bal_Mkt_Price)-8):nrow(Bal_Mkt_Price)),]
write.csv(Bal_Mkt_Price,'generation122017.csv')


### Loading EIC codes
EIC_codes_raw <- read.csv("EIC_Codes_Generation.csv",stringsAsFactors = F)
Gen_Per_Unit__HH2 <- full_join(Gen_Per_Unit__HH, EIC_codes_raw, by = c("V11" = "Energy.Identification.Code") )
Gen_Per_Unit__HH2$V8 <- ymd(Gen_Per_Unit__HH2$V8)
Gen_Per_Unit__HH2 <- full_join(Gen_Per_Unit__HH2, Bal_Mkt_Price, by=c("V8" = "V3","V9" = "V4"))



##### Running Loop for Market data
service6 <- 'MID' #In this case we change this variable
Period <- '*'
ServiceType6 <- 'csv'

start <- as.Date("01-01-18",format="%d-%m-%y")
end   <- as.Date("31-01-18",format="%d-%m-%y")

theDate <- start
Bal_Mkt_Price <- NULL
while (theDate <= end)
{
FromSettlementDate <- theDate
ToSettlementDate <- theDate
url6 <- capture.output(cat('https://api.bmreports.com/BMRS/', service6, '/v1?APIKey=', APIKey,
                           '&FromSettlementDate=', format(FromSettlementDate),
                           '&ToSettlementDate=', format(ToSettlementDate),
                           '&Period=', Period,
                           '&ServiceType=', ServiceType6, sep = ''))

Bal_Mkt_Price_Loop_raw <- GET(url = url6) 
names(Bal_Mkt_Price_Loop_raw)
Bal_Mkt_Price_Loop_csv <- rawToChar(Bal_Mkt_Price_Loop_raw$content)
Bal_Mkt_Price_Loop <- read.table(text = Bal_Mkt_Price_Loop_csv, sep=',', fill = TRUE) 
Bal_Mkt_Price_Loop <- Bal_Mkt_Price_Loop[-c((nrow(Bal_Mkt_Price_Loop)-48):nrow(Bal_Mkt_Price_Loop)),]
Bal_Mkt_Price_Loop <- Bal_Mkt_Price_Loop[-1,]
print(url6)

Bal_Mkt_Price <- rbind(Bal_Mkt_Price, Bal_Mkt_Price_Loop)
theDate <- theDate+1
}
write.csv(Bal_Mkt_Price,'Prices_012018.csv')

plot(Bal_Mkt_Price$V6,Bal_Mkt_Price$V5)
plot(Bal_Mkt_Price$V4,Bal_Mkt_Price$V5)
