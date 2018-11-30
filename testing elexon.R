#The following code is an example to extract data from elexon
#The code is based on the BMRS API and Data Push User Guide
#https://www.elexon.co.uk/guidance-note/bmrs-api-data-push-user-guide/

library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

APIKey <- 'x11mhwta8mlnbb8'

#------------
#6. B1610 - Actual Genreation Ouput per Generation Unit

service5 <- 'B1610' #In this case we change this variable
SettlementDate <- '2017-12-25'
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
service6 <- 'MID' #In this casewe change this variable
FromSettlementDate <- '2017-12-25'
ToSettlementDate <- '2017-12-25'
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
Bal_Mkt_Price <- Bal_Mkt_Price[-c((nrow(Bal_Mkt_Price)-48):nrow(Bal_Mkt_Price)),]
Bal_Mkt_Price <- Bal_Mkt_Price[-1,]
write.csv(Bal_Mkt_Price,'generation12201333.csv')


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
