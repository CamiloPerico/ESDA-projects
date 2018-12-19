library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

APIKey <- 'x11mhwta8mlnbb8'

#Extract data from generatio unit
service_gen <- 'B1610'
Period <- '*'
ServiceType <- 'csv'

#Define the range that we want data from:
start <- as.Date("01-01-17",format="%d-%m-%y")
end   <- as.Date("31-12-17",format="%d-%m-%y")

theDate <- start
#Define the matrix that will hold the data extracted
generationrange <- data.frame(matrix(ncol = 19, nrow = 0))

while (theDate <= end)
{
  url <- capture.output(cat('https://api.bmreports.com/BMRS/', service_gen, '/v1?APIKey=', APIKey,
                             '&SettlementDate=', format(theDate),
                             '&Period=', Period,
                             '&ServiceType=', ServiceType, sep = ''))
  generationloop_raw <- GET(url = url)
  
  generationloop_csv <- rawToChar(generationloop_raw$content)
  generationloop_HH <- read.table(text = generationloop_csv, sep=',', fill = TRUE)
  
  generationloop_HH <- generationloop_HH[-(1:4),]
  rownames(generationloop_HH) <- NULL
  colnames(generationloop_HH) <- as.character(unlist(generationloop_HH[1,]))
  generationloop_HH <- generationloop_HH[-1,]
  rownames(generationloop_HH) <- NULL
  generationloop_HH <- generationloop_HH[-c(nrow(generationloop_HH)),]
  
  print(url)
  
  generationrange <- rbind(generationrange, generationloop_HH) 
  theDate <- theDate+1
}

#Now we get the price for each settlement period
service_price <- 'MID'

price_range <- data.frame(matrix(ncol = 6, nrow = 0))

theDate2 <- start

while (theDate2 <= end)
{
  FromSettlementDate <- theDate2
  ToSettlementDate <- theDate2
  url6 <- capture.output(cat('https://api.bmreports.com/BMRS/', service_price, '/v1?APIKey=', APIKey,
                             '&FromSettlementDate=', format(FromSettlementDate),
                             '&ToSettlementDate=', format(ToSettlementDate),
                             '&Period=', Period,
                             '&ServiceType=', ServiceType, sep = ''))
  
  Bal_Mkt_Price_Loop_raw <- GET(url = url6) 
  
  Bal_Mkt_Price_Loop_csv <- rawToChar(Bal_Mkt_Price_Loop_raw$content)
  Bal_Mkt_Price_Loop <- read.table(text = Bal_Mkt_Price_Loop_csv, sep=',', fill = TRUE) 
  Bal_Mkt_Price_Loop <- Bal_Mkt_Price_Loop[-c((nrow(Bal_Mkt_Price_Loop)-48):nrow(Bal_Mkt_Price_Loop)),]
  Bal_Mkt_Price_Loop <- Bal_Mkt_Price_Loop[-1,]
  rownames(Bal_Mkt_Price_Loop) <- NULL
  print(url6)
  
  price_range <- rbind(price_range, Bal_Mkt_Price_Loop)
  theDate2 <- theDate2+1
}

#Now We merge the two datasets from Elexon. generationrange and price range
#Changing the format of the date variable and renaiming the date and settlement hour to merge with price range
generationrange$`Settlement Date` <- ymd(generationrange$`Settlement Date`)
names(generationrange)[8] <- "V3"
names(generationrange)[9] <- "V4"

price_range$V3 <- ymd(price_range$V3)
price_range$V4 <- as.factor(price_range$V4)

#Now we do a left merge from generatin range to price range by the settlement date and hour
generation_price <- left_join(generationrange,price_range, by = c("V3", "V4"))

#Now we eliminate the column we do not want from our dataset of generation and price
generation_price <- subset(generation_price, select = c(5,8,9,11,15,22,23))

names(generation_price)[1] <- "EnergySupply"
names(generation_price)[2] <- "SettlementDate"
names(generation_price)[3] <- "SettlementPeriod"
names(generation_price)[6] <- "Price"
names(generation_price)[7] <- "Volume"

#Now we merge by the EIC Code the generation price dataset with the EIC Codes generation

EIC_codes_raw <- read.csv("EIC_Codes_Generation_nodupli.csv",stringsAsFactors = F)
EIC_codes_raw <- EIC_codes_raw[,-c(3,4,8,10:13,15)]
names(EIC_codes_raw)[3] <- "Registered Resource EIC Code"

generation_price$`Registered Resource EIC Code` <- as.character(generation_price$`Registered Resource EIC Code`)

generation_unified <- left_join(generation_price, EIC_codes_raw, by = "Registered Resource EIC Code")

#We now get a file with all the data BE CAREFUL REMOVE THIS FILE FROM YOUR GITHUB FOLDER TO DONT UPDATELOAD TO GITHUB
write.csv(generation_unified,'generation_price_unit_2017.csv')

#Code to evaluate duplicity
duplicated(EIC_codes_raw$`Registered Resource EIC Code`) | duplicated(EIC_codes_raw$`Registered Resource EIC Code`, fromLast = TRUE)

