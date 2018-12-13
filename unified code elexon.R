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
start <- as.Date("01-02-18",format="%d-%m-%y")
end   <- as.Date("02-02-18",format="%d-%m-%y")

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

#Extract the price of Market data - NOT RUNNING WELL AT LEAST FOR ME
service <- 'MID'
Bal_Mkt_Price <- data.frame(matrix(ncol = 6, nrow = 0))

theDate2 <- start

while (theDate2 <= end)
{
  FromSettlementDate <- theDate2
  ToSettlementDate <- theDate2
  url6 <- capture.output(cat('https://api.bmreports.com/BMRS/', service6, '/v1?APIKey=', APIKey,
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
  
  Bal_Mkt_Price <- rbind(Bal_Mkt_Price, Bal_Mkt_Price_Loop)
  theDate2 <- theDate2+1
}

#Now We merge the two datasets from Elexon. generationrange and Bal_Mkt_Price
generationrange$`Settlement Date` <- ymd(generationrange$`Settlement Date`)
names(generationrange)[8] <- "V3"
names(generationrange)[9] <- "V4"

Bal_Mkt_Price$V3 <- ymd(Bal_Mkt_Price$V3)
Bal_Mkt_Price$V4 <- as.factor(Bal_Mkt_Price$V4)

generation_price <- left_join(generationrange,Bal_Mkt_Price, by = c("V3", "V4"))

#Now we merge the generation price dataset with the EIC Codes generation

EIC_codes_raw <- read.csv("EIC_Codes_Generation.csv",stringsAsFactors = F)
EIC_codes_raw <- EIC_codes_raw[,-c(3,4,8,10:13,19:88)]
names(EIC_codes_raw)[3] <- "VCode"

names(generation_price)[11] <- "VCode"

generatio_unified <- left_join(generation_price, EIC_codes_raw, by = "VCode")

write.csv(generatio_unified,'test.csv')
colnames(generationrange) <-   c("*Document Type",
                                 "Business Type",
                                 "Process Type",
                                 "Time Series ID",
                                 "Quantity", 
                                 "Curve Type",
                                 "Resolution",
                                 "Settlement Date",
                                 "Settlement Period",
                                 "Power System Resource  Type",
                                 "Registered Resource EIC Code",
                                 "Market Generation Unit EIC Code",
                                 "Market Generation BMU Id", 
                                 "Market Generation NGC BMU Id",
                                 "BM Unit ID",
                                 "NGC BM Unit ID",
                                 "Active Flag",
                                 "Document ID",
                                 "Document RevNum")
