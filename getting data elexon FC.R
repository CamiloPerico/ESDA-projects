library(httr)
library(ggplot2)

#This section corresponds to the procedure to just get one settlement date
service <- 'B1610' #In this case we change this variable
SettlementDate <- '2017-12-01'
APIKey <- 'strk9dt12yoof7y'
Period <- '*'
NGCBMUnitID <- '2__ABLBL000'
ServiceType <- 'csv'
url2 <- capture.output(cat('https://api.bmreports.com/BMRS/', service, '/v1?APIKey=', APIKey,
                           '&SettlementDate=', SettlementDate,
                           '&Period=', Period,
                           '&ServiceType=', ServiceType, sep = ''))

url2
generationUnit_raw <- GET(url = url2) 
names(generationUnit_raw)

generationUnit_csv <- rawToChar(generationUnit_raw$content)
generationUnit_HH <- read.table(text = generationUnit_csv, sep=',', fill = TRUE) 

generationUnit_HH <- generationUnit_HH[-(1:4),]
rownames(generationUnit_HH) <- NULL
colnames(generationUnit_HH) <- as.character(unlist(generationUnit_HH[1,]))
generationUnit_HH <- generationUnit_HH[-1,]
rownames(generationUnit_HH) <- NULL
#---------------------

#This section corresponds to the loop to get the range of date in data
#First you select from which day until which day you want to get data from
start <- as.Date("01-01-18",format="%d-%m-%y")
end   <- as.Date("31-01-18",format="%d-%m-%y")

theDate <- start
generationrange <- generationrange[-(1:nrow(generationrange)),]

while (theDate <= end)
{
  url3 <- capture.output(cat('https://api.bmreports.com/BMRS/', service, '/v1?APIKey=', APIKey,
                             '&SettlementDate=', format(theDate),
                             '&Period=', Period,
                             '&ServiceType=', ServiceType, sep = ''))
  generationloop_raw <- GET(url = url3)
  
  generationloop_csv <- rawToChar(generationloop_raw$content)
  generationloop_HH <- read.table(text = generationloop_csv, sep=',', fill = TRUE)
  
  generationloop_HH <- generationloop_HH[-(1:4),]
  rownames(generationloop_HH) <- NULL
  colnames(generationloop_HH) <- as.character(unlist(generationloop_HH[1,]))
  generationloop_HH <- generationloop_HH[-1,]
  rownames(generationloop_HH) <- NULL
  generationloop_HH <- generationloop_HH[-c(nrow(generationloop_HH)),]
  
  print(url3)
  
  generationrange <- rbind(generationrange, generationloop_HH) 
  theDate <- theDate+1
}

generation_jan_2018 <- generationrange
write.csv(generationrange,'generation012018.csv')


#Code to merge the generation units with the price

generationUnit_HH$`Settlement Date` <- ymd(generationUnit_HH$`Settlement Date`)
names(generationUnit_HH)[8] <- "V3"
names(generationUnit_HH)[9] <- "V4"

Bal_Mkt_Price$V3 <- ymd(Bal_Mkt_Price$V3)

generationUnit_HH <- left_join(generationUnit_HH,Bal_Mkt_Price, by = c("V3", "V4"))

generationUnit_HH <- generationUnit_HH[,-c(10:21)]

