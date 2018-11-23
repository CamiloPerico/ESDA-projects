#The following code is an example to extract data from elexon
#The code is based on the BMRS API and Data Push User Guide
#https://www.elexon.co.uk/guidance-note/bmrs-api-data-push-user-guide/

library(httr)
library(ggplot2)
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


