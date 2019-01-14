library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)


#The following code corresponds to the kenobi group it is divided in the following sections:
#1. Data adquisition through API from ELEXON. 3 API develop: Generation unis, Market Price and Imbalance Price
#2. K-means method to analyze the clusters fuel type over the year 2017. Fuel type analyzed: CCGT, COAL, NUCLEAR AND WIND
#3. PCA develop over selected period for 4 scenarios
#4. NEURAL NETWORK considering the 19 most important power plants delivered from PCA results
#5. The tool for visualization



# 1.  The following section corresponds to ELEXON API -------------------------


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

# Price of the Market -----------------------------------------------------
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


# Price from Imbalance Market  -------------------------------------------------
service_price2 <- 'B1770'

price_long <- data.frame(matrix(ncol = 15, nrow = 0))

theDate3 <- start

while (theDate3 <= end)
{
  FromSettlementDate1 <- theDate3
  ToSettlementDate <- theDate3
  url7 <- capture.output(cat('https://api.bmreports.com/BMRS/', service_price2, '/v1?APIKey=', APIKey,
                             '&SettlementDate=', format(FromSettlementDate1),
                             '&Period=', Period,
                             '&ServiceType=', ServiceType, sep = ''))
  
  price_new_raw <- GET(url = url7) 
  
  price_new_csv <- rawToChar(price_new_raw$content)
  price_loop <- read.table(text =price_new_csv, sep=',', fill = TRUE) 
  price_loop <-price_loop[-c(1:4),]
  rownames(price_loop) <- NULL
  colnames(price_loop) <- as.character(unlist(price_loop[1,]))
  price_loop <-price_loop[-1,]
  price_loop <-price_loop[-c(nrow(price_loop)),]
  rownames(price_loop) <- NULL
  print(url7)
  
  price_long <- rbind(price_long, price_loop)
  theDate3 <- theDate3+1
}

write.csv(price_long,'price_market_2017.csv')




# 2. The following section is K-MEANS ----------------------------------------


df <- read.csv("generation_price_unit_2017.csv")
df <- select(df, -c(1))
df <- select(df, SettlementDate, SettlementPeriod, everything())
df <- filter(df, SettlementPeriod <=48)
names(df)[4] = "Code"
summary(df)


#The following line give us which type of fuels we can filter in the database
levels(df$BMRA_FUEL_TYPE)
#We can choose within the following fuels in the UK:
#BIOMASS" "CCGT"    "COAL"    "NPSHYD"  "NUCLEAR" "OCGT"    "PS"      "WIND"   

#We will now develop a K-means general method for any type of FUEL
#Define the variables that you want to analize
#INPUTS
fuel_type <- "WIND"
n_cluster <- 4
#Use when FILTER BY DATE
#Date_k <- "2017-02-01"
#Use when FILTER BY CODE
#n_code <- "48W0000000ABTH7Y"


#Type of filter
#Per FUEL
fuel_long <- filter(df, BMRA_FUEL_TYPE == fuel_type)
#Per MONTH or DAY
#fuel_long <- filter(fuel_long, month(SettlementDate) == month(Date_k))
#Per CODE
#fuel_long <- filter(fuel_long, Code == n_code)
#Filter Date Range Winter and Summer
# fuel_long$SettlementDate <- ymd(fuel_long$SettlementDate)
# WINTER fuel_long <- fuel_long[fuel_long$SettlementDate >= "2017-01-01" & fuel_long$SettlementDate <= "2017-03-21",]
# SUMMER fuel_long <- fuel_long[fuel_long$SettlementDate >= "2017-06-21" & fuel_long$SettlementDate <= "2017-09-23",]


summary(fuel_long$BMRA_FUEL_TYPE)
summary(fuel_long$SettlementDate)
summary(fuel_long$Code)




?order()
# 2.1 K-MEANS -----------------------------------------------------------------
#This part begins the code to evaluate cluster with K-means method
fuel_long <- subset(fuel_long, select = c(1,2,3,4))
fuel_spread <- spread(fuel_long, key = "SettlementPeriod", value = "EnergySupply")

#We look for NA in each both long and spread df
colSums(is.na(fuel_long))
colSums(is.na(fuel_spread))

#We can set the NA to 0 as the line 52 DOES or we can delete those rows
#fuel_spread <- na.omit(fuel_spread)

#There area few NA on the the fuel_spread DF this is given that we have hours with no data.
#We now change all the NA found in the spread dataset for zero given we cannot do k-means with empty valeus
fuel_spread[is.na(fuel_spread)] <- 0

#We first begin evaluating the number of cluster and how this effect the sum of squares from 1 to 20 cluster
store_ss <- vector("numeric",length=20) 
for(i in 1:20){
  foo <- kmeans(fuel_spread[,3:50],(i+1))
  store_ss[i] <- foo$tot.withinss
}
plot(2:21,store_ss,'b')
?kmeans

#We now begin our cluster, we are interested in developing K number of cluster
foo <- kmeans(fuel_spread[,3:50],n_cluster) 
names(foo)
foo$size
#This results corresponds to the amount of curves that go to each cluster

#Now we want to know the shape of this K clusters
fuel_kmeans <- data.frame(foo$centers)
fuel_kmeans$Cluster <- 1:n_cluster
mu_long <- gather(fuel_kmeans,"Time","MWh",1:48)
mu_long$Time <- as.numeric(gsub("X","",(mu_long$Time))) 
ggplot(mu_long)+geom_line(aes(x=Time,y=MWh,group=Cluster,col=factor(Cluster)))


#Now we want to know the shape of all the curves that fit each cluster
fuel_spread$Cluster <- foo$cluster
fuel_cluster_long <- gather(fuel_spread, Time,MWh,3:50)
fuel_cluster_long$Time <- as.numeric(gsub("X","",(fuel_cluster_long$Time))) 
fuel_cluster_long$SettlementDate <- ymd(fuel_cluster_long$SettlementDate)
fuel_cluster_long$day_of_week <- wday(fuel_cluster_long$SettlementDate,label=TRUE)


ggplot(fuel_cluster_long)+geom_line(aes(x=Time,y=MWh, group=interaction(SettlementDate, Code)), alpha=.5)+
  facet_wrap(~Cluster,ncol=2) + ggtitle("Wind Cluster 2017")


table(fuel_cluster_long[,c(3,6)])/48
fuel_cluster_long$mth <- month(fuel_cluster_long$SettlementDate,label=TRUE) 
fuel_cluster_long$mth <- factor(fuel_cluster_long$mth, levels = month.abb) 
t(table(fuel_cluster_long[,c(3,7)])/48)



# 2.2 VISUALIZATION OF CURVE -----------------------------------------------
#Visualization of curves per a selected day 

visu_curves <- fuel_long

visu_curves <- filter(visu_curves, SettlementDate == "2017-01-05")

uniq_code <- unique(visu_curves$Code)
random_code <- uniq_code[sample(length(uniq_code),1)]

ind <- NULL
for(i in 1:length(random_code)){
  ind <- c(ind,which(visu_curves$Code==random_code[i])) }

ggplot(visu_curves[ind,])+ 
  geom_line(aes(x=as.numeric(SettlementPeriod),y=EnergySupply, group=Code,col=as.factor(Code)), na.rm = T)+
  ylim(0,500)
visu_curves[ind,]
?geom_line


# 3.3 PRICE K-MEANS  -------------------------------------------------------

#K-Means for the price
#New prices values
price_market <- read.csv("price_market_2017.csv")
price_market <- select(price_market, c(12,11,10))
price_market <- unique(price_market)
price_market$SettlementDate <- ymd(price_market$SettlementDate)
price_market <- filter(price_market, SettlementPeriod <=48)

# 3.31 PRICE FROM OLD Values ONLY USE WITH OLD PRICE-----
price_dataset_long <- subset(df, select = c(1,2,6))
price_dataset_long$SettlementDate <- ymd(price_dataset_long$SettlementDate)
price_dataset_long <- unique(price_dataset_long)
# SUMMER price_dataset_long <- price_dataset_long[price_dataset_long$SettlementDate >= "2017-06-21" & price_dataset_long$SettlementDate <= "2017-09-23",]
# WINTER price_dataset_long <- price_dataset_long[price_dataset_long$SettlementDate >= "2017-01-01" & price_dataset_long$SettlementDate <= "2017-03-21",]
#---- ONLY USE WITH OLD PRICE
price_dataset_spread <- spread(price_dataset_long, key = "SettlementPeriod", value = "Price")
#---- ONLY USE WITH OLD PRICE

#---- ONLY USE WITH NEW PRICE IMBLAANCE MARKET
price_dataset_spread <- spread(price_market, key = "SettlementPeriod", value = "ImbalancePriceAmount")
#---- ONLY USE WITH NEW PRICE IMBLAANCE MARKET

#Remove all Rows with one NA
#price_dataset_spread <- na.omit(price_dataset_spread)

#summary(price_dataset_spread)
#We first see if we have any NA values and set them equal to zero to be able to carry the k-means
colSums(is.na(price_dataset_spread))
price_dataset_spread[is.na(price_dataset_spread)] <- 0

#The 2017-05-17 was a day that had a special speak price
#Test what happen if we remove it
price_dataset_spread <- filter(price_dataset_spread, SettlementDate != "2017-05-17")

#Now we develop the K-Means
foo_price <- kmeans(price_dataset_spread[,2:49], n_cluster) 
names(foo_price)
foo_price$size

price_kmeans <- data.frame(foo_price$centers)
price_kmeans$Cluster <- 1:4
mu_long_price <- gather(price_kmeans,"Time","Price",1:48)
mu_long_price$Time <- as.numeric(gsub("X","",(mu_long_price$Time))) 

ggplot(mu_long_price)+geom_line(aes(x=Time,y=Price,group=Cluster,col=factor(Cluster)))+
  scale_colour_manual(name="Cluster", values=c("red", "dark green", "blue", "purple"))+  
  ggtitle("Price of the imbalance market in 2017")+
  labs(y = "Price (GBP/MWh)", x = "Settlement Period")+
  theme(title = element_text(size=18, face = "bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16))

price_dataset_spread$Cluster <- foo_price$cluster
price_cluster_long <- gather(price_dataset_spread,Time,Price,2:49)
price_cluster_long$Time <- as.numeric(gsub("X","",(price_cluster_long$Time))) 
price_cluster_long$SettlementDate <- ymd(price_cluster_long$SettlementDate)
price_cluster_long$day_week <- wday(price_cluster_long$SettlementDate,label=TRUE)


ggplot(price_cluster_long)+geom_line(aes(x=(Time),y=Price,group=SettlementDate), alpha=.5)+
  facet_wrap(~Cluster,ncol=2, labeller = labeller(Cluster = labels)) + 
  ggtitle("Price of the imbalance market in 2017")+
  labs(y="Price (GBP/MWh))", x = "Settlement Period")+
  theme(title = element_text(size=18, face = "bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16),
        strip.text.x = element_text(size = 16))

table(price_cluster_long[,c(2,5)])/48
price_cluster_long$mth <- month(price_cluster_long$SettlementDate,label=TRUE) 
price_cluster_long$mth <- factor(price_cluster_long$mth, levels = month.abb) 
t(table(price_cluster_long[,c(2,6)])/48)
b <- as.data.frame.matrix(t(table(price_cluster_long[,c(2,6)])/48))

# 3.4 CLUSTER FOR MOST PRODUCTIVE PLANTS ----------------------------------
#Operation of plants analysis
n_rank <- 10

fuel_operation <- fuel_long
fuel_operation <- subset(fuel_operation, select = c(1,2,3,4))


#This corresponds to the total yearly generation of each plant
fuel_total_generation <- fuel_operation %>%
  group_by(Code) %>%
  summarise(TotalGeneration = sum(EnergySupply))


#We order them from bigger
fuel_total_generation <- fuel_total_generation[order(fuel_total_generation$TotalGeneration, decreasing = F),] 

#Code to  run the top generating plants: fuel_total_generation <- fuel_total_generation[1:n_rank,]
#Code to run the worst generating plants: fuel_total_generation <- fuel_total_generation[(NROW(fuel_total_generation)-n_rank):NROW(fuel_total_generation),]
fuel_total_generation <- fuel_total_generation <- fuel_total_generation[1:n_rank,]

vector_plants <- fuel_total_generation[,1]
vector_plants <- as.data.frame(vector_plants)
vector_plants
fuel_plants <- data.frame()


for (i in 1:n_rank) {
  data_plant <- filter(fuel_operation, Code == as.factor(vector_plants[i,1]))
  fuel_plants <- rbind(fuel_plants, data_plant)
}

fuel_plants$Code <- factor(fuel_plants$Code)
levels(fuel_plants$Code)

#K-Means for the plants filtered with higher production
#This part begins the code to evaluate cluster with K-means method
fuel_plants_spread <- spread(fuel_plants, key = "SettlementPeriod", 
                             value = "EnergySupply")

#We look for NA in each both long and spread df
colSums(is.na(fuel_plants))
colSums(is.na(fuel_plants_spread))

#We can set the NA to 0 as the line 52 DOES or we can delete those rows
#fuel_spread <- na.omit(fuel_spread)

#There area few NA on the the fuel_spread DF this is given that we have hours with no data.
#We now change all the NA found in the spread dataset for zero given we cannot do k-means with empty valeus
fuel_plants_spread[is.na(fuel_plants_spread)] <- 0

#We first begin evaluating the number of cluster and how this effect the sum of squares from 1 to 20 cluster
store_ss <- vector("numeric",length=20) 
for(i in 1:20){
  foo <- kmeans(fuel_plants_spread[,3:50],(i+1))
  store_ss[i] <- foo$tot.withinss
}
plot(2:21,store_ss,'b')


#We now begin our cluster, we are interested in developing K number of cluster
foo <- kmeans(fuel_plants_spread[,3:50],n_cluster) 
names(foo)
foo$size
#This results corresponds to the amount of curves that go to each cluster

#Now we want to know the shape of this K clusters
fuel_plants_kmeans <- data.frame(foo$centers)
fuel_plants_kmeans$Cluster <- 1:n_cluster
mu_plants_long <- gather(fuel_plants_kmeans,"Time","MWh",1:48)
mu_plants_long$Time <- as.numeric(gsub("X","",(mu_plants_long$Time))) 

ggplot(mu_plants_long)+geom_line(aes(x=Time,y=MWh,group=Cluster,col=factor(Cluster)))+
  labs(y= "Energy Generation (MWh)", x = "Settlement Period")+
  scale_colour_manual(name="Cluster", values=c("red", "dark green", "blue", "purple"))+  
  ggtitle("Wind power plants with lowest generation")+
  theme(title = element_text(size=18, face = "bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16))


#Now we want to know the shape of all the curves that fit each cluster
fuel_plants_spread$Cluster <- foo$cluster
fuel_plants_cluster_long <- gather(fuel_plants_spread, Time,MWh,3:50)
fuel_plants_cluster_long$Time <- as.numeric(gsub("X","",(fuel_plants_cluster_long$Time))) 
fuel_plants_cluster_long$SettlementDate <- ymd(fuel_plants_cluster_long$SettlementDate)
fuel_plants_cluster_long$day_of_week <- wday(fuel_plants_cluster_long$SettlementDate,label=TRUE)

labels <- c("1" = "Cluster 1", "2" = "Cluster 2", "3" = "Cluster 3", "4" = "Cluster 4", "5" = "Cluster 5",
            "6" = "Cluster 6")

ggplot(fuel_plants_cluster_long)+geom_line(aes(x=Time,y=MWh, group=interaction(SettlementDate)), 
                                           alpha=0.5, se = F, size = 0.1, colour = "black")+
  facet_wrap(~Cluster,ncol=2, labeller = labeller(Cluster = labels)) + 
  ggtitle("Wind power plants with lowest generation")+
  labs(y="Energy Generation (MWh)", x = "Settlement Period")+
  theme(title = element_text(size=18, face = "bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16),
        strip.text.x = element_text(size = 16))


table(fuel_plants_cluster_long[,c(3,6)])/48
fuel_plants_cluster_long$mth <- month(fuel_plants_cluster_long$SettlementDate,label=TRUE) 
fuel_plants_cluster_long$mth <- factor(fuel_plants_cluster_long$mth, levels = month.abb) 
t(table(fuel_plants_cluster_long[,c(3,7)])/48)

a <- as.data.frame.matrix(t(table(fuel_plants_cluster_long[,c(3,7)])/48))

year_results <- data.frame(table(fuel_plants_cluster_long[,c(3,7)])/48)
year_results$Cluster <- factor(year_results$Cluster)

ggplot(year_results)+geom_line(aes(x=mth,y=Freq,group=Cluster,col=factor(Cluster)))+
  labs(y= "Number of Days", x = "Month")+
  scale_colour_manual(name="Cluster", values=c("red", "dark green", "blue", "purple"))+  
  ggtitle("COAL power plants with highest generation")+
  theme(title = element_text(size=18, face = "bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16))






