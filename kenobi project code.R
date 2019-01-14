library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(shiny)
library(neuralnet)


#The following code corresponds to the kenobi group it is divided in the following sections:
#*RECOMMENDATION*: RUN EACH SECTION SEPARATELY
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
end   <- as.Date("04-01-17",format="%d-%m-%y")

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


# 2.3 PRICE K-MEANS  -------------------------------------------------------

#K-Means for the price
#New prices values
price_market <- read.csv("price_market_2017.csv")
price_market <- select(price_market, c(12,11,10))
price_market <- unique(price_market)
price_market$SettlementDate <- ymd(price_market$SettlementDate)
price_market <- filter(price_market, SettlementPeriod <=48)

# 2.31 PRICE FROM OLD Values ONLY USE WITH OLD PRICE-----
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

# 2.4 CLUSTER FOR MOST PRODUCTIVE PLANTS ----------------------------------
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





# 3. PCA ANALYSIS ---------------------------------------------------------
# 3.1 Data importing and tidying ----------------------------------------------


###### Data importing and tidying
generation_price3 <- read.csv("generation_price_unit_2017.csv", stringsAsFactors = F)
generation_price2 <- generation_price3
generation_price2 <- select(generation_price2,-c(1))
generation_price2 <- generation_price2[,c(2,3,4,1,6,7)]
head(generation_price2)
generation_price2 <- unique(generation_price2)
colnames(generation_price2)[3] <- "EIC_Code"
generation_price2 <- spread(generation_price2, key="EIC_Code", value="EnergySupply")
generation_price2[is.na(generation_price2)] <- 0
generation_price2 <- data.frame(generation_price2)
generation_price3 <- generation_price2[, -c(1:4)]
generation_price4 <- scale(generation_price3)
generation_price_PrOnly <- scale(generation_price2[,c(3,5:length(generation_price2))])
generation_Price_VolOnly <- scale(generation_price2[,c(4:length(generation_price2))])
generation_price_prtionly <- scale(t(generation_price2[,c(4:5)]))



# 3.2 P3CA Analysis: only plants at scale --------------------------------------
####PCA Analysis: only plants at scale
Name3 <- paste("PCA1 Analysis: Only Plants ")
pca3 <- NULL
pca3 <- prcomp(generation_price2[,c(5:length(generation_price2))],center = FALSE,scale=TRUE)
plot(pca3)
pca3x <- pca3$x
nrow(pca3x)
ncol(pca3x)
x <- (pca3$x[,1])
y <- (pca3$x[,2])
z <- generation_price3$Price
a <- generation_price3$Volume
plot(x,y)
pca3.var <- pca3$sdev^2
pca3.var.per <- round(pca3.var/sum(pca3.var)*100,1)
barplot(pca3.var.per[1:7], main = "Scree Plot Scenario 1 ",  xlab = "Principal Component", ylab="Percent Variation")
pca3.data <- data.frame(x=pca3$x[,1], y=pca3$x[,2])
pca3.data$Counter <- 1
pca3.data$Samples <- cumsum(pca3.data$Counter)
pca3.data <- pca3.data[, c(4,1:3)]
head(pca3.data)
A <- ggplot(data=pca3.data, aes(x=x, y=y, label = "+"))+
  geom_text() +
  xlab(paste("PC1 -", pca3.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca3.var.per[2], "%", sep="")) +
  theme_bw() + theme(axis.title=element_text(size=16,face="bold")) +theme(title=element_text(size=16,face="bold")) +
  ggtitle(Name3)
A
loading_scores <- pca3$rotation[,1]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked3 <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked3[1:20])
PCA3Results <- Top_20_Plants
PCA3Results <- gsub("^X", "",  PCA3Results)
PCA3ResultsDetails <- pca3$rotation[Top_20_Plants,1]
PCA3ResultsDetails
s <- 1
PCA3List <- NULL
for (s in 1:length(PCA3Results)) {
  PCA3ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA3Results[s]) 
  PCA3List <- rbind(PCA3List, PCA3ListTemp)
  PCA3List <- unique(PCA3List)
}
PCA3List <- PCA3List[,c(7,16,17)]
Title <- paste("Only plants scenario. Most influential Plants from",start,"to",end,".csv")
write.csv(PCA3List,Title)


# 3.3 PCA Price with Plants ---------------------------------------------------
#####PCA Price with Plants
Name4 <- paste("PCA2 Analysis: Price with Plants")
pca4 <- prcomp(generation_price_PrOnly,center = FALSE,scale=TRUE)
plot(pca4)
plot(pca4$x[,1], pca4$x[,2])
str(pca4$x[,1])
pca4x <- pca4$x
nrow(pca4x)
ncol(pca4x)
x <- (pca4$x[,1])
y <- (pca4$x[,2])
z <- generation_price2$Price
a <- generation_price2$Volume
plot(a,z)
plot(x,y)
pca4.var <- pca4$sdev^2
pca4.var.per <- round(pca4.var/sum(pca4.var)*100,1)
barplot(pca4.var.per[1:7], main = "Scree Plot Scenario 2",  xlab = "Principal Component", ylab="Percent Variation")
pca4.data <- data.frame(x=pca4$x[,1], y=pca4$x[,2])
pca4.data$Counter <- 1
pca4.data$Samples <- cumsum(pca4.data$Counter)
pca4.data <- pca4.data[, c(4,1:3)]
head(pca4.data)
B <-ggplot(data=pca4.data, aes(x=x, y=y, label = "+"))+
  geom_text() +
  xlab(paste("PC1 -", pca4.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca4.var.per[2], "%", sep="")) +
  theme_bw() + theme(axis.title=element_text(size=16,face="bold")) +theme(title=element_text(size=16,face="bold")) +
  ggtitle(Name4)  
B 
loading_scores <- pca4$rotation[,2]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked4 <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked4[1:20])
PCA4Results <- Top_20_Plants
PCA4Results <- gsub("^X", "",  PCA4Results)
PCA4Results
PCA4ResultsDetails <- pca4$rotation[Top_20_Plants,1]
PCA4ResultsDetails
s <- 1
PCA4List <- NULL
for (s in 1:length(PCA4Results)) {
  PCA4ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA4Results[s]) 
  PCA4List <- rbind(PCA4List, PCA4ListTemp)
}
PCA4List <- PCA4List[,c(7,16,17)]
PCA4List
Title <- paste("Plants and Price scenario. Most influential Plants from",start,"to",end,".csv")
write.csv(PCA4List,Title)


# 3.4 PCA Analisys Volume with Plants -----------------------------------------
####PCA Analisys Volume with Plants
PCA5 <- NULL
Name5 <- paste("PCA3 Analisys: Volume with Plants")
pca5 <- prcomp(generation_Price_VolOnly,center = FALSE,scale=FALSE)
plot(pca5)
pca5x <- pca5$x
nrow(pca5x)
ncol(pca5x)
x <- (pca5$x[,1])
y <- (pca5$x[,2])
plot(x,y)
pca5.var <- pca5$sdev^2
pca5.var.per <- round(pca5.var/sum(pca5.var)*100,1)
barplot(pca5.var.per[1:7], main = "Scree Plot Scenrario 3",  xlab = "Principal Component", ylab="Percent Variation")
pca5.data <- data.frame(x=pca5$x[,1], y=pca5$x[,2])
pca5.data$Counter <- 1
pca5.data$Samples <- cumsum(pca5.data$Counter)
pca5.data <- pca5.data[, c(4,1:3)]
head(pca5.data)
C <- ggplot(data=pca5.data, aes(x=x, y=y, label = "+"))+
  geom_text() +
  xlab(paste("PC1 -", pca5.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca5.var.per[2], "%", sep="")) +
  theme_bw() + theme(axis.title=element_text(size=16,face="bold")) +theme(title=element_text(size=16,face="bold")) +
  ggtitle(Name5)
C
loading_scores <- pca5$rotation[,1]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked5 <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked5[1:20])
PCA5Results <- Top_20_Plants
PCA5Results <- gsub("^X", "", PCA5Results)
PCA5Results
PCA5ResultsDetails <- pca5$rotation[Top_20_Plants,1]
PCA5ResultsDetails
s <- 1
PCA5List <- NULL
for (s in 1:length(PCA5Results)) {
  PCA5ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA5Results[s]) 
  PCA5List <- rbind(PCA5List, PCA5ListTemp)
}
PCA5List <- PCA5List[,c(7,16,17)]
Title <- paste("Volume with Plants scenario. Most influential Plants from",start,"to",end,".csv")
write.csv(PCA5List,Title)
PCA5List
# 3.5 PCA4 Analisys: Price & Volume with Plants -------------------------------
pca6 <- NULL
Name6 <- paste("PCA4 Analisys: Price & Volume with Plants")
pca6 <- prcomp(generation_price2[,c(3:length(generation_price2))],center = FALSE,scale=TRUE)
plot(pca6)
pca6x <- pca6$x
nrow(pca6x)
ncol(pca6x)
x <- (pca6$x[,1])
y <- (pca6$x[,2])
plot(x,y)
pca6.var <- pca6$sdev^2
pca6.var.per <- round(pca6.var/sum(pca6.var)*100,1)
barplot(pca6.var.per[1:7], main = "Scree Plot Scenario 4",  xlab = "Principal Component", ylab="Percent Variation")
pca6.data <- data.frame(x=pca6$x[,1], y=pca6$x[,2])
pca6.data$Counter <- 1
pca6.data$Samples <- cumsum(pca6.data$Counter)
pca6.data <- pca6.data[, c(4,1:3)]
head(pca6.data)
D <- ggplot(data=pca6.data, aes(x=x, y=y, label = "+"))+
  geom_text() + 
  xlab(paste("PC1 -", pca6.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca6.var.per[2], "%", sep="")) +
  theme_bw() + theme(axis.title=element_text(size=16,face="bold")) +theme(title=element_text(size=16,face="bold")) +
  ggtitle(Name6)

D 
loading_scores <- pca6$rotation[,4]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked6 <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked6[1:20])
PCA6Results <- Top_20_Plants
PCA6Results <- gsub("^X", "",  PCA6Results)
PCA6Results
PCA6ResultsDetails <- pca6$rotation[Top_20_Plants,1]
PCA6ResultsDetails
s <- 1
PCA6List <- NULL
for (s in 1:length(PCA6Results)) {
  PCA6ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA6Results[s]) 
  PCA6List <- rbind(PCA6List, PCA6ListTemp)
}

PCA6List <- PCA6List[,c(7,16,17)]
Title <- paste("Price, Volume and Plants Scenario. Most influential Plants from",start,"to",end,".csv")
write.csv(PCA6List,Title)







# 4. NEURAL NETWORK -------------------------------------------------------


# 4. 1. CODE FOR 11 2017 --------------------------------------------------------------

data1 <- read.csv('generation_price_unit_Nov2017.csv',stringsAsFactors = F)

#select target plants 
data <- data1[c(2:5,7,8,15)]
data_select1 <- subset(data,Registered.Resource.EIC.Code=="48W000000DNGB216")
data_select2 <- subset(data,Registered.Resource.EIC.Code=="48W000000SHOS-1N")
data_select3 <- subset(data,Registered.Resource.EIC.Code=="48W000000HEYM11C")
data_select4 <- subset(data,Registered.Resource.EIC.Code=="48W000000HINB-77")
data_select5 <- subset(data,Registered.Resource.EIC.Code=="48W000000HINB-85")
data_select6 <- subset(data,Registered.Resource.EIC.Code=="48W100000GRMO-1N")
data_select7 <- subset(data,Registered.Resource.EIC.Code=="48W000000HUNB-79")
data_select8 <- subset(data,Registered.Resource.EIC.Code=="48W000000LBAR-1Y")
data_select9 <- subset(data,Registered.Resource.EIC.Code=="48W000000MARK-1D")
data_select10 <- subset(data,Registered.Resource.EIC.Code=="48W000000TORN-1G")
data_select11 <- subset(data,Registered.Resource.EIC.Code=="48W000000FASN-42")
data_select12 <- subset(data,Registered.Resource.EIC.Code=="48W00000PEMB-51H")
data_select13 <- subset(data,Registered.Resource.EIC.Code=="48W000000TORN-2E")
data_select14 <- subset(data,Registered.Resource.EIC.Code=="48W100000HUMR-1N")
data_select15 <- subset(data,Registered.Resource.EIC.Code=="48W000000HRTL-16")
data_select16 <- subset(data,Registered.Resource.EIC.Code=="48W000000HEYM28W")
data_select17 <- subset(data,Registered.Resource.EIC.Code=="48W000000HEYM27Y")
data_select18 <- subset(data,Registered.Resource.EIC.Code=="48W00000DRAXX-48")
data_select19 <- subset(data,Registered.Resource.EIC.Code=="48W000000SCCL-1U")
data_select <- rbind(data_select1,data_select2,data_select3,data_select4,data_select5,data_select6,data_select7,data_select8,data_select9,data_select10,data_select11,data_select12,data_select13,data_select14,data_select15,data_select16,data_select17,data_select18,data_select19)
data_select <- data_select[,-7]


#spread datasets
data_spread <- data_select %>% spread(Registered.Resource.EIC.Code,EnergySupply)
data_spread[is.na(data_spread)]=0
tail(data_spread)

data_spread <- data_spread %>% mutate(Current_Price=Price) 
class(data_spread$`48W000000DNGB216`)



# for loop
data_spread <- as.data.frame(data_spread)
data_spread$Price <- as.numeric(data_spread$Price)
data_spread$Volume <- as.numeric(data_spread$Volume)

colnames(data_spread)[5] <- "EIC1"
colnames(data_spread)[6] <- "EIC2"
colnames(data_spread)[7] <- "EIC3"
colnames(data_spread)[8] <- "EIC4"
colnames(data_spread)[9] <- "EIC5"
colnames(data_spread)[10] <- "EIC6"
colnames(data_spread)[11] <- "EIC7"
colnames(data_spread)[12] <- "EIC8"
colnames(data_spread)[13] <- "EIC9"
colnames(data_spread)[14] <- "EIC10"
colnames(data_spread)[15] <- "EIC11"
colnames(data_spread)[16] <- "EIC12"
colnames(data_spread)[17] <- "EIC13"
colnames(data_spread)[18] <- "EIC14"
colnames(data_spread)[19] <- "EIC15"
colnames(data_spread)[20] <- "EIC16"
colnames(data_spread)[21] <- "EIC17"
colnames(data_spread)[22] <- "EIC18"
colnames(data_spread)[23] <- "EIC19"



a <- data.frame()

for(i in 1:1439){
  a[i,1] <- data_spread$Volume[i+1]
  a[i,2] <- data_spread$Price[i+1]
  a[i,3] <- data_spread$EIC1[i]
  a[i,4] <- data_spread$EIC2[i]
  a[i,5] <- data_spread$EIC3[i]
  a[i,6] <- data_spread$EIC4[i]
  a[i,7] <- data_spread$EIC5[i]
  a[i,8] <- data_spread$EIC6[i]
  a[i,9] <- data_spread$EIC7[i]
  a[i,10] <- data_spread$EIC8[i]
  a[i,11] <- data_spread$EIC9[i]
  a[i,12] <- data_spread$EIC10[i]
  a[i,13] <- data_spread$EIC11[i]
  a[i,14] <- data_spread$EIC12[i]
  a[i,15] <- data_spread$EIC13[i]
  a[i,16] <- data_spread$EIC14[i]
  a[i,17] <- data_spread$EIC15[i]
  a[i,18] <- data_spread$EIC16[i]
  a[i,19] <- data_spread$EIC17[i]
  a[i,20] <- data_spread$EIC18[i]
  a[i,21] <- data_spread$EIC19[i]
  a[i,22] <- data_spread$Current_Price[i+1]
}

names(a)[1] <- "Volume"
names(a)[2] <- "Price"
names(a)[3] <- "EIC1"
names(a)[4] <- "EIC2"
names(a)[5] <- "EIC3"
names(a)[6] <- "EIC4"
names(a)[7] <- "EIC5"
names(a)[8] <- "EIC6"
names(a)[9] <- "EIC7"
names(a)[10] <- "EIC8"
names(a)[11] <- "EIC9"
names(a)[12] <- "EIC10"
names(a)[13] <- "EIC11"
names(a)[14] <- "EIC12"
names(a)[15] <- "EIC13"
names(a)[16] <- "EIC14"
names(a)[17] <- "EIC15"
names(a)[18] <- "EIC16"
names(a)[19] <- "EIC17"
names(a)[20] <- "EIC18"
names(a)[21] <- "EIC19"
names(a)[22] <- "Current_Price"

tail(a)

#normalise data 
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

a_norm <- normalise(a)

#neural network
f <- reformulate(names(a_norm[1:21]),
                 response = "Current_Price")
f

neural_network <- neuralnet(formula = f,
                            data = a_norm,
                            hidden = c(5,3),
                            linear.output = TRUE)
plot(neural_network)
print(neural_network)

#neural network for predicted price

f11 <- reformulate(names(a_norm[1:21]),
                   response = "Predicted_price")
f11

neural_network11 <- neuralnet(formula = f11,
                              data = a_norm,
                              hidden = c(5,3),
                              linear.output = TRUE)
plot(neural_network11)
print(neural_network11)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~result tidy

a_result <- as.data.frame(neural_network$net.result)

#convert result to unnormalisation 
before_norm <- function(x){
  return(x*(max(a)-min(a))+min(a))
}
a_result1<- before_norm(a_result)

result <- cbind(a,a_result1)
tail(result)

# merge with date / periods
b <- data_spread[c(1,2)]
b <- b[c(1:1439),] 

c <- cbind(b,result)
names(c)[25] <- "Predicted_Price"
tail(c)

write.csv(c,'result.csv')

c$Month <- month(c$SettlementDate)
c$Day <- mday(c$SettlementDate)

c <- c[c(1,2,26,27,24,25,3:23)]
tail(c)

# ggplot of comparison in one day
test <- filter(c, SettlementDate == "2017-11-01")

g1 <- ggplot(test) + geom_line(aes(x=SettlementPeriod, y =Current_Price), colour="red" )+
  geom_line(aes(x=SettlementPeriod, y =Predicted_Price),colour="blue") +labs(subtitle = "Comparison on 2017-11-01")
g1

# Accuracy 
error_p <- function(x){
  return(abs(((x-c$Current_Price)/c$Current_Price)))
}
error <- error_p(c$Predicted_Price)
error <- as.data.frame(error)

c <- cbind(c,error)

ggplot(c) + geom_bar(aes(x=SettlementPeriod,y=error))+labs(subtitle = "Accuracy on 2017-11-01")#用timechange转换为小时之后横坐标为几点


?geom_bar()

#SettlementPeriod 
timechange <- read.csv("timechange.csv",stringsAsFactors = F)
time <- timechange[c(1,2)]

withtime <- left_join(c,time,by=c("SettlementPeriod"="Hour"))

#comparison plot for one month
ggplot(c)+geom_point(aes(x=Day, y =Current_Price), colour="red")+
  geom_point(aes(x=Day, y =Predicted_Price), colour="blue") +labs(subtitle = "Comparison in Nov 2017")

# Accuracy
ex <- c[c(1,2,5,6,28)] #用timechange转换为小时
ex$Predicted_Price <-  round(ex$Predicted_Price,2)
ex$error <-  round(ex$error,4)
ex$error <- ex$error*100
colnames(ex)[5] <- "Error (%)"

write.csv(ex,"result with error.csv")

a_norm <- cbind(a_norm,a_result)
colnames(a_norm)[23] <- "Predicted_price"
head(a_norm)

Error <- a_norm$Current_Price-a_norm$Predicted_price
Error <- as.data.frame(Error)

a_norm <- cbind(a_norm,Error)

#MAE
MAE <- function(Error){
  mean(abs(Error))
}
Error_MAE <- MAE(a_norm$Error)
Error_MAE <- as.data.frame(Error_MAE)

#drop_na
dropna <- drop_na(a_norm)
dropna[is.na(dropna)]=0

#MAPE
MAPE <- function(Error){
  mean(abs(Error/dropna$Current_Price))
}
Error_MAPE <- MAPE(dropna$Error)
is.infinite(Error_MAPE)

Error_MAPE <- abs(a_norm$Error/a_norm$Current_Price)*100
Error_MAPE <- mean(Error_MAPE)
Error_MAPE <- round(Error_MAPE)


# RMSE
RMSE <- function(Error){
  sqrt(mean(Error^2))
}

Error_RMSE <- RMSE(a_norm$Error)  
Error_RMSE <- as.data.frame(Error_RMSE)

#final result
final <- left_join(c,time,by=c("SettlementPeriod"="Hour"))
colnames(final)[29] <- "Time"

final <- final[c(1,2,29,3:28)]

#format time 
final$Year <- year(final$SettlementDate)
final$Month <- month(final$SettlementDate)
final$Day <- mday(final$SettlementDate)

head(final)
final <- final[c(1,2,30,4,5,3,6:29)]

class(final$Time)

final$Month <- as.factor(final$Month)
final$Time <- as.integer(final$Time)

# final[c(3,4,5,6),] %>% select(Year,Month,Day,Time) %>% mutate(data_time=make_datetime(Year,Month,Day,Time))
final_output <- final[c(1,3,6,7)]
final_output$Predicted_Price <- round(final_output$Predicted_Price,2)
write.csv(final_output,"final_result.csv")

# #ggplot for the whole month 
# ggplot(final)+geom_point(aes(x=Day, y =Current_Price), colour="red")+
#   geom_point(aes(x=Day, y =Predicted_Price), colour="blue") +labs(subtitle = "Comparison in Nov 2017")
# 
# 
# #ggplot for one day example
# sample <- filter(final, SettlementDate == "2017-11-01")
# class(sample$Time)
# sample$Time <- hm(sample$Time)
# ?hms()
# 
# 
# g1 <- ggplot(sample) + geom_line(aes(x=Time, y =Current_Price), colour="red" )+
#   geom_line(aes(x=Time, y =Predicted_Price),colour="blue") +labs(subtitle = "Comparison on 2017-11-01")
# g1


#try boxplot for RMSE
test
plot(test$Current_Price,test$Predicted_Price,col="red",main = "comparison", pch=6, cex=0.5)+abline(0,1,width=0.5)













# 4.2 CODE FOR 4 2018 ---------------------------------------------------------------------

data1 <- read.csv('generation_price_unit_April2018.csv',stringsAsFactors = F)


#select target plants 
data <- data1[c(2:5,7,8,15)]
data_select1 <- subset(data,Registered.Resource.EIC.Code=="48W000000DAMC-1M")
data_select1u <- unique(data_select1)
data_select2 <- subset(data,Registered.Resource.EIC.Code=="48W000000HRTL-24")
data_select2u <- unique(data_select2)
data_select3 <- subset(data,Registered.Resource.EIC.Code=="48W000000TORN-1G")
data_select3u <- unique(data_select3)
data_select4 <- subset(data,Registered.Resource.EIC.Code=="48W000000FAWN-1P")
data_select4u <- unique(data_select4)
data_select5 <- subset(data,Registered.Resource.EIC.Code=="48W000000SIZB-2S")
data_select5u <- unique(data_select5)
data_select6 <- subset(data,Registered.Resource.EIC.Code=="48W000000HEYM27Y")
data_select6u <- unique(data_select6)
data_select7 <- subset(data,Registered.Resource.EIC.Code=="48W100000GRMO-1N")
data_select7u <- unique(data_select7)
data_select8 <- subset(data,Registered.Resource.EIC.Code=="48W000000DNGB224")
data_select8u <- unique(data_select8)
data_select9 <- subset(data,Registered.Resource.EIC.Code=="48W000000HINB-77")
data_select9u <- unique(data_select9)
data_select10 <- subset(data,Registered.Resource.EIC.Code=="48W000000DNGB216")
data_select10u <- unique(data_select10)
data_select11 <- subset(data,Registered.Resource.EIC.Code=="48W000000TORN-2E")
data_select11u <- unique(data_select11)
data_select12 <- subset(data,Registered.Resource.EIC.Code=="48W000000HEYM12A")
data_select12u <- unique(data_select12)
data_select13 <- subset(data,Registered.Resource.EIC.Code=="48W000000SIZB-1U")
data_select13u <- unique(data_select13)
data_select14 <- subset(data,Registered.Resource.EIC.Code=="48W000000FASN-42")
data_select14u <- unique(data_select14)
data_select15 <- subset(data,Registered.Resource.EIC.Code=="48W000000SCCL-3Q")
data_select15u <- unique(data_select15)
data_select16 <- subset(data,Registered.Resource.EIC.Code=="48W100000HUMR-1N")
data_select16u <- unique(data_select16)
data_select17 <- subset(data,Registered.Resource.EIC.Code=="48W000000HUNB-87")
data_select17u <- unique(data_select17)
data_select18 <- subset(data,Registered.Resource.EIC.Code=="48W000000HEYM28W")
data_select18u <- unique(data_select18)
data_select19 <- subset(data,Registered.Resource.EIC.Code=="48W00000DRAXX-3A")
data_select19u <- unique(data_select19)
data_select <- rbind(data_select1,data_select2,data_select3,data_select4,data_select5,data_select6,data_select7,data_select8,data_select9,data_select10,data_select11,data_select12,data_select13,data_select14,data_select15,data_select16,data_select17,data_select18,data_select19)
data_select <- data_select[,-7]
data_select <- unique(data_select)
tail(data_select)

#spread datasets
data_spread <- data_select %>% spread(Registered.Resource.EIC.Code,EnergySupply)
data_spread[is.na(data_spread)]=0
tail(data_spread)

data_spread <- data_spread %>% mutate(Current_Price=Price) 
class(data_spread$`48W000000DNGB216`)

# for loop
data_spread <- as.data.frame(data_spread)
data_spread$Price <- as.numeric(data_spread$Price)
data_spread$Volume <- as.numeric(data_spread$Volume)

colnames(data_spread)[5] <- "EIC1"
colnames(data_spread)[6] <- "EIC2"
colnames(data_spread)[7] <- "EIC3"
colnames(data_spread)[8] <- "EIC4"
colnames(data_spread)[9] <- "EIC5"
colnames(data_spread)[10] <- "EIC6"
colnames(data_spread)[11] <- "EIC7"
colnames(data_spread)[12] <- "EIC8"
colnames(data_spread)[13] <- "EIC9"
colnames(data_spread)[14] <- "EIC10"
colnames(data_spread)[15] <- "EIC11"
colnames(data_spread)[16] <- "EIC12"
colnames(data_spread)[17] <- "EIC13"
colnames(data_spread)[18] <- "EIC14"
colnames(data_spread)[19] <- "EIC15"
colnames(data_spread)[20] <- "EIC16"
colnames(data_spread)[21] <- "EIC17"
colnames(data_spread)[22] <- "EIC18"
colnames(data_spread)[23] <- "EIC19"

tail(data_spread)


a <- data.frame()

for(i in 1:1403){
  a[i,1] <- data_spread$Volume[i+1]
  a[i,2] <- data_spread$Price[i+1]
  a[i,3] <- data_spread$EIC1[i]
  a[i,4] <- data_spread$EIC2[i]
  a[i,5] <- data_spread$EIC3[i]
  a[i,6] <- data_spread$EIC4[i]
  a[i,7] <- data_spread$EIC5[i]
  a[i,8] <- data_spread$EIC6[i]
  a[i,9] <- data_spread$EIC7[i]
  a[i,10] <- data_spread$EIC8[i]
  a[i,11] <- data_spread$EIC9[i]
  a[i,12] <- data_spread$EIC10[i]
  a[i,13] <- data_spread$EIC11[i]
  a[i,14] <- data_spread$EIC12[i]
  a[i,15] <- data_spread$EIC13[i]
  a[i,16] <- data_spread$EIC14[i]
  a[i,17] <- data_spread$EIC15[i]
  a[i,18] <- data_spread$EIC16[i]
  a[i,19] <- data_spread$EIC17[i]
  a[i,20] <- data_spread$EIC18[i]
  a[i,21] <- data_spread$EIC19[i]
  a[i,22] <- data_spread$Current_Price[i+1]
}

names(a)[1] <- "Volume"
names(a)[2] <- "Price"
names(a)[3] <- "EIC1"
names(a)[4] <- "EIC2"
names(a)[5] <- "EIC3"
names(a)[6] <- "EIC4"
names(a)[7] <- "EIC5"
names(a)[8] <- "EIC6"
names(a)[9] <- "EIC7"
names(a)[10] <- "EIC8"
names(a)[11] <- "EIC9"
names(a)[12] <- "EIC10"
names(a)[13] <- "EIC11"
names(a)[14] <- "EIC12"
names(a)[15] <- "EIC13"
names(a)[16] <- "EIC14"
names(a)[17] <- "EIC15"
names(a)[18] <- "EIC16"
names(a)[19] <- "EIC17"
names(a)[20] <- "EIC18"
names(a)[21] <- "EIC19"
names(a)[22] <- "Current_Price"

tail(a)

#normalise data 
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

a_norm <- normalise(a)

#example nn
sample <- sample(dim(a_norm)[1],dim(a_norm)[1]*0.4)
test_norm <- a_norm[sample,]
train_norm <- a_norm[-sample,]

nn <- neuralnet(formula = f,
                data = train_norm,
                hidden = c(5,3),
                linear.output = TRUE)
plot(nn)

predicted_result <- compute(nn,test_norm[,-22])

predict_norm <- predicted_result$net.result


test_norm <- test_norm %>% mutate(predict_price = predict_norm)
tail(test_norm)

test <- before_norm(test_norm)
head(test)

plot(test$Current_Price,test$predict_price,col="blue",pch=16,ylab="Predicted_price",xlab = "Current_price")
rmse.nn <- (sum((test$Current_Price-test$predict_price)^2)/nrow(test))^0.5


#neural network
f <- reformulate(names(a_norm[1:21]),
                 response = "Current_Price")
f

neural_network <- neuralnet(formula = f,
                            data = a_norm,
                            hidden = c(5,3),
                            linear.output = TRUE)
plot(neural_network)
print(neural_network)

#neural network in Predicted_Price
f11 <- reformulate(names(a_norm[1:21]),
                   response = "Predicted_price")
f11

neural_network1 <- neuralnet(formula = f11,
                             data = a_norm,
                             hidden = c(5,3),
                             linear.output = TRUE)
plot(neural_network1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~result tidy

a_result <- as.data.frame(neural_network$net.result)
a_norm <- cbind(a_norm,a_result)
head(a_norm)
colnames(a_norm)[23] <- "Predicted_price"

a_accuracy <- a_norm
a <- a[1:22]

#convert result to unnormalisation 
before_norm <- function(x){
  return(x*(max(a)-min(a))+min(a))
}
a_result1<- before_norm(a_result)

result <- cbind(a,a_result1)
tail(result)


# merge with date / periods
b <- data_spread[c(1,2)]
b <- b[c(1:1403),] 

c <- cbind(b,result)
names(c)[25] <- "Predicted_Price"
tail(c)

write.csv(c,'result.csv')

c$Month <- month(c$SettlementDate)
c$Day <- mday(c$SettlementDate)

c <- c[c(1,2,26,27,24,25,3:23)]
tail(c)


# ggplot of comparison in one day
test <- filter(c, SettlementDate == "2018-04-06")

g1 <- ggplot(test) + geom_line(aes(x=SettlementPeriod, y =Current_Price), colour="red" )+
  geom_line(aes(x=SettlementPeriod, y =Predicted_Price),colour="blue") +labs(subtitle = "Comparison on 2018-04-02")


# Accuracy 
accuracy <- function(x){
  return(abs(((x-c$Current_Price)/c$Current_Price)))
}
c <- c %>% mutate(Error=accuracy(c$Predicted_Price))
c <- c %>% mutate(Accuracy=1-c$Error)
tail(c)
head(c)

# RMSE

RMSE <- function(Error){
  sqrt(mean(Error^2))
}

Error <- a_accuracy$Current_Price-a_accuracy$Predicted_price
a_accuracy <- a_accuracy %>% mutate(Error=Error) 
Error_RMSE <- RMSE(a_accuracy$Error)  
Error_RMSE <- as.data.frame(Error_RMSE)

#MAE
MAE <- function(Error){
  mean(abs(Error))
}
Error_MAE <- MAE(a_accuracy$Error)
Error_MAE <- as.data.frame(Error_MAE)

#MAPE
Error_MAPE <- abs(a_accuracy$Error/a_accuracy$Current_Price)
Error_MAPE <- as.data.frame(Error_MAPE)
Error_MAPE$Error_MAPE <- abs(Error_MAPE$Error_MAPE)*100
mean_value <- mean(Error_MAPE$Error_MAPE)
sum(Error_MAPE$Error_MAPE)

Error_MAPE <- mean(Error_MAPE)
Error_MAPE <- round(Error_MAPE)

c <- cbind(c,Error)

#SettlementPeriod 
timechange <- read.csv("timechange.csv",stringsAsFactors = F)
time <- timechange[c(1,2)]

c <- left_join(c,timechange,by=)

#comparison plot for one month
ggplot(c)+geom_point(aes(x=Day, y =Current_Price), colour="red")+
  geom_point(aes(x=Day, y =Predicted_Price), colour="blue") +labs(subtitle = "Comparison in April 2018")


#export results
ex <- c[c(1,2,5,6,28)] #用timechange转换为小时
ex$Predicted_Price <-  round(ex$Predicted_Price,2)
ex$error <-  round(ex$error,4)
ex$error <- ex$error*100
colnames(ex)[5] <- "Error (%)"

write.csv(ex,"result with error.csv")

# Error_test <- filter(ex, SettlementDate == "2018-04-06")
# g2 <- ggplot(Error_test) + geom_line(aes(x=SettlementPeriod,y='Error (%)'))+scale_y_continuous(limits = c(0,20))+labs(subtitle = "Accuracy on 2017-04-02")
# g2
# g3 <- ggplot(Error_test) + geom_line(aes(x=SettlementPeriod,y='Error (%)'))+scale_y_continuous(limits = c(0,5))+labs(subtitle = "Accuracy on 2017-04-02")
# g3


#plot of comparison in one day
test
plot(test$Current_Price,test$predict_price,col="blue",main = "Comparison of 06/04/2018", pch=6, cex=0.5,xlab = "Current_Price",ylab = "Predicted_Price")+abline(0,-1,width=0.2)

#final output
final <- left_join(c,time,by=c("SettlementPeriod"="Hour"))
colnames(final)[29] <- "Time"
head(final)

final <- final[c(1,2,29,3:28)]
final_output <- final[c(1,3,6,7)]
head(final_output)
final_output$Predicted_Price <- round(final_output$Predicted_Price,2)

represent <- filter(final_output, SettlementDate == "2018-04-06")
write.csv(represent,"example_output.csv")






# 5. Visualization tool ---------------------------------------------------
library(shiny)
library(tidyverse)
#Read files
df_daily_spread <- readRDS("final_generation_2017.RDS")
info_plants <- readRDS("info_plants.RDS")

# Define UI ----
ui <- fluidPage(
  
  titlePanel("Interactive Panel for comparison of two generation units 2017 UK "),
  
  fluidRow(
    # fluidRow creates rows and columns - we can add up to 12 arguments in one FluidRow
    plotOutput('plot') 
  ),
  # we just created one line with 4 elements, each one with a width of 3! total = 12  
  fluidRow(
    
    column(4,
           align = "center",
           dateRangeInput("date_range", label=h3("Date Range"),start="2017-01-01", end="2017-12-31")),
    
    column(4,
           selectInput(inputId = "plant", label = h3("Select Plant 1"), choices = names(df_daily_spread)[c(2:236)])),
    
    column(4, 
           selectInput(inputId = "plant2", label = h3("Select Plant 2"), choices = names(df_daily_spread)[c(2:236)]))
  ),
  
  fluidRow(
    
    column(12,
           mainPanel(h2("Power Plant Information")))
  ),
  fluidRow(
    column(4,
           h4("Responsible Party 1"),textOutput("selected_var1")),
    
    column(4,
           h4("Fuel Type Plant 1"), textOutput("selected_var2")),
    column(4,
           h4("Maximum Capacity Plant 1 (MW)"), textOutput("selected_var3"))
    
  ),
  fluidRow(
    column(4,
           h4("Responsible Party 2"),textOutput("selected_var4")),
    
    column(4,
           h4("Fuel Type Plant 2"), textOutput("selected_var5")),
    column(4,
           h4("Maximum Capacity Plant 2 (MW)"), textOutput("selected_var6"))
    
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  dfInput <- reactive({
    ##subsetting is a bit tricky here to id the column on which to subset        
    select(df_daily_spread,c(1,input$plant,input$plant))
  })
  daInput <- reactive({
    ##subsetting is a bit tricky here to id the column on which to subset        
    select(df_daily_spread,c(1,input$plant2))
  })
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlot({
    df2 <- daInput()
    df1 <- dfInput()
    p <- ggplot() +
      geom_line(data= df1, aes(x = as.Date(Date), y = df1[,2], col = "Plant 1"))+
      geom_line(data= df2, aes(x = as.Date(Date), y = df2[,2], col = "Plant 2"))+
      xlim(input$date_range[1],input$date_range[2]) +
      labs(y = "Daily energy generation (MWh)",
           x = "Date")+
      theme(title = element_text(size=18, face = "bold"),
            axis.text=element_text(size=18),
            axis.title=element_text(size=16,face="bold"),
            legend.text = element_text(size=16),
            strip.text.x = element_text(size = 16))
    p
  })
  
  deInput <- reactive({
    filter(info_plants,Energy.Identification.Code == as.character(input$plant))[1,2]
    
  })
  output$selected_var1 <- renderText({ 
    paste(deInput())
  })
  
  de2Input <- reactive({
    filter(info_plants,Energy.Identification.Code == as.character(input$plant))[1,3]
    
  })
  output$selected_var2 <- renderText({ 
    paste(de2Input())
  })
  
  de3Input <- reactive({
    filter(info_plants,Energy.Identification.Code == as.character(input$plant))[1,4]
    
  })
  output$selected_var3 <- renderText({ 
    paste(de3Input())
  })
  
  de4Input <- reactive({
    filter(info_plants,Energy.Identification.Code == as.character(input$plant2))[1,2]
    
  })
  output$selected_var4 <- renderText({ 
    paste(de4Input())
  })
  
  de5Input <- reactive({
    filter(info_plants,Energy.Identification.Code == as.character(input$plant2))[1,3]
    
  })
  output$selected_var5 <- renderText({ 
    paste(de5Input())
  })
  
  de6Input <- reactive({
    filter(info_plants,Energy.Identification.Code == as.character(input$plant2))[1,4]
    
  })
  output$selected_var6 <- renderText({ 
    paste(de6Input())
  })
  
  
}

shinyApp(ui,server)


