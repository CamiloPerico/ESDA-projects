#This code test k-means for only wind farms during January
#The results are hard to interpret for hourly basis which is why I analized the curve of generation per month

library(tidyverse)
library(lubridate)

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
fuel_type <- "CCGT"
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

# 1. K-MEANS -----------------------------------------------------------------
#This part begins the code to evaluate cluster with K-means method
fuel_long <- subset(fuel_long, select = c(1,2,3,4))
fuel_spread <- spread(fuel_long, key = "SettlementPeriod", value = "EnergySupply")

#We look for NA in each both long and spread df
colSums(is.na(fuel_long))
colSums(is.na(fuel_spread))

#We can set the NA to 0 as the line 52 DOES or we can delete those rows
fuel_spread <- na.omit(fuel_spread)

#There area few NA on the the fuel_spread DF this is given that we have hours with no data.
#We now change all the NA found in the spread dataset for zero given we cannot do k-means with empty valeus
#fuel_spread[is.na(fuel_spread)] <- 0

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


ggplot(fuel_cluster_long)+geom_line(aes(x=(Time),y=MWh,group=SettlementDate), alpha=.5)+
  facet_wrap(~Cluster,ncol=2) + ggtitle("Wind Cluster 2017")

table(fuel_cluster_long[,c(3,6)])/48
fuel_cluster_long$mth <- month(fuel_cluster_long$SettlementDate,label=TRUE) 
fuel_cluster_long$mth <- factor(fuel_cluster_long$mth, levels = month.abb) 
t(table(fuel_cluster_long[,c(3,7)])/48)



# 2. VISUALIZATION OF CURVE -----------------------------------------------
#Visualization of curves per a selected day 

visu_curves <- fuel_long

uniq_code <- unique(visu_curves$Code)
random_code <- uniq_code[sample(length(uniq_code),5)]

ind <- NULL
for(i in 1:length(random_code)){
  ind <- c(ind,which(visu_curves$Code==random_code[i])) }

ggplot(visu_curves[ind,])+ 
  geom_line(aes(x=as.numeric(SettlementPeriod),y=EnergySupply, group=Code,col=as.factor(Code)))




# 3. PRICE K-MEANS  -------------------------------------------------------

#K-Means for the price
price_dataset_long <- subset(df, select = c(1,2,6))
price_dataset_long$SettlementDate <- ymd(price_dataset_long$SettlementDate)
price_dataset_long <- unique(price_dataset_long)
# SUMMER price_dataset_long <- price_dataset_long[price_dataset_long$SettlementDate >= "2017-06-21" & price_dataset_long$SettlementDate <= "2017-09-23",]
# WINTER price_dataset_long <- price_dataset_long[price_dataset_long$SettlementDate >= "2017-01-01" & price_dataset_long$SettlementDate <= "2017-03-21",]
price_dataset_spread <- spread(price_dataset_long, key = "SettlementPeriod", value = "Price")


#Remove all Rows with one NA
price_dataset_spread <- na.omit(price_dataset_spread)

#summary(price_dataset_spread)
#We first see if we have any NA values and set them equal to zero to be able to carry the k-means
#colSums(is.na(price_dataset_spread))
#price_dataset_spread[is.na(price_dataset_spread)] <- 0

#Now we develop the K-Means
foo_price <- kmeans(price_dataset_spread[,2:49], n_cluster) 
names(foo_price)
foo_price$size

price_kmeans <- data.frame(foo_price$centers)
price_kmeans$Cluster <- 1:4
mu_long_price <- gather(price_kmeans,"Time","Price",1:48)
mu_long_price$Time <- as.numeric(gsub("X","",(mu_long_price$Time))) 
ggplot(mu_long_price)+geom_line(aes(x=Time,y=Price,group=Cluster,col=factor(Cluster)))


price_dataset_spread$Cluster <- foo_price$cluster
price_cluster_long <- gather(price_dataset_spread,Time,Price,2:49)
price_cluster_long$Time <- as.numeric(gsub("X","",(price_cluster_long$Time))) 
price_cluster_long$SettlementDate <- ymd(price_cluster_long$SettlementDate)
price_cluster_long$day_week <- wday(price_cluster_long$SettlementDate,label=TRUE)

ggplot(price_cluster_long)+geom_line(aes(x=(Time),y=Price,group=SettlementDate), alpha=.5)+
  facet_wrap(~Cluster,ncol=2) + ggtitle("Price Winter Cluster 2017")

table(price_cluster_long[,c(2,5)])/48
price_cluster_long$mth <- month(price_cluster_long$SettlementDate,label=TRUE) 
price_cluster_long$mth <- factor(price_cluster_long$mth, levels = month.abb) 
t(table(price_cluster_long[,c(2,6)])/48)

# 4. Hierarchical Clustering ----------------------------------------------

d <- dist(fuel_spread[,3:50], method = 'euclidean')
mode(d)
dim(as.matrix(d))
h <- hclust(d, method = 'single')
plot(h)


h <- hclust(d,method="average") 
plot(h)
rect.hclust(h , k = 6)
?rect.hclust
clusterCut <- cutree(h, 6) 
table(clusterCut)

fuel_spread %<>% mutate(clusterCut)

fuel_hclus_long <- gather(fuel_spread, Time, MWh, 3:50) 
fuel_hclus_long$Time <- as.numeric(gsub("X","",(fuel_hclus_long$Time))) 
fuel_hclus_long$SettlementDate <- ymd(fuel_hclus_long$SettlementDate) 

ggplot(fuel_hclus_long)+geom_line(aes(x=(Time),y=MWh,group=SettlementDate), alpha=.5)+
  facet_wrap(~clusterCut,ncol=2)


# Code not USED -----------------------------------------------------------


#We want to see first the total generation of each fuel for january
tapply(df$EnergySupply, df$BMRA_FUEL_TYPE, FUN=sum)
sum(df$EnergySupply)

#Unifying the data for a daily basis
wind_long$SettlementDate <- ymd(wind_long$SettlementDate)
byday_wind <- wind_long %>%
  mutate(day= format(SettlementDate, "%d/%m/%y"), code = Code) %>%
  group_by(day, code) %>%
  summarise(total = sum(EnergySupply))

#Now we try again to get random days
uniq_code1 <- unique(byday_wind$code)
random_code1 <- uniq_code1[sample(length(uniq_code1),5)]

ind1 <- NULL
for(i in 1:length(random_code1)){
  ind1 <- c(ind1,which(byday_wind$code==random_code1[i])) }

ggplot(byday_wind[ind1,])+ 
  geom_line(aes(x=as.factor(day),y=total, group=code,col=as.factor(code)))

#Now we try K-means WIND per day
byday_wind_spread <- spread(byday_wind, key = "day", value = "total")
byday_wind_spread[is.na(byday_wind_spread)] <- 0


store_ss1 <- vector("numeric",length=20) 
for(i in 1:20){
  foo1 <- kmeans(byday_wind_spread[,2:32],(i+1))
  store_ss1[i] <- foo1$tot.withinss
}
plot(2:21,store_ss1,'b')

foo1 <- kmeans(byday_wind_spread[,2:32],4) 
names(foo1)
foo1$size

#We now graph de 4 cluster
df2 <- data.frame(foo1$centers)
df2$cluster <- 1:4
mu_long1 <- gather(df2,"date","MwHr",1:31)
mu_long1$date <- substring(mu_long1$date,2)
ggplot(mu_long1)+geom_line(aes(x=date,y=MwHr,group=cluster,col=factor(cluster)))

#We now graph all the curves from each cluster
byday_wind_spread$cluster <- foo1$cluster
byday_wind_long <- gather(byday_wind_spread,date,MwHr,2:32)
byday_wind_long$date <- ymd(byday_wind_long$date)


ggplot(byday_wind_long)+geom_line(aes(x=(date),y=MwHr,group=code), alpha=.5)+
  facet_wrap(~cluster,ncol=2)


