#This code test k-means for only wind farms during January
#The results are hard to interpret for hourly basis which is why I analized the curve of generation per month

library(tidyverse)
library(lubridate)

df <- read.csv("generation_price_unit_JAN2017.csv")
df <- select(df, -c(1))
df <- select(df, SettlementDate, SettlementPeriod, everything())
names(df)[4] = "Code"
summary(df)
#The following line give us which type of fuels we can filter in the database
levels(df$BMRA_FUEL_TYPE)
#We can choose within the following fuels in the UK:
#BIOMASS" "CCGT"    "COAL"    "NPSHYD"  "NUCLEAR" "OCGT"    "PS"      "WIND"   

#We will now develop a K-means general method for any type of FUEL
#Define the variables that you want to analize
#INPUTS
fuel_type <- "COAL"
n_cluster <- 4
#Use when FILTER BY DATE
Date_k <- "2017-01-01"
#Use when FILTER BY CODE
#n_code <- "48W0000000ABTH7Y"


#Type of filter
#Per FUEL
fuel_long <- filter(df, BMRA_FUEL_TYPE == fuel_type)
#Per MONTH or DAY
fuel_long <- filter(fuel_long, day(SettlementDate) == day(Date_k))
#Per CODE
#fuel_long <- filter(fuel_long, Code == n_code)

# 1. K-MEANS -----------------------------------------------------------------
#This part begins the code to evaluate cluster with K-means method
fuel_long <- subset(fuel_long, select = c(1,2,3,4))
fuel_spread <- spread(fuel_long, key = "SettlementPeriod", value = "EnergySupply")

#We look for NA in each both long and spread df
colSums(is.na(fuel_long))
colSums(is.na(fuel_spread))

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
fuel_kmeans$Cluster <- 1:4
mu_long <- gather(fuel_kmeans,"Time","MWh",1:48)
mu_long$Time <- as.numeric(gsub("X","",(mu_long$Time))) 
ggplot(mu_long)+geom_line(aes(x=Time,y=MWh,group=Cluster,col=factor(Cluster)))


#Now we want to know the shape of all the curves that fit each cluster
fuel_spread$Cluster <- foo$cluster
fuel_cluster_long <- gather(fuel_spread, Time,MWh,3:50)
fuel_cluster_long$Time <- as.numeric(gsub("X","",(fuel_cluster_long$Time))) 
fuel_cluster_long$SettlementDate <- ymd(fuel_cluster_long$SettlementDate)

ggplot(fuel_cluster_long)+geom_point(aes(x=(Time),y=MWh,group=SettlementDate), alpha=.5)+
  facet_wrap(~Cluster,ncol=2)




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

#K-Means for the price
price <- subset(df, select = c(1,2,6))
price <- unique(price)
price_spread <- spread(price, key = "SettlementPeriod", value = "Price")
price_spread[is.na(price_spread)] <- 0


foo2 <- kmeans(price_spread[,2:49],4) 
names(foo2)

foo2$size

df3 <- data.frame(foo2$centers)
df3$cluster <- 1:4
mu_long2 <- gather(df3,"time","price",1:48)
mu_long2$time <- as.numeric(gsub("X","",(mu_long2$time))) 
ggplot(mu_long2)+geom_line(aes(x=time,y=price,group=cluster,col=factor(cluster)))


price_spread$cluster <- foo2$cluster
price_long <- gather(price_spread,time,price,2:49)
price_long$time <- as.numeric(gsub("X","",(price_long$time))) 
price_long$SettlementDate <- ymd(price_long$SettlementDate)

ggplot(price_long)+geom_line(aes(x=(time),y=price,group=SettlementDate), alpha=.5)+
  facet_wrap(~cluster,ncol=2) + ggtitle("Price Cluster January")

#We want to see first the total generation of each fuel for january
tapply(df$EnergySupply, df$BMRA_FUEL_TYPE, FUN=sum)
sum(df$EnergySupply)

