#This code test k-means for only wind farms during January
#The results are hard to interpret for hourly basis which is why I analized the curve of generation per month

library(tidyverse)
library(lubridate)

df <- read.csv("generation_price_unit_JAN2017.csv")
df <- select(df, -c(1))
df <- select(df, SettlementDate, SettlementPeriod, everything())
names(df)[4] = "Code"

levels(df$BMRA_FUEL_TYPE)

wind_long <- filter(df, BMRA_FUEL_TYPE =="WIND")
wind_long <- subset(wind_long, select = c(1,2,3,4))
names(wind_long)[4] = "Code"
wind <- spread(wind_long, key = "SettlementPeriod", value = "EnergySupply")

#We look for NA in each df

colSums(is.na(wind_long))
colSums(is.na(wind))
#There area few NA on the spread WIND DF
#This wind farm has some hours with NA same with others for that we change all NA for 0
#wind_BLLA_1K <- filter(wind, Code == "48W000000BLLA-1K")
wind[is.na(wind)] <- 0

#Visualization of curves
wind_sample <- filter(wind_long, SettlementDate == "2017-01-04")
uniq_code <- unique(wind_sample$Code)
random_code <- uniq_code[sample(length(uniq_code),10)]

ind <- NULL
for(i in 1:length(random_code)){
  ind <- c(ind,which(wind_sample$Code==random_code[i])) }

ggplot(wind_sample[ind,])+ 
  geom_line(aes(x=as.numeric(SettlementPeriod),y=EnergySupply, group=Code,col=as.factor(Code)))

#K-Means analysis per hour WIND
store_ss <- vector("numeric",length=20) 
for(i in 1:20){
  foo <- kmeans(wind[,3:50],(i+1))
  store_ss[i] <- foo$tot.withinss
}
plot(2:21,store_ss,'b')

foo <- kmeans(wind[,3:50],4) 
names(foo)
foo$size


df1 <- data.frame(foo$centers)
df1$cluster <- 1:4
mu_long <- gather(df1,"time","MwHr",1:48)
mu_long$time <- as.numeric(gsub("X","",(mu_long$time))) 
ggplot(mu_long)+geom_line(aes(x=time,y=MwHr,group=cluster,col=factor(cluster)))

wind$cluster <- foo$cluster
uk_wind_long <- gather(wind,time,MwHr,3:50)
uk_wind_long$time <- as.numeric(gsub("X","",(uk_wind_long$time))) 
uk_wind_long$SettlementDate <- ymd(uk_wind_long$SettlementDate)
uk_wind_long$day_of_week <- wday(uk_wind_long$SettlementDate,label=TRUE)

ggplot(uk_wind_long)+geom_line(aes(x=(time),y=MwHr,group=SettlementDate), alpha=.5)+
  facet_wrap(~cluster,ncol=2)

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

#Now we try k-means for the most importat fuel in the UK which is CCGT

df_CCGT_long <- filter(df, BMRA_FUEL_TYPE == "CCGT")
df_CCGT_long <- subset(df_CCGT_long, select = c(1,2,3,4))
df_CCGT <- spread(df_CCGT_long, key = "SettlementPeriod", value = "EnergySupply")

colSums(is.na(df_CCGT))
colSums(is.na(df_CCGT_long))
df_CCGT[is.na(df_CCGT)] <- 0

store_ss_ccgt <- vector("numeric", length = 20)
for (i in 1:20) {
  foo_ccgt <- kmeans(df_CCGT[,3:50],(i+1))
  store_ss_ccgt[i] <- foo_ccgt$tot.withinss 
}
plot(2:21, store_ss_ccgt, 'b')

foo_ccgt <- kmeans(df_CCGT[,3:50],4)
names(foo_ccgt)
foo_ccgt$size


df1_ccgt <- data.frame(foo_ccgt$centers)
df1_ccgt$cluster <- 1:4
mu_long_ccgt <- gather(df1_ccgt,"time","MwHr",1:48)
mu_long_ccgt$time <- as.numeric(gsub("X","",(mu_long_ccgt$time))) 
ggplot(mu_long_ccgt)+geom_line(aes(x=time,y=MwHr,group=cluster,col=factor(cluster))) + 
  ggtitle("CCGT Cluster Jan2017 kmeans")


df_CCGT$cluster <- foo_ccgt$cluster
df_CCGT_long <- gather(df_CCGT, time, MWh, 3:50)
df_CCGT_long$time <- as.numeric(gsub("X","",(df_CCGT_long$time)))
df_CCGT_long$SettlementDate <- ymd(df_CCGT_long$SettlementDate)


uk_wind_long$day_of_week <- wday(uk_wind_long$SettlementDate,label=TRUE)

ggplot(df_CCGT_long)+geom_line(aes(x=(time),y=MWh,group=SettlementDate), alpha=.5)+
  facet_wrap(~cluster,ncol=2)

#K means analysis for Nuclear Energy

df_nuclear_long <- filter(df, BMRA_FUEL_TYPE == "NUCLEAR")
df_nuclear_long <- subset(df_nuclear_long, select = c(1,2,3,4))
df_nuclear <- spread(df_nuclear_long, key = "SettlementPeriod", value = "EnergySupply")

colSums(is.na(df_nuclear))
df_nuclear[is.na(df_nuclear)] <- 0

store_ss_nuclear <- vector("numeric", length = 20)
for (i in 1:20) {
  foo_nuclear <- kmeans(df_nuclear[,3:50],(i+1))
  store_ss_nuclear[i] <- foo_nuclear$tot.withinss
}
plot(2:21, store_ss_nuclear, 'b')

foo_nuclear <- kmeans(df_nuclear[,3:50],4)
foo_nuclear$size

df1_nuclear <- data.frame(foo_nuclear$centers)
df1_nuclear$cluster <- 1:4
mu_long_nuclear <- gather(df1_nuclear, "time", "MWh", 1:48)
mu_long_nuclear$time <- as.numeric(gsub("X","",(mu_long_nuclear$time)))
ggplot(mu_long_nuclear)+geom_line(aes(x=time,y=MWh,group=cluster,col=factor(cluster))) + 
  ggtitle("NuclearCluster Jan2017 kmeans")

df1_ccgt <- data.frame(foo_ccgt$centers)
df1_ccgt$cluster <- 1:4
mu_long_ccgt <- gather(df1_ccgt,"time","MwHr",1:48)
mu_long_ccgt$time <- as.numeric(gsub("X","",(mu_long_ccgt$time))) 
ggplot(mu_long_ccgt)+geom_line(aes(x=time,y=MwHr,group=cluster,col=factor(cluster))) + 
  ggtitle("CCGT Cluster Jan2017 kmeans")


df_nuclear$cluster <- foo_nuclear$cluster
df_nuclear_long <- gather(df_nuclear, time, MWh, 3:50)
df_nuclear_long$time <- as.numeric(gsub("X","",(df_nuclear_long$time)))
df_nuclear_long$SettlementDate <- ymd(df_nuclear_long$SettlementDate)

ggplot(df_nuclear_long)+geom_line(aes(x=(time),y=MWh,group=SettlementDate), alpha=.5)+
  facet_wrap(~cluster,ncol=2)


#Now we try with COAL which is the 3rd fuel in the electricity matrix

df_coal_long <- filter(df, BMRA_FUEL_TYPE == "COAL")
df_coal_long <- subset(df_coal_long, select = c(1,2,3,4))
df_coal <- spread(df_coal_long, key = "SettlementPeriod", value = "EnergySupply")

colSums(is.na(df_coal))
df_coal[is.na(df_coal)] <- 0


store_ss_coal<- vector("numeric", length = 20)
for (i in 1:20) {
  foo_coal <- kmeans(df_coal[,3:50],(i+1))
  store_ss_coal[i] <- foo_coal$tot.withinss
}
plot(2:21, store_ss_coal, 'b')

foo_coal <- kmeans(df_coal[,3:50],4)
foo_coal$size

df1_coal <- data.frame(foo_coal$centers)
df1_coal$cluster <- 1:4
mu_long_coal <- gather(df1_coal, "time", "MWh", 1:48)
mu_long_coal$time <- as.numeric(gsub("X","",(mu_long_coal$time)))
ggplot(mu_long_coal)+geom_line(aes(x=time,y=MWh,group=cluster,col=factor(cluster))) + 
  ggtitle("Coal Cluster Jan2017 kmeans")


df_coal$cluster <- foo_coal$cluster
df_coal_long <- gather(df_coal, time, MWh, 3:50)
df_coal_long$time <- as.numeric(gsub("X","",(df_coal_long$time)))
df_coal_long$SettlementDate <- ymd(df_coal_long$SettlementDate)

ggplot(df_coal_long)+geom_line(aes(x=(time),y=MWh,group=SettlementDate), alpha=.5)+
  facet_wrap(~cluster,ncol=2)
