#This code test k-means for only wind farms during January
#The results are hard to interpret for hourly basis which is why I analized the curve of generation per month

library(tidyverse)
library(lubridate)

df <- read.csv("generation_price_unit_JAN2017.csv")
df <- select(df, -c(1))
df <- select(df, SettlementDate, SettlementPeriod, everything())

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

#K-Means analysis per hour
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

#Now we try K-means per day
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
