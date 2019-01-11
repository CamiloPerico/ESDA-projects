library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggbiplot)
#This example will try to use PCA for the analysis of January 2017 for the fuels
#We will begin from the overall aproach examinating the total fuel consumption

df <- read.csv("generation_price_unit_JAN2017.csv")
df <- select(df, -c(1))
df <- select(df, SettlementDate, SettlementPeriod, everything())
names(df)[4] = "Code"
names(df)[14] = "Fuel"


df_allfuel <- subset(df, select = c(1:4,6,14))
df_wind <- filter(df_allfuel, Fuel == "WIND")
df_wind_spread <- spread(df_wind, key = "Code", value = "EnergySupply")
colSums(is.na(df_wind_spread))
summary(df_wind_spread)
df_wind_spread$SettlementDate <- as.Date(df_wind_spread$SettlementDate)
df_wind_spread$Fuel <- NULL
df_wind_spread$`48W00000KLGLW-1S` <- NULL
df_wind_spread$Price <- NULL

pca_wind <- prcomp(na.omit(df_wind_spread[3:62]), scale = TRUE)
summary(pca_wind)
ggbiplot(pca_wind,ellipse=TRUE,var.axes=T)

df_wind_spread[is.na(df_wind_spread)] <- 0

colSums(Filter(is.numeric, df_wind_spread))


?prcomp

# OLD CODE ----------------------------------------------------------------


#We split the total dataset in a sample with only the data we are interested
df_allfuel <- subset(df, select = c(1:4,6,14))
df_allfuel_spread <- spread(df_allfuel, key = "SettlementPeriod", value = "EnergySupply")
colSums(is.na(df_allfuel_spread))
df_allfuel_spread[is.na(df_allfuel_spread)] <- 0

#Now we begin doing the PCA where we just take the column with Periods
pca_allfuel <- prcomp(df_allfuel_spread[,4:51], center = FALSE, scale = FALSE  )
plot(pca_allfuel)
summary(pca_allfuel)
#We see that the 1st PCA account for 92% while with the second we account 96% of the variance
#We will try to reconstruct curves based in the two PCA

recon_fuel_one <- pca_allfuel$x[,40]%*%t(pca_allfuel$rotation[,40])
recon_fuel_one <- data.frame(recon_fuel_one)
recon_fuel_one$date <- df_allfuel_spread$SettlementDate
recon_fuel_one$code <- df_allfuel_spread$Code
recon_fuel_one_long <- gather(recon_fuel_one, time, MWh, 1:48)
recon_fuel_one_long$time <- as.numeric(gsub("X","",(recon_fuel_one_long$time)))

#Now we will try to reconstruct random days:
only_wind_oneplantlong$SettlementDate <- ymd(only_wind_oneplantlong$SettlementDate)
uniq_date <- unique(only_wind_oneplantlong$SettlementDate)
random_dates <- uniq_date[sample(length(uniq_date),5)] 
random_dates

ind <- NULL
for(i in 1:length(random_dates)){
  ind <- c(ind,which(only_wind_oneplantlong$SettlementDate==random_dates[i])) 
}

ggplot(recon_fuel_one_long[ind,])+ 
  geom_line(aes(x=time,y=MWh,group=date),linetype=2)+ 
  geom_line(data=df_allfuel[ind,], aes(x=SettlementPeriod,y=EnergySupply,group=SettlementDate),col='red')

#No good results too many curves

#Lets do PCA on a random plant we first get a random CODE
uniq_code <- unique(df_allfuel_spread$Code)
random_code <- uniq_code[sample(length(uniq_code),1)]

ind_code <- NULL
for(i in 1:length(random_code)){
  ind_code <- c(ind_code,which(only_one_plant_long$Code==random_code[i])) 
}

random_code

#We now filter the data for the random plant
only_one_plant <- filter(df_allfuel_spread, Code == random_code)
only_one_plant_long <- gather(only_one_plant, key = "SettlementPeriod", value = "EnergySupply", 4:51)
only_one_plant_long$SettlementPeriod <- as.numeric(only_one_plant_long$SettlementPeriod)
only_one_plant_long$SettlementDate <- ymd(only_one_plant_long$SettlementDate)


#PCA for one random plant base on CODE (47-48)
pca_oneplant <- prcomp(only_one_plant[,4:51], center = FALSE, scale = TRUE)
plot(pca_oneplant)

#Now we want to reconstruct the curve based on the PC found we this can be modified
recon_one_plant <- pca_oneplant$x[,20]%*%t(pca_oneplant$rotation[,20])
recon_one_plant <- data.frame(recon_one_plant)
recon_one_plant$date <- only_one_plant$SettlementDate
recon_one_plant$code <- only_one_plant$Code
recon_one_plant_long <- gather(recon_one_plant, time, MWh, 1:48)
recon_one_plant_long$time <- as.numeric(gsub("X","",(recon_one_plant_long$time)))

#Now we get random day to evaluate
uniq_date <- unique(only_one_plant_long$SettlementDate)
random_dates <- uniq_date[sample(length(uniq_date),3)] 
random_dates

ind_date <- NULL
for(i in 1:length(random_dates)){
  ind_date <- c(ind_date,which(only_one_plant_long$SettlementDate==random_dates[i])) 
}

ggplot(recon_one_plant_long[ind_date,])+ 
  geom_line(aes(x=time,y=MWh,group=date),linetype=2)+ 
  geom_line(data=only_one_plant_long[ind_date,], aes(x=SettlementPeriod,y=EnergySupply,group=SettlementDate),
            col='red')



#Continuar con el codigo para solo un dia y reconstruir curvas con PCA