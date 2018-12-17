library(swfscMisc)
library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(corrplot)

###### First attempt of a pca correlation matrix
gns3 <- gns3[, -c(7)]
View(gns3)
gns3 <- gns3[, -c(6,7)]
generation_unified2 <- generation_unified
generation_unified2$MAX_CAPACITY <- as.numeric(gsub(",", "", generation_unified2$MAX_CAPACITY))
generation_unified2 <- subset(generation_unified2, !is.na(Price))
gns2 <- generation_unified2[,-c(2,4,5,8:14,16)]
gns2$SettlementPeriod <- as.numeric(gns2$SettlementPeriod)
gns2$MAX_CAPACITY <- as.numeric(gns2$MAX_CAPACITY)
gns2$EnergySupply <- as.numeric(as.character(gns2$EnergySupply))
gns2 <- subset(gns2, !is.na(Price))
summary(gns2)
na.count(gns2)
gns4 <- scale(gns2)
pca <- prcomp(gns4,center = FALSE,scale=FALSE)
plot(pca)
corrplot(pca$rotation)







Generation_Range_New <- generationrange[, c(5,8,9,12)]
Generation_Range_New2 <- Generation_Range_New%>% spread(`Market Generation Unit EIC Code`,Quantity)
generation_price2 <- left_join(Generation_Range_New2,price_range, by = c("V3", "V4"))
names(generation_price2)[221] <- "Price"
names(generation_price2)[222] <- "Volume"
names(generation_price2)[1] <- "SettlementDate"
names(generation_price2)[2] <- "SettlementPeriod"
generation_price2 <- generation_price2[,-c(219,220)]
generation_price2 <- generation_price2 [, c(1,2,219,220,3:218)]
generation_price2 <- subset(generation_price2, !is.na(Price))
generation_price2 <- subset(generation_price2, !is.na(Volume))
summary(generation_price2$Price)
summary(generation_price2$Volume)
na.count(generation_price2)

generation_price2$`48W000000DINO-5O` <- as.character(generation_price2$`48W000000DINO-5O`)
generation_price2$`48W000000DINO-5O`[is.na(generation_price2$`48W000000DINO-5O`)] <-0
generation_price2$`48W000000DINO-5O` <- as.numeric(generation_price2$`48W000000DINO-5O`)

generation_price2[20] <- as.character(generation_price2[20])
generation_price2[20] <- as.numeric(gsub(",", "", generation_price2[20]))
generation_price2[20][is.na(generation_price2[20])] <-0
generation_price2[20] <- as.numeric(generation_price2[20])
summary(genera)
View(generation_price2)
