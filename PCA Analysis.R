library(swfscMisc)
library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(corrplot)

###### First attempt of a pca correlation gns3 <- gns3[, -c(7)]
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

generation_price_PrOnly <- scale(t(generation_price2[,c(4,6:220)]))
generation_Price_TiOnly <- scale(t(generation_price2[,c(5:220)]))





generation_price2 <- read.csv("generation_price2.csv",stringsAsFactors = F)
generation_price2 <- as.numeric(generation_price2[,c(6:221)])
generation_price3 <- generation_price2[, -c(1:3)]
generation_price4 <- scale(generation_price3)




EIC_codes <- read.csv("EIC_Codes_Generation.csv")
EIC_codes$Energy.Identification.Code <- gsub("-",".",EIC_codes$Energy.Identification.Code)
generation_price2 <- read.csv("generation_price2.csv", stringsAsFactors = F)
colnames(generation_price2) <- gsub("^X", "",  colnames(generation_price2))
generation_price2 <- generation_price2[,-c(1)]
generation_price2$SettlementDate <- (gsub("/", "", generation_price2$SettlementDate))
generation_price2$SettlementDate <- dmy(generation_price2$SettlementDate)
generation_price2 <- data.frame(generation_price2)
generation_price3 <- generation_price2[, -c(1:4)]
generation_price4 <- scale(generation_price3)
generation_price_PrOnly <- scale(generation_price2[,c(3,5:220)])
generation_Price_VolOnly <- scale(generation_price2[,c(4:220)])
generation_price_prtionly <- scale(t(generation_price2[,c(4:5)]))
summary(generation_price2$SettlementDate)


####PCA Analysis: Only Plants
Name <- paste("PCA Analysis: Only Plants")
pca2 <- prcomp(generation_price2[,c(5:220)],center = FALSE,scale=FALSE)
plot(pca2)
pca2x <- pca2$x
nrow(pca2x)
ncol(pca2x)
x <- (pca2$x[,1])
y <- (pca2$x[,2])
plot(x,y)
pca2.var <- pca2$sdev^2
pca2.var.per <- round(pca2.var/sum(pca2.var)*100,1)
barplot(pca2.var.per, main = "Scree Plot",  xlab = "Principal Component", ylab="Percent Variation")
pca2.data <- data.frame(x=pca2$x[,1], y=pca2$x[,2])
pca2.data$Counter <- 1
pca2.data$Samples <- cumsum(pca2.data$Counter)
pca2.data <- pca2.data[, c(4,1:3)]
head(pca2.data)
ggplot(data=pca2.data, aes(x=x, y=y, label = Samples))+
geom_text() +
xlab(paste("PC1 -", pca2.var.per[1], "%", sep="")) +
ylab(paste("PC2 -", pca2.var.per[2], "%", sep="")) +
theme_bw() +
ggtitle(Name)

loading_scores <- pca2$rotation[,1]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked[1:20])
PCA2Results <- Top_20_Plants
PCA2ResultsDetails <- pca2$rotation[Top_20_Plants,1]
EIC_codes$Energy.Identification.Code <- as.character(EIC_codes$Energy.Identification.Code)
s <- 1
PCA2List <- NULL
for (s in 1:length(PCA2Results)) {
  PCA2ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA2Results[s]) 
  PCA2List <- rbind(PCA2List, PCA2ListTemp)
}




str(PCA2Results)

#####PCA Analysis: only plants at scale
Name <- paste("PCA Analysis: Scaled only plants ")
pca3 <- prcomp(t(generation_price2[,c(5:220)]),center = FALSE,scale=TRUE)
str(pca3)
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
barplot(pca3.var.per, main = "Scree Plot",  xlab = "Principal Component", ylab="Percent Variation")
pca3.data <- data.frame(x=pca3$x[,1], y=pca3$x[,2])
pca3.data$Plants <- colnames(generation_price2[,(5:220)])
pca3.data <- pca3.data[, c(3,1,2)]
head(pca3.data)
ggplot(data=pca3.data, aes(x=x, y=y, label = Plants))+
geom_text() +
xlab(paste("PC1 -", pca3.var.per[1], "%", sep="")) +
ylab(paste("PC2 -", pca3.var.per[2], "%", sep="")) +
theme_bw() +
ggtitle(Name)






#####PCA Price with Plants
Name <- paste("PCA Price with Plants")
pca4 <- prcomp(t(generation_price_PrOnly),center = FALSE,scale=FALSE)
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
barplot(pca4.var.per, main = "Scree Plot",  xlab = "Principal Component", ylab="Percent Variation")
pca4.data <- data.frame(x=pca4$x[,1], y=pca4$x[,2])
pca4.data$Plants <- colnames(generation_price2[,c(3,5:220)])
pca4.data <- pca4.data[, c(3,1,2)]
head(pca4.data)
ggplot(data=pca4.data, aes(x=x, y=y, label = Plants))+
  geom_text() +
  xlab(paste("PC1 -", pca4.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca4.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle(Name)




####PCA Analisys Volume with Plants
Name <- paste("PCA Analisys Volume with Plants")
pca5 <- prcomp(t(generation_Price_VolOnly),center = FALSE,scale=FALSE)
plot(pca5)
pca5x <- pca5$x
nrow(pca5x)
ncol(pca5x)
x <- (pca5$x[,1])
y <- (pca5$x[,2])
plot(x,y)
pca5.var <- pca5$sdev^2
pca5.var.per <- round(pca5.var/sum(pca5.var)*100,1)
barplot(pca5.var.per, main = "Scree Plot",  xlab = "Principal Component", ylab="Percent Variation")
pca5.data <- data.frame(x=pca5$x[,1], y=pca5$x[,2])
pca5.data$Plants <- colnames(generation_price2[4:220])
head(pca5.data)
ggplot(data=pca5.data, aes(x=x, y=y, label = Plants))+
geom_text() +
xlab(paste("PC1 -", pca5.var.per[1], "%", sep="")) +
ylab(paste("PC2 -", pca5.var.per[2], "%", sep="")) +
theme_bw() +
ggtitle(Name)










###################################################################

####PCA Analysis: only plants at scale
Name <- paste("PCA Analysis: Scaled only plants ")
pca3 <- prcomp(generation_price2[,c(5:220)],center = FALSE,scale=TRUE)
str(pca3)
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
barplot(pca3.var.per, main = "Scree Plot",  xlab = "Principal Component", ylab="Percent Variation")
pca3.data <- data.frame(x=pca3$x[,1], y=pca3$x[,2])
pca3.data$Counter <- 1
pca3.data$Samples <- cumsum(pca3.data$Counter)
pca3.data <- pca3.data[, c(4,1:3)]
head(pca3.data)
ggplot(data=pca3.data, aes(x=x, y=y, label = Samples))+
  geom_text() +
  xlab(paste("PC1 -", pca3.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca3.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle(Name)

loading_scores <- pca3$rotation[,1]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked[1:20])
PCA3Results <- Top_20_Plants
PCA3Results
PCA3ResultsDetails <- pca3$rotation[Top_20_Plants,1]
PCA3ResultsDetails
s <- 1
PCA3List <- NULL
for (s in 1:length(PCA3Results)) {
  PCA3ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA3Results[s]) 
  PCA3List <- rbind(PCA3List, PCA3ListTemp)
}
#####PCA Price with Plants
Name <- paste("PCA Price with Plants")
pca4 <- prcomp(generation_price_PrOnly,center = FALSE,scale=FALSE)
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
barplot(pca4.var.per, main = "Scree Plot",  xlab = "Principal Component", ylab="Percent Variation")
pca4.data <- data.frame(x=pca4$x[,1], y=pca4$x[,2])
pca4.data$Counter <- 1
pca4.data$Samples <- cumsum(pca4.data$Counter)
pca4.data <- pca4.data[, c(4,1:3)]
head(pca4.data)
ggplot(data=pca4.data, aes(x=x, y=y, label = Samples))+
  geom_text() +
  xlab(paste("PC1 -", pca4.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca4.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle(Name)
loading_scores <- pca4$rotation[,1]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked[1:20])
PCA4Results <- Top_20_Plants
PCA4Results
PCA4ResultsDetails <- pca4$rotation[Top_20_Plants,1]
PCA4ResultsDetails
s <- 1
PCA4List <- NULL
for (s in 1:length(PCA4Results)) {
  PCA4ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA4Results[s]) 
  PCA4List <- rbind(PCA4List, PCA4ListTemp)
}
####PCA Analisys Volume with Plants
Name <- paste("PCA Analisys Volume with Plants")
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
barplot(pca5.var.per, main = "Scree Plot",  xlab = "Principal Component", ylab="Percent Variation")
pca5.data <- data.frame(x=pca5$x[,1], y=pca5$x[,2])
pca5.data$Counter <- 1
pca5.data$Samples <- cumsum(pca5.data$Counter)
pca5.data <- pca4.data[, c(4,1:3)]
head(pca5.data)
ggplot(data=pca5.data, aes(x=x, y=y, label = Samples))+
  geom_text() +
  xlab(paste("PC1 -", pca5.var.per[1], "%", sep="")) +
  ylab(paste("PC2 -", pca5.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle(Name)
loading_scores <- pca5$rotation[,1]
Plants_scores  <- abs(loading_scores)
Plants_scores_ranked <- sort(Plants_scores, decreasing = TRUE)
Top_20_Plants <- names(Plants_scores_ranked[1:20])
PCA5Results <- Top_20_Plants
PCA5Results
PCA5ResultsDetails <- pca5$rotation[Top_20_Plants,1]
PCA5ResultsDetails
s <- 1
PCA5List <- NULL
for (s in 1:length(PCA5Results)) {
  PCA5ListTemp <- filter(EIC_codes, Energy.Identification.Code == PCA5Results[s]) 
  PCA5List <- rbind(PCA5List, PCA5ListTemp)
}


###############################

