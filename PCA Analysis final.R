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
###############################
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