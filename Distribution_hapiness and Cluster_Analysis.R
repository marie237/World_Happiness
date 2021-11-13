library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyr)
library(NbClust)
library(GGally)
library(tmap)

#Import data 
whr2021 <- read_excel("~/Desktop/whr2021.xls")

#Select only year 2019 
data <- data.frame(whr2021$`Country name`,whr2021$year,whr2021$`Life Ladder`,whr2021$`Log GDP per capita`,whr2021$`Social support`,whr2021$`Healthy life expectancy at birth`,whr2021$`Perceptions of corruption`)
data2019 <- data[which(data$whr2021.year == 2019),]

#Distribution in South America
data_2019_SA <- data[c(51,232,341,371,410,664.699,1366,1380,1893),]
summary(data_2019_SA$whr2021..Life.Ladder.)
ggplot(data_2019_SA,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#Distribution in Africa 
data_2019_AF <- data[c(33,174,218,258,292,512,550,588,591,635,673,904,1001,1038,1050,1076,1196,1267,1297,1467,1496,1580,1711,1736,1752,1933,1948),]
summary(data_2019_AF$whr2021..Life.Ladder.)
ggplot(data_2019_AF,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#Distribution in Europe 
data_2019_EU <- data[c(92,160,244,422,436,463,541,563,578,620,649,799,829,967,1015,1027,1087,1224,1408,1421,1440,1545,1558,1614,1651,1661,1835),]
summary(data_2019_EU$whr2021..Life.Ladder.)
ggplot(data_2019_EU,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#Distribution in South_East Asia 
data_2019_South_East_Asia <- data [c(277,761,953,1062,1394,1534,1726,1908),]
summary(data_2019_South_East_Asia)
ggplot(data_2019_South_East_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#Distribution in Central Asia 
data_2019_Central_Asia <- data[c(1696,1879,1778,944,889),]
summary(data_2019_Central_Asia)
ggplot(data_2019_Central_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#Distribution in East Asia 
data_2019_East_Asia <- data[c(356,709,859,1150,1595,1681),]
summary(data_2019_East_Asia)
ggplot(data_2019_East_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#Distribution in South Asia 
data_2019_South_Asia <- data[c(132,12,746,773,1211,1325,1628),]
summary(data_2019_South_Asia)
ggplot(data_2019_South_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#Repartition for different GDP ( low, medium,high)
data_2019_low <- data2019[which(data2019$whr2021..Log.GDP.per.capita. < 8.564),]
summary(data_2019_low$whr2021..Life.Ladder.)
ggplot(data_2019_low,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

data_2019_medium <- data2019[which(data2019$whr2021..Log.GDP.per.capita. > 8.564 & data2019$whr2021..Log.GDP.per.capita. < 10.445),]
summary(data_2019_medium$whr2021..Life.Ladder.)
ggplot(data_2019_medium,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

data_2019_high <- data2019[which(data2019$whr2021..Log.GDP.per.capita. > 10.445),]
summary(data_2019_high$whr2021..Life.Ladder.)
ggplot(data_2019_high,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill='Orange')

#See the differences in hapiness score for the three categories
mean_low <- rep(mean(data_2019_low$whr2021..Life.Ladder.),length(data_2019_low$whr2021..Life.Ladder.))
difference_low <- mean_low - data_2019_low$whr2021..Life.Ladder.
data_difference_low <- data.frame(data_2019_low$whr2021..Country.name.,difference_low)
ggplot(data_difference_low,aes(x=data_2019_low.whr2021..Country.name.,y=difference_low)) + geom_boxplot() + scale_fill_brewer(palette='YlOrRd') 

mean_medium <- rep(mean(data_2019_medium$whr2021..Life.Ladder.),length(data_2019_medium$whr2021..Life.Ladder.))
difference_medium <- mean_medium - data_2019_medium$whr2021..Life.Ladder.
data_difference_medium <- data.frame(data_2019_medium$whr2021..Country.name.,difference_medium)
ggplot(data_difference_medium,aes(x=data_2019_medium.whr2021..Country.name.,y=difference_medium)) + geom_boxplot() + scale_fill_brewer(palette='YlOrRd') 


mean_high <- rep(mean(data_2019_high$whr2021..Life.Ladder.),length(data_2019_high$whr2021..Life.Ladder.))
difference_high <- mean_high - data_2019_high$whr2021..Life.Ladder.
data_difference_high <- data.frame(data_2019_high$whr2021..Country.name.,difference_high)
ggplot(data_difference_high,aes(x=data_2019_high.whr2021..Country.name.,y=difference_high)) + geom_boxplot() + scale_fill_brewer(palette='YlOrRd') 

#Clustering with K-means algorithm: 
data_cluster_2019 <- data.frame(data2019$whr2021..Life.Ladder.,data2019$whr2021..Log.GDP.per.capita.,data2019$whr2021..Social.support.,data2019$whr2021..Healthy.life.expectancy.at.birth.,data2019$whr2021..Perceptions.of.corruption.)
row.names(data_cluster_2019) <- data2019$whr2021..Country.name.
data_cluster_2019 <- na.omit(data_cluster_2019)

set.seed(1245)
kluster <- kmeans(data_cluster_2019,centers=5)
data_cluster_2019$cluster <- as.factor(kluster$cluster)
table(kluster$cluster)
ggplot(data_cluster_2019,aes(data_cluster_2019$cluster,data_cluster_2019$data2019.whr2021..Life.Ladder.,col=cluster)) + geom_point(alpha=0.6) + geom_jitter() 

#Optimal number of cluster
fviz_nbclust(data_cluster_2019,kmeans,method="wss") + geom_vline(xintercept = 4,linetype=2) + labs(subtitle="Elbow method")

#Repeat clustering with 4 clusters
set.seed(1245)
kluster4 <- kmeans(data_cluster_2019,centers=4)
data_cluster_2019$cluster4 <- as.factor(kluster4$cluster)
table(kluster4$cluster)

#Group by the cluster assignement and calculat averages 
clus4_average <- data_cluster_2019 %>% group_by(cluster4) %>% summarize_if(is.numeric,mean,na.rm=TRUE)
ggparcoord(clus4_average, columns= c(2:6), groupColumn ="cluster4",scale="globalminmax",order="skewness") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

#Cluster 1 
data_cluster_2019 %>% filter(cluster4==1) %>% select(data2019.whr2021..Life.Ladder.)

#Cluster 2
data_cluster_2019 %>% filter(cluster4==2) %>% select(data2019.whr2021..Life.Ladder.)

#Cluster3
data_cluster_2019 %>% filter(cluster4==3) %>% select(data2019.whr2021..Life.Ladder.)

#Cluster 4
data_cluster_2019 %>% filter(cluster4==4) %>% select(data2019.whr2021..Life.Ladder.)

#Map for clustering
 


