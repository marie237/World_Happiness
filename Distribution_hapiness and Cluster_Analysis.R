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
library(hrbrthemes)
library(viridis)
library(forcats)
library(RColorBrewer)
library(plyr)

#Choose a palette
color <- brewer.pal(5, 'YlOrRd')
display.brewer.pal(5, 'YlOrRd')
bigcolor <- brewer.pal(9,'YlOrRd')

#Import data 
whr2021 <- read_excel("~/Desktop/whr2021.xls")

#Select only year 2019 
data <- data.frame(whr2021$`Country name`,whr2021$year,whr2021$`Life Ladder`,whr2021$`Log GDP per capita`,whr2021$`Social support`,whr2021$`Healthy life expectancy at birth`,whr2021$`Perceptions of corruption`,whr2021$`Positive affect`,whr2021$`Negative affect`,whr2021$`Freedom to make life choices`)
data2019 <- data[which(data$whr2021.year == 2019),]
row.names(data2019) <- 1:144
data2019$country <- rep(0,144)

#Distribution in South America
data_2019_SA <- data2019[c(4,14,17,24,26,34,103,104,138,140,29,36,47,49,83,94,102),]
data2019$country[c(4,14,17,24,26,34,103,104,138,140,29,36,47,49,83,94,102)] <- 1
data_2019_SA$whr2021..Country.name. <- substr(data_2019_SA$whr2021..Country.name.,1,3)
summary(data_2019_SA$whr2021..Life.Ladder.)
ggplot(data_2019_SA,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[3],alpha=1,width=0.5) + theme_bw() + labs(title = " South America",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),
                                                                                                                                                                                                                             axis.text = element_text(size = 15))


#Distribution in Africa 
data_2019_AF <- data[c(3,13,16,19,21,23,27,28,35,38,41,42,45,48,64,71,72,73,76,77,79,81,82,87,88,90,9596,110,112,114,118,127,129,130,133,143,144,59,60,122),]
data2019$country[c(3,13,16,19,21,23,27,28,35,38,41,42,45,48,64,71,72,73,76,77,79,81,82,87,88,90,95,96,110,112,114,118,127,129,130,133,143,144,59,60,122)] <- 2
data_2019_AF$whr2021..Country.name. <- substr(data_2019_AF$whr2021..Country.name.,1,4)
summary(data_2019_AF$whr2021..Life.Ladder.)
ggplot(data_2019_AF,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[3],alpha=0.75,width=0.5) + theme_bw() + labs(title = " Africa",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),axis.text = element_text(size = 15,angle=90))


#Distribution in Europe 
data_2019_EU <- data[c(2,7,11,12,15,18,30,32,37,39,40,44,46,51,52,56,58,69,74,75,80,84,86,92,98,99,106,107,108,109,113,116,117,120,123,124,134,136,43),]
data2019$country[c(2,7,11,12,15,18,30,32,37,39,40,44,46,51,52,56,58,69,74,75,80,84,86,92,98,99,106,107,108,109,113,116,117,120,123,124,134,136,43)] <- 3
data_2019_EU$whr2021..Country.name. <- substr(data_2019_EU$whr2021..Country.name.,1,5)
summary(data_2019_EU$whr2021..Life.Ladder.)
ggplot(data_2019_EU,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[3],alpha=0.5,width=0.5) + theme_bw() + labs(title = " Europe",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),
                                                                                                                                                                                                                        axis.text = element_text(size = 15,angle=90))

#Distribution in South_East Asia 
data_2019_South_East_Asia <- data [c(20,68,78,89,128,141,54,105,115),]
data2019$country[c(20,68,78,89,128,141,54,105,115)] <- 5
data_2019_South_East_Asia$whr2021..Country.name. <- substr(data_2019_South_East_Asia$whr2021..Country.name.,1,4)
summary(data_2019_South_East_Asia)
ggplot(data_2019_South_East_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[4],alpha=1,width=0.5) + theme_bw() + labs(title = " South East Asia",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),
                                                                                                                                                                                                                                            axis.text = element_text(size = 15))

#Distribution in Central Asia 
data_2019_Central_Asia <- data[c(1,126,139,132,67,63),]
data2019$country[c(1,126,139,132,67,63)] <- 6
data_2019_Central_Asia$whr2021..Country.name. <- substr(data_2019_Central_Asia$whr2021..Country.name.,1,4)
summary(data_2019_Central_Asia)
ggplot(data_2019_Central_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[4],alpha=0.75,width=0.5) + theme_bw() + labs(title = " Central Asia",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),
                                                                                                                                                                                                                                         axis.text = element_text(size = 15))

#Distribution in East Asia 
data_2019_East_Asia <- data[c(25,50,61,85,119,125,31,65,70,97,101,111,142),]
data2019$country[c(25,50,61,85,119,125,31,65,70,97,101,111,142)] <- 4
data_2019_East_Asia$whr2021..Country.name. <- substr(data_2019_East_Asia$whr2021..Country.name.,1,4)
summary(data_2019_East_Asia)
ggplot(data_2019_East_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[4],alpha=0.5,width=0.5) + theme_bw() + labs(title = " East Asia",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),
                                                                                                                                                                                                                                  axis.text = element_text(size = 15))


#Distribution in South Asia 
data_2019_South_Asia <- data[c(10,53,91,100,121,135),]
data2019$country[c(10,53,91,100,121,135)] <- 7
data_2019_South_Asia$whr2021..Country.name. <- substr(data_2019_South_Asia$whr2021..Country.name.,1,4)
summary(data_2019_South_Asia)
ggplot(data_2019_South_Asia,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[5],alpha=1,width=0.5) + theme_bw() + labs(title = " South Asia",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),
                                                                                                                                                                                                                                  axis.text = element_text(size = 15))
#Distribution in Western Asia 
data_2019_Western_Asia <- data[c(5,8,9,55,57,62,66,131),]
data2019$country[c(5,8,9,55,57,62,66,131)] <- 8
summary(data_2019_Western_Asia)

ggplot(data2019[1:50,],aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.,fill=as.factor(country))) + geom_col() + coord_flip() + scale_fill_manual(values = bigcolor)

ggplot(data,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.,group=continent)) + geom_col(aes(fill=continent)) + facet_wrap(~continent,ncol =7) 

#Repartition for different GDP ( low, medium,high)
data2019$wealth <- rep(0,144)
data_2019_low <- data2019[which(data2019$whr2021..Log.GDP.per.capita. < 8.564),]
data_2019_low$whr2021..Country.name. <- substr(data_2019_low$whr2021..Country.name.,1,6)
summary(data_2019_low$whr2021..Life.Ladder.)
ggplot(data_2019_low,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.))+ geom_col(col="black",fill=color[2],alpha=1,width=0.5) + theme_bw() + labs(title = " Low GDP",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),axis.text = element_text(size = 15,angle=90))

data_2019_medium <- data2019[which(data2019$whr2021..Log.GDP.per.capita. > 8.564 & data2019$whr2021..Log.GDP.per.capita. < 10.445),]
data_2019_medium$whr2021..Country.name. <- substr(data_2019_medium$whr2021..Country.name.,1,5)
summary(data_2019_medium$whr2021..Life.Ladder.)
ggplot(data_2019_medium,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[3],alpha=1,width=0.5) + theme_bw() + labs(title = " Medium GDP",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),axis.text = element_text(size = 6,angle=90))

data_2019_high <- data2019[which(data2019$whr2021..Log.GDP.per.capita. > 10.445),]
data_2019_high$whr2021..Country.name. <- substr(data_2019_high$whr2021..Country.name.,1,8)
summary(data_2019_high$whr2021..Life.Ladder.)
ggplot(data_2019_high,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(col="black",fill=color[4],alpha=1,width=0.5) + theme_bw() + labs(title = " High GDP",x="Countries",y="Index of hapiness") + theme(axis.title = element_text(size = 15),axis.text = element_text(size = 15,angle=90))

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
data_country_2019 <- data.frame(data2019$whr2021..Country.name.,data2019$whr2021..Life.Ladder.,data2019$whr2021..Log.GDP.per.capita.,data2019$whr2021..Social.support.,data2019$whr2021..Healthy.life.expectancy.at.birth.,data2019$whr2021..Perceptions.of.corruption.,data2019$whr2021..Freedom.to.make.life.choices.,data2019$whr2021..Negative.affect.,data2019$whr2021..Positive.affect.)
data_country_2019 <- na.omit(data_country_2019)
data_cluster_2019 <- data_country_2019[-1]

set.seed(1245)
kluster <- kmeans(data_cluster_2019,centers=5)
data_country_2019$cluster <- as.factor(kluster$cluster)
table(kluster$cluster)
ggplot(data_country_2019,aes(x=cluster,y=data2019.whr2021..Life.Ladder.,col=cluster)) + geom_point(alpha=0.6) + geom_jitter() + labs(x="Clusters",y="Index of hapiness",colour="clusters")+ scale_color_manual(values = color)

#Optimal number of cluster
fviz_nbclust(data_cluster_2019,kmeans,method="wss") + geom_vline(xintercept = 4,linetype=2) + labs(subtitle="Elbow method")

#Repeat clustering with 4 clusters
data_country_2019_2 <- data_country_2019
data_cluster_2019_2 <- data_country_2019_2[-1]
kluster4 <- kmeans(data_cluster_2019_2,centers=4)
data_country_2019_2$cluster <- as.factor(kluster4$cluster)
table(data_country_2019_2$cluster)

#Group by the cluster assignement and calculate averages 
clus4_average <- data_country_2019_2 %>% group_by(cluster) %>% summarize_if(is.numeric,mean,na.rm=TRUE)
clus4_average <- rename(clus4_average, c("data2019.whr2021..Life.Ladder."= "Index of Hapiness","data2019.whr2021..Log.GDP.per.capita."="Log GDP","data2019.whr2021..Social.support."="Social Support","data2019.whr2021..Healthy.life.expectancy.at.birth."= "Healthy life","data2019.whr2021..Perceptions.of.corruption."="Perception of corruption","data2019.whr2021..Freedom.to.make.life.choices."="Freedom","data2019.whr2021..Negative.affect."="Negative affect","data2019.whr2021..Positive.affect."="Positive affect"))
ggparcoord(clus4_average, columns= c(2:9), groupColumn ="cluster",scale="uniminmax",order="skewness") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + scale_colour_manual("cluster",,labels=levels(clus4_average$cluster),values=color[1:4]) + labs(x="Features",y="Values")

#Cluster 1 
data_country_2019_2%>% filter(cluster==1) %>% select(data2019.whr2021..Country.name.)

#Cluster 2
data_country_2019_2 %>% filter(cluster==2) %>% select(data2019.whr2021..Country.name.)

#Cluster3
data_country_2019_2 %>% filter(cluster==3) %>% select(data2019.whr2021..Country.name.)

#Cluster 4
data_country_2019_2 %>% filter(cluster==4) %>% select(data2019.whr2021..Country.name.)

#Map for clustering
 #Use data_country_2019_2 and take the cluster assignments and list of countries 


