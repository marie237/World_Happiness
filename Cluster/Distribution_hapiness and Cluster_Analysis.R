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
sd_SA <- sd(data_2019_SA$whr2021..Life.Ladder.)
sd_SA
summary(data_2019_SA$whr2021..Life.Ladder.)

#Distribution in Africa 
data_2019_AF <- data[c(3,13,16,19,21,23,27,28,35,38,41,42,45,48,64,71,72,73,76,77,79,81,82,87,88,90,9596,110,112,114,118,127,129,130,133,143,144,59,60,122),]
data2019$country[c(3,13,16,19,21,23,27,28,35,38,41,42,45,48,64,71,72,73,76,77,79,81,82,87,88,90,95,96,110,112,114,118,127,129,130,133,143,144,59,60,122)] <- 2
data_2019_AF$whr2021..Country.name. <- substr(data_2019_AF$whr2021..Country.name.,1,4)
na.omit(data_2019_AF$whr2021..Life.Ladder.)
sd_AF <- sd(na.omit(data_2019_AF$whr2021..Life.Ladder.))
sd_AF
summary(data_2019_AF$whr2021..Life.Ladder.)

#Distribution in Europe 
data_2019_EU <- data[c(2,7,11,12,15,18,30,32,37,39,40,44,46,51,52,56,58,69,74,75,80,84,86,92,98,99,106,107,108,109,113,116,117,120,123,124,134,136,43),]
data2019$country[c(2,7,11,12,15,18,30,32,37,39,40,44,46,51,52,56,58,69,74,75,80,84,86,92,98,99,106,107,108,109,113,116,117,120,123,124,134,136,43)] <- 3
data_2019_EU$whr2021..Country.name. <- substr(data_2019_EU$whr2021..Country.name.,1,5)
sd_EU <- sd(data_2019_EU$whr2021..Life.Ladder.)
sd_EU
summary(data_2019_EU$whr2021..Life.Ladder.)

#Distribution in South_East Asia 
data_2019_South_East_Asia <- data [c(20,68,78,89,128,141,54,105,115),]
data2019$country[c(20,68,78,89,128,141,54,105,115)] <- 5
data_2019_South_East_Asia$whr2021..Country.name. <- substr(data_2019_South_East_Asia$whr2021..Country.name.,1,4)
sd_SEA <- sd(data_2019_South_East_Asia$whr2021..Life.Ladder.)
sd_SEA
summary(data_2019_South_East_Asia$whr2021..Life.Ladder.)

#Distribution in Central Asia 
data_2019_Central_Asia <- data[c(1,126,139,132,67,63),]
data2019$country[c(1,126,139,132,67,63)] <- 6
data_2019_Central_Asia$whr2021..Country.name. <- substr(data_2019_Central_Asia$whr2021..Country.name.,1,4)
sd_CA <- sd(data_2019_Central_Asia$whr2021..Life.Ladder.)
sd_CA
summary(data_2019_Central_Asia$whr2021..Life.Ladder.)

#Distribution in East Asia 
data_2019_East_Asia <- data[c(25,50,61,85,119,125,31,65,70,97,101,111,142),]
data2019$country[c(25,50,61,85,119,125,31,65,70,97,101,111,142)] <- 4
data_2019_East_Asia$whr2021..Country.name. <- substr(data_2019_East_Asia$whr2021..Country.name.,1,4)
sd_EA <- sd(data_2019_East_Asia$whr2021..Life.Ladder.)
sd_EA
summary(data_2019_East_Asia$whr2021..Life.Ladder.)

#Distribution in South Asia 
data_2019_South_Asia <- data[c(10,53,91,100,121,135),]
data2019$country[c(10,53,91,100,121,135)] <- 7
data_2019_South_Asia$whr2021..Country.name. <- substr(data_2019_South_Asia$whr2021..Country.name.,1,4)
sd_SA <- sd(data_2019_South_Asia$whr2021..Life.Ladder.)
sd_SA
summary(data_2019_South_Asia$whr2021..Life.Ladder.)

#Distribution in Western Asia 
data_2019_Western_Asia <- data[c(5,8,9,55,57,62,66,131),]
data2019$country[c(5,8,9,55,57,62,66,131)] <- 8
sd_WA <- sd(data_2019_Western_Asia$whr2021..Life.Ladder.)
sd_WA
summary(data_2019_Western_Asia$whr2021..Life.Ladder.)

#Creation of the histogram
ordering <- c(rep(1,length(data_2019_SA)),rep(2,length(data_2019_AF)),rep(3,length(data_2019_EU)),rep(4,length(data_2019_East_Asia)),rep(5,length(data_2019_South_East_Asia)),rep(6,length(data_2019_Central_Asia)),rep(7,length(data_2019_South_East_Asia)),rep(8,length(data_2019_Western_Asia)))
ordered<- data2019[order(data2019$country),]
level_order <- c("0","1","2","3","4","5","6","7","8")
ordered <- ordered %>% mutate(name=fct_reorder(whr2021..Country.name.,country)) 
ordered$whr2021..Country.name.[105] <- "Hong Kong"
ordered$whr2021..Country.name.[3] <- "Dominican Rep."
ordered$whr2021..Country.name.[30] <- "Congo"
ordered$whr2021..Country.name.[68] <- "Bosnia"
ordered$whr2021..Country.name.[114] <- "Taiwan"
ordered$whr2021..Country.name.[111] <- "Palestinian"
ordered$whr2021..Country.name.[136] <- "Arab Emirates"
ggplot(ordered[1:36,],aes(x=reorder(whr2021..Country.name.,country),y=whr2021..Life.Ladder.,fill=as.factor(country))) + geom_col() + coord_flip() + scale_fill_manual(values = c("#FFFFCC","#FFEDA0","#FED976" )) + labs(x="Countries",y="Index of hapiness",fill="Colour") + ylim(c(0,8))
ggplot(ordered[36:72,],aes(x=reorder(whr2021..Country.name.,country),y=whr2021..Life.Ladder.,fill=as.factor(country))) + geom_col() + coord_flip() + scale_fill_manual(values = c("#FED976","#FEB24C")) + labs(x="Countries",y="Index of hapiness",fill="Colour") + ylim(c(0,8)) 
ggplot(ordered[72:108,],aes(x=reorder(whr2021..Country.name.,country),y=whr2021..Life.Ladder.,fill=as.factor(country))) + geom_col() + coord_flip() + scale_fill_manual(values = c("#FEB24C","#FD8D3C")) + labs(x="Countries",y="Index of hapiness",fill="Colour") + ylim(c(0,8))  
ggplot(ordered[108:144,],aes(x=reorder(whr2021..Country.name.,country),y=whr2021..Life.Ladder.,fill=as.factor(country))) + geom_col() + coord_flip() + scale_fill_manual(values = c("#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026")) + labs(x="Countries",y="Index of hapiness",fill="Colour") + ylim(c(0,8)) 


#Repartition for different GDP ( low, medium,high)
data2019$wealth <- rep(0,144)
data_2019_low <- data2019[which(data2019$whr2021..Log.GDP.per.capita. < 9),]
data2019$group_GDP[which(data2019$whr2021..Log.GDP.per.capita. < 9)] <- 1
data_2019_low$whr2021..Country.name. <- substr(data_2019_low$whr2021..Country.name.,1,6)
sd_low <- sd(data_2019_low$whr2021..Life.Ladder.)
mean_low <- mean(data_2019_low$whr2021..Life.Ladder.)
mean_low
sd_low
summary(data_2019_low$whr2021..Life.Ladder.)
ggplot(data_2019_low,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.))+ geom_col(fill = "#FC4E2A") + labs(title = " Low GDP",x="Countries",y="Index of hapiness")  + coord_flip()

data_2019_medium <- data2019[which(data2019$whr2021..Log.GDP.per.capita. > 9 & data2019$whr2021..Log.GDP.per.capita. < 10),]
data2019$group_GDP[which(data2019$whr2021..Log.GDP.per.capita. > 9 & data2019$whr2021..Log.GDP.per.capita. < 10)] <- 2
data_2019_medium$whr2021..Country.name. <- substr(data_2019_medium$whr2021..Country.name.,1,5)
sd_medium <- sd(data_2019_medium$whr2021..Life.Ladder.)
mean_medium <- mean(data_2019_medium$whr2021..Life.Ladder.)
sd_medium
mean_medium
summary(data_2019_medium$whr2021..Life.Ladder.)
ggplot(data_2019_medium,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill="#E31A1C")+ labs(title = " Medium GDP",x="Countries",y="Index of hapiness") + coord_flip()

data_2019_high <- data2019[which(data2019$whr2021..Log.GDP.per.capita. > 10),]
data2019$group_GDP[which(data2019$whr2021..Log.GDP.per.capita. > 10)] <- 3
data_2019_high$whr2021..Country.name. <- substr(data_2019_high$whr2021..Country.name.,1,8)
sd_high <- sd(data_2019_high$whr2021..Life.Ladder.)
mean_high <- mean(data_2019_high$whr2021..Life.Ladder.)
sd_high
mean_high
summary(data_2019_high$whr2021..Life.Ladder.)
ggplot(data_2019_high,aes(x=whr2021..Country.name.,y=whr2021..Life.Ladder.)) + geom_col(fill="#BD0026") + labs(title = " High GDP",x="Countries",y="Index of hapiness") + coord_flip()


#See the differences in happiness score for the three categories ( Not used in the report)
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

# Standard deviation and mean for each cluster: 
sd_1 <- sd(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 1)])
mean_1 <- mean(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 1)])
sd_1
mean_1 

sd_2 <- sd(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 2)])
mean_2 <- mean(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 2)])
sd_2 
mean_2 

sd_3 <- sd(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 3)])
mean_3 <- mean(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 3)])
sd_3
mean_3 

sd_4 <- sd(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 4)])
mean_4 <- mean(data_country_2019_2$data2019.whr2021..Life.Ladder.[which(data_country_2019_2$cluster == 4)])
sd_4
mean_4

#Map for clustering
 #Use data_country_2019_2 and take the cluster assignments and list of countries 

data_map <- data.frame(data_country_2019_2$data2019.whr2021..Country.name.,data_country_2019_2$cluster)
write.csv(data_map,"Desktop\\data_map")


