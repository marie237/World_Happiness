library(base)
library(datasets)
library(graphics)
library(grDevices)
library(ggplot2)
library(methods)
library(RColorBrewer)
library(stats)
library(utils)
library(readxl)
library(cluster)
library(NbClust)
library(factoextra)
library(dplyr)



#color settings
color <- brewer.pal(5, 'YlOrRd')
display.brewer.pal(5, 'YlOrRd')

#Importing data from WHO and WHR
getwd()
setwd(getwd())

WHO_Country_data <- read.csv("WHO_Country_life_expectancy_data.csv")
WHO_Continent_data <- read.csv("WHO_Continent_life_expectancy_data.csv")
WHR_data <- read_excel("World_happinness_report_data_2021.xls") 


#Study of correlation between happinness and healthy life expectancy
ggplot(WHR_data, aes(`Healthy life expectancy at birth`, `Life Ladder`)) +
  geom_point(alpha = 0.4, color= color[3], size=2) + 
  theme_bw()+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size=20))+
  geom_smooth(method = "lm", se = FALSE, col = color[5]) +
  labs(x = "Healthy Life Expectancy",y = "Happiness")

res_pearson <- cor.test(WHR_data$`Life Ladder`, WHR_data$`Healthy life expectancy at birth`, method = "pearson")
res_pearson
res_spearman <- cor.test(WHR_data$`Life Ladder`, WHR_data$`Healthy life expectancy at birth`, method = "spearman")
res_spearman

#discussion about outliers

#for low and high values of life expectancy
below_40 <- which(WHR_data$`Healthy life expectancy at birth` < 40)
life_outliers_40 <- WHR_data[below_40,]
above_76 <- which(WHR_data$`Healthy life expectancy at birth` >= 76)
life_outliers_76 <- WHR_data[above_76,]

#scatters plot with singapore and haiti highlighted
#creation of  variable to distinguish the wanted countries
is_singapore <- which(WHR_data$`Country name` == "Singapore")
is_haiti <- which(WHR_data$`Country name` == "Haiti")

Countries <- rep("Others", length(WHR_data$`Country name`))
Countries[is_singapore] <- "Singapore"
Countries[is_haiti] <- "Haiti"

#adding the variable to the frame
WHR_data_mod_life <- cbind(WHR_data, Countries)

#plotting
ggplot(WHR_data_mod_life, aes(`Healthy life expectancy at birth`, `Life Ladder`, color = `Countries`, alpha = `Countries`)) +
  geom_point(aes(size = `Countries`))+
  labs(x = "Healthy Life Expectancy",y = "Happiness")+
  theme_bw()+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  theme(legend.position = c(0.14, 0.8), legend.text = element_text(size = 20), legend.title = element_text(size=25, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour =color[5]))+
  scale_color_manual(values = c(color[5], color[3], color[4]))+
  scale_size_manual(values = c(3, 2, 3))+
  scale_alpha_manual(values = c(1, 0.1, 1))
  theme_bw()

                      
#COMPARISON OF LIFE AND HEALTHY LIFE EXPECTANCY
#we consider the data from WHO as they have the same years. FOr both sexe

#For haiti, Singapore and Switzerland 
singapore <- which(WHO_Country_data$X == "Singapore")
haiti <- which(WHO_Country_data$X == "Haiti")
switzerland <- which(WHO_Country_data$X == "Switzerland")

singapore_healthy_life <- WHO_Country_data$Healthy.life.expectancy..HALE..at.birth..years.[singapore]
singapore_life <- WHO_Country_data$Life.expectancy.at.birth..years.[singapore]
switzerland_healthy_life <- WHO_Country_data$Healthy.life.expectancy..HALE..at.birth..years.[switzerland]
switzerland_life <- WHO_Country_data$Life.expectancy.at.birth..years.[switzerland]
haiti_healthy_life <- WHO_Country_data$Healthy.life.expectancy..HALE..at.birth..years.[haiti]
haiti_life <- WHO_Country_data$Life.expectancy.at.birth..years.[haiti]


#plotting

date <- c(2000, 2010, 2015, 2019)
#as there will be many plots, I created a function to plot automatically
makeplot <- function(y_1, y_2, ylim, x_leg, y_leg){
    
      # y_1 : first data points, linked to life expectancy
      # y_2 : second data points, linked to healthy life expectancy
      # ylim : range of the y axis, as it changes in between countries -cluster
      # x_leg, y_leg : position of the legend
  
  plot(y = y_1, x= date, ylim = ylim, xlim = c(2000, 2020), 
       type = 'o', pch=20, col = color[2], lwd = 4, cex = 2,
       xlab = 'Year', ylab = 'Age', cex.lab = 2, cex.axis = 1.7)
  points(y= y_2, x= date, 
         type = 'o', pch=20, col = color[3], lwd = 4, cex =2)
  legend(x = x_leg, y = y_leg, legend = c("Life Expectancy", "Healthy Life Expectancy"), col=c(color[2], color[3]), lty = 1, cex =1.5, bty = "n")
}


#singapore
makeplot(rev(singapore_life), rev(singapore_healthy_life), c(60, 90), 2013, 90)


#haiti
makeplot(rev(haiti_life), rev(haiti_healthy_life), c(20, 80), 2013, 80)

#Switzerland
makeplot(rev(switzerland_life), rev(switzerland_healthy_life), c(60, 90), 2013, 90)


#Clustering of Clarisse
#imoporting data used by Clarisse
data <- data.frame(WHR_data$`Country name`,WHR_data$year,WHR_data$`Life Ladder`, WHR_data$`Freedom to make life choices`, WHR_data$`Positive affect`, WHR_data$`Negative affect`,WHR_data$`Log GDP per capita`, WHR_data$`Social support`,WHR_data$`Healthy life expectancy at birth`,WHR_data$`Perceptions of corruption`)
data2019 <- data[which(data$WHR_data.year == 2019),]

#Clustering with K-means algorithm: 
data_country_2019 <- data.frame(data2019$WHR_data..Country.name.,data2019$WHR_data..Life.Ladder.,data2019$WHR_data..Log.GDP.per.capita.,
                                data2019$WHR_data..Social.support.,data2019$WHR_data..Healthy.life.expectancy.at.birth.,data2019$WHR_data..Perceptions.of.corruption.,data2019$WHR_data..Freedom.to.make.life.choices.,data2019$WHR_data..Negative.affect.,data2019$WHR_data..Positive.affect.)
data_country_2019 <- na.omit(data_country_2019)
data_cluster_2019_2 <- data_country_2019[-1]


#Repeat clustering with 4 clusters
kluster4 <- kmeans(data_cluster_2019_2,centers=4)
data_country_2019$cluster <- as.factor(kluster4$cluster)
table(data_country_2019$cluster)

#Group of country per cluster
cluster_1 <- which(data_country_2019$cluster == 1)
country_cluster_1 <- data2019$WHR_data..Country.name.[cluster_1]

cluster_2 <- which(data_country_2019$cluster == 2)
country_cluster_2 <- data2019$WHR_data..Country.name.[cluster_2]

cluster_3 <- which(data_country_2019$cluster == 3)
country_cluster_3 <- data2019$WHR_data..Country.name.[cluster_3]

cluster_4 <- which(data_country_2019$cluster == 4)
country_cluster_4 <- data2019$WHR_data..Country.name.[cluster_4]


#extracting the data for each cluster in the WHO table
WHO_data <- data.frame(WHO_Country_data$X, WHO_Country_data$X.1, WHO_Country_data$Life.expectancy.at.birth..years., WHO_Country_data$Healthy.life.expectancy..HALE..at.birth..years.)
WHO_cluster_1 <- WHO_data[which(WHO_data$WHO_Country_data.X %in% country_cluster_1),] #everything except Tanzania
WHO_cluster_1 <- WHO_cluster_1[which(WHO_cluster_1$WHO_Country_data.X.1 %in% date),]

WHO_cluster_2 <- WHO_data[which(WHO_data$WHO_Country_data.X %in% country_cluster_2),] #everything except Hong Kong, Kosovo, laos, Niger, Palestina
WHO_cluster_2 <- WHO_cluster_2[which(WHO_cluster_2$WHO_Country_data.X.1 %in% date),]

WHO_cluster_3 <- WHO_data[which(WHO_data$WHO_Country_data.X %in% country_cluster_3),] #everything except Bolivia, Iran, Ivory coast, South Korea, Taiwan
WHO_cluster_3 <- WHO_cluster_3[which(WHO_cluster_3$WHO_Country_data.X.1 %in% date),]

WHO_cluster_4 <- WHO_data[which(WHO_data$WHO_Country_data.X %in% country_cluster_4),] #everything except Congo(Brazaville), Moldova, North Cyprus, Russia, Swazyland
WHO_cluster_4 <- WHO_cluster_4[which(WHO_cluster_4$WHO_Country_data.X.1 %in% date),]

#averaging for each year
WHO_cluster_1_average <- WHO_cluster_1[,-1] %>% group_by(WHO_Country_data.X.1) %>% summarise(Life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Life.expectancy.at.birth..years.)), Healthy.life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Healthy.life.expectancy..HALE..at.birth..years.)))
WHO_cluster_2_average <- WHO_cluster_2[,-1] %>% group_by(WHO_Country_data.X.1) %>% summarise(Life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Life.expectancy.at.birth..years.)), Healthy.life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Healthy.life.expectancy..HALE..at.birth..years.)))
WHO_cluster_3_average <- WHO_cluster_3[,-1] %>% group_by(WHO_Country_data.X.1) %>% summarise(Life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Life.expectancy.at.birth..years.)), Healthy.life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Healthy.life.expectancy..HALE..at.birth..years.)))
WHO_cluster_4_average <- WHO_cluster_4[,-1] %>% group_by(WHO_Country_data.X.1) %>% summarise(Life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Life.expectancy.at.birth..years.)), Healthy.life.expectancy.at.birth. = mean(as.numeric(WHO_Country_data.Healthy.life.expectancy..HALE..at.birth..years.)))


#plotting
#cluster 1
makeplot(y_1 = WHO_cluster_1_average$Life.expectancy.at.birth., y_2 = WHO_cluster_1_average$Healthy.life.expectancy.at.birth., 
         ylim = c(50, 80), x_leg = 2013, y_leg = 82)

#cluster 2
makeplot(y_1 = WHO_cluster_2_average$Life.expectancy.at.birth., y_2 = WHO_cluster_2_average$Healthy.life.expectancy.at.birth., 
         ylim = c(50, 80), x_leg = 2013, y_leg = 80)

#cluster 3
makeplot(y_1 = WHO_cluster_3_average$Life.expectancy.at.birth., y_2 = WHO_cluster_3_average$Healthy.life.expectancy.at.birth., 
         ylim = c(50, 80), x_leg = 2013, y_leg = 80)

#cluster 4
makeplot(y_1 = WHO_cluster_4_average$Life.expectancy.at.birth., y_2 = WHO_cluster_4_average$Healthy.life.expectancy.at.birth., 
         ylim = c(50, 80), x_leg = 2013, y_leg = 80)

#global
global <- which(WHO_Continent_data$X == "Global")
global_healthy_life <- WHO_Continent_data$Healthy.life.expectancy..HALE..at.birth..years.[global]
global_life <- WHO_Continent_data$Life.expectancy.at.birth..years.[global]

makeplot(y_1 = rev(global_life), y_2 = rev(global_healthy_life), ylim = c(50, 80), x_leg = 2013, y_leg = 80)




