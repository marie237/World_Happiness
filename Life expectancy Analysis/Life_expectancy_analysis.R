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

#color settings
color <- brewer.pal(5, 'YlOrRd')
display.brewer.pal(5, 'YlOrRd')

#Importing data from WHO and WHR
setwd(getwd())

WHO_Country_data <- read.csv("WHO_Country_life_expectancy_data.csv")
WHO_Continent_data <- read.csv("WHO_Continent_life_expectancy_data.csv")
WHR_data <- read_excel("World_happinness_report_data_2021.xls") 


#Study of correlation between happinness and healthy life expectancy
ggplot(WHR_data, aes(`Healthy life expectancy at birth`, `Life Ladder`)) +
  geom_point(alpha = 0.4, color='red', size=2) + 
  geom_smooth(method = "lm", se = FALSE) +
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
life_outliers_country <- rep("Others", length(WHR_data$`Country name`))
life_outliers_country[is_singapore] <- "Singapore"
life_outliers_country[is_haiti] <- "Haiti"
#adding the variable to the frame
WHR_data_mod_life <- cbind(WHR_data, life_outliers_country)
#plotting
ggplot(WHR_data_mod_life, aes(`Healthy life expectancy at birth`, `Life Ladder`, color = `life_outliers_country`, alpha = `life_outliers_country`)) +
  geom_point(aes(size = `life_outliers_country`))+
  labs(x = "Healthy Life Expectancy",y = "Happiness")+
  scale_color_manual(values = c(color[5], color[2], color[3]))+
  scale_size_manual(values = c(3, 2, 3))+
  scale_alpha_manual(values = c(1, 0.1, 1))+
  theme_bw()

                      


above_8 <- which(WHR_data$`Life Ladder` >= 8)
happy_outliers_8 <- WHR_data[above_8,]
below_2.5 <- which(WHR_data$`Life Ladder` <2.5)
happy_outliers_2.5 <- WHR_data[below_2.5,]


