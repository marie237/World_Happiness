library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyr)
library(NbClust)
library(GGally)
library(tmap)
library(ggplot2)
library(grid)
library(lattice)


rm(list = ls())

#Import data 
whr2021 <- read_excel("/Users/raphaelmirallie/Documents/EPFL/aMA_3/CSV/Report_2/DataPanelWHR2021C2.xls")

#------------------------------------------------------------------------------
#Split the data wrt year
data <- split(whr2021, whr2021$year) #we get data from 2005 to 2020

#------------------------------------------------------------------------------
#Boxplot of ladder per year

ggplot(whr2021, aes(group = year, x = factor(year), y =  `Life Ladder`)) +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..),width = 0.25) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..),width = 0.25) +
  xlab("Year") +
  scale_x_discrete(breaks = c("2005","2010","2015","2020"))+
  geom_boxplot(linetype = "dashed", outlier.shape = 1) +
  geom_boxplot(aes(ymin=..lower.., ymax=..upper..))

#------------------------------------------------------------------------------
#Keep only country that are present in year 2019

#data_2019 <- data[15]
#data_2019 <- data.frame(data_2019)
#country_keep <- data_2019[,c(1)]

#reduced_data_19 <- whr2021[whr2021$`Country name` %in% country_keep,]
#reduced_data_19_split <- split(reduced_data_19, reduced_data_19$year)


#------------------------------------------------------------------------------
#Keep only country that are present in all year (without 2005,2006,2020)
data_2 <- data[-c(1,2,16)] 

list_countries <- data_2[[1]]$`Country name`
for(n in 2:length(data_2)){
 countries <- data_2[[n]]$`Country name`
 list_countries <- intersect(list_countries,countries)
}
    #We only keep 65 countries

rm(countries,n)

reduced_data <- whr2021[whr2021$`Country name` %in% list_countries,]
#reduced_data <- reduced_data[!(reduced_data$year %in% c(2005, 2006, 2020)),]
reduced_data <- droplevels(reduced_data[!reduced_data$year %in% c(2005, 2006, 2020),])
reduced_data_split <- split(reduced_data, reduced_data$year)
reduced_data_split <- reduced_data_split[-c(1,2,16)]


#------------------------------------------------------------------------------
#Boxplot and information of Life Ladder on those countries

ls <- list()
for (n in 1:length(reduced_data_split)){
  quant <- quantile(reduced_data_split[[n]]$'Life Ladder')
  ls[[n]] <- quant
}

rm(quant,n)

ggplot(reduced_data, aes(group = year, x = factor(year), y =  `Life Ladder`)) +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..),width = 0.25) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..),width = 0.25) +
  xlab("Year") +
  scale_x_discrete(breaks = c("2007","2010","2013","2016","2019"))+
  geom_boxplot(linetype = "dashed", outlier.shape = 1) +
  geom_boxplot(aes(ymin=..lower.., ymax=..upper..))

#------------------------------------------------------------------------------
#Ladder index per country

data_per_country <- split(reduced_data, reduced_data$`Country name`)

#barplot(data_per_country[[1]]$`Life Ladder`, names.arg = data_per_country[[1]]$year)

#par(mfrow = c(5, 3))
p <- list()
for (n in 1:length(data_per_country)){
  
  thing <- ggplot(data_per_country[[n]], aes( x = factor(year), y =  `Life Ladder`)) + 
    geom_bar(stat="identity") +
    ggtitle(data_per_country[[n]]$`Country name`[1]) +
    scale_x_discrete(breaks = c("2007","2019"))+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) 
  #print(thing)
  
  p[[n]] <- thing
}
rm(thing,n)

#Plot the result

#do.call(grid.arrange,p[1:12])
#do.call(grid.arrange,p[25:36])
#do.call(grid.arrange,p[37:48])
#do.call(grid.arrange,p[13:24])
#do.call(grid.arrange,p[49:60])
#do.call(grid.arrange,p[61:65])





