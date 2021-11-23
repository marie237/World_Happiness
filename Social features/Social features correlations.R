data <- read_excel("D:\\Documents\\données\\WHR2021.xls")

library(readxl)
library(tidyverse)
library(stringr)
library(ggplot2)
library(RColorBrewer)
color <- brewer.pal(5,'YlOrRd')
color[1] <- '#f2f0f0'

# Boxplot of the features

# Violin Plots
library(vioplot)
x1 <- data$'Social support'
x2 <- data$'Freedom to make life choices'
x3 <- data$'Perceptions of corruption'
vioplot(x1, x2, x3, names=c("Social support", "Freedom to make life choices", "Perceptions of corruption"),
        col= "red")

#first analysis of the different correlations between life ladder (happiness) and social features

#social support

p<-ggplot(data, aes(`Social support`, `Life Ladder`) ) + geom_point(color='red', size=2)
p + geom_smooth(method = "lm", se = FALSE) + labs(x = "Social support",y = "Life Ladder") + stat_smooth(color = "black", linetype="dashed")+
  ylim(0, 10)+xlim(0,1)+
  theme(text = element_text(size=20))

fit1 <- lm( `Life Ladder` ~ `Social support`, data = data)
summary(fit1)

# Apply loess function
values <- loess(`Life Ladder` ~ `Social support`, data= data)    
predict(values)

#Freedom to make life choices

p<-ggplot(data, aes(`Freedom to make life choices`, `Life Ladder`) )+ geom_point(color='red', size=2)
p + geom_smooth(method = "lm", se = FALSE) +labs(x = "Freedom to make life choices",y = "Life Ladder")+
  geom_abline(intercept = 4.1, slope = 4.16 , color="yellow", linetype="dashed", size=1.5)+
  geom_abline(intercept = 0.5, slope = 4.16 , color="orange", linetype="dashed", size=1.5)+
  ylim(0, 10)+xlim(0,1)+
  theme(text = element_text(size=20))

fit2 <- lm( `Life Ladder` ~ `Freedom to make life choices`, data = data)
summary(fit2)

data_out1 <- subset(data,  data$`Life Ladder` < 0.5+4.16*data$`Freedom to make life choices`)
data_out2 <- subset(data,  data$`Life Ladder` > 4.1+4.16*data$`Freedom to make life choices`)

#Perception of corruption

p<-ggplot(data, aes(`Perceptions of corruption`, `Life Ladder`) )+ geom_point(color='red', size=2)
p + geom_smooth(method = "lm", se = FALSE) +labs(x = "Perceptions of corruption",y = "Life Ladder") + 
  geom_abline(intercept = 4.6, slope = -2.579 , color="orange", linetype="dashed", size=1.5)+
  ylim(0, 10)+xlim(0,1)+
  theme(text = element_text(size=20))

fit3<- lm( `Life Ladder` ~ `Perceptions of corruption`, data = data)
summary(fit3)

#understanding the outliers occcuring in "corruption"

data_out3 <- subset(data,  data$`Life Ladder` < 4.6-2.579*data$`Perceptions of corruption`)

















