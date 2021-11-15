library(data.table)
library(tmaptools)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(Rcpp)
library(tmap) 
library(leaflet) 
library(ggplot2)
library(RColorBrewer)


rm(list = ls())

#Data
data(World)

#Helper for manual writing
country <- World[,c('iso_a3','name')]
country <- st_drop_geometry(country)
write.csv(country,'~/Desktop/country.csv', row.names = FALSE)

#Import cluster
data_cluster <- read.csv('/Users/raphaelmirallie/Documents/EPFL/aMA_3/CSV/Report_2/cluster_cla.csv',header=FALSE)
cluster <- c(data_cluster)
cluster <- cluster[[1]]
new_world <- cbind(World,cluster)
new_world <- new_world[-c(7,8),]
rm(data_cluster, cluster)

#Plot the map
color <- brewer.pal(5,'YlOrRd')
color[1] <- '#f2f0f0'

map <- tm_shape(new_world) + tm_polygons('cluster',
                                            title = 'Cluster',
                                            legend.reverse=TRUE,
                                            palette = color)

map

