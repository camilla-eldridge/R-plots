#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(ggplot2)
library(ggmap)

haplotype_csv<-args[1]
output_plot_name<-args[2]

register_google(key = "AIzaSyOhYSCEscgQ_KE6iRqu80-XnKjBsG973WM")
coords<-read.csv(haplotype_csv, header = TRUE)
attach(coords)

world_map<-get_stamenmap(bbox = c(left = -20, bottom = 20, right = 170, top = 75), zoom = 4, maptype = "terrain-background")
map<-ggmap(world_map, crs = "EPSG:4326")
dist_map_d<-map +  geom_point(data = coords, aes(x = Latitude, y = Longitude, color =  Haplogroup ) , shape = 18, size = 4)
J<-dist_map_d + labs(color = "Haplogroup") + xlab("Longitude") + ylab("Latitude") + theme(legend.key = element_blank()) + theme(legend.text = element_text(face = "italic"))
J
ggsave(output_plot_name, device = "png", width = 35, height = 10, units = "cm")
