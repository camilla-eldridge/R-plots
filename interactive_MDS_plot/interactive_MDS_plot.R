library(pegas)
library(plotly)

#  Plot Distance matrix of sequences and colour by a group variable

# Read distance matrix from Clustalo 
distance_matrix<-read.csv("test.csv", row.names = 1) 

#weighted euclidean distance
dist_eu <- as.matrix(dist(distance_matrix, method = "euclidean"))
mds_eu <- as.data.frame(cmdscale(dist_eu))
mds_eu$names <- rownames(mds_eu)
mds_eu<- mds_eu[order(mds_eu$names),]

#  merge with Species info
label_table<-read.csv("test_labels.csv", header = TRUE)
colnames(mds_eu)<- c("V1", "V2", "Species")
mds_final<-merge(mds_eu, label_table)
mds_df<-data.frame(mds_final)

# Make interactive plot for MDS of genetic distance (raw) and colour points by group
attach(mds_df)
mds_plotly<-plot_ly(data = mds_df, x = V1, y = V2, mode = "markers", type='scatter', color = Group, text = Species)
mds_plotly 
