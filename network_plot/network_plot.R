library(ggnet)
library(network)
library(sna)
library(ggplot2)

# all observable HiC associations
net<-read.csv("network_plot_ilCosTrap1.csv", header = TRUE, sep = ",")

#entries with multiple HiC associations 
net<-data.frame(net)
names(net) <- NULL

only_multi<-NULL
X<-split(net, sort(as.numeric(rownames(net))))

for (i in X){
  if (i[[1]] != i[[2]]){    #  if col 1 is not equal to col 2
    multi<-cbind(as.character(i[[1]]),as.character(i[[2]]))
    only_multi<-rbind(only_multi, multi)
  }
}

# plot of HiC associations
ggnet2(only_multi, node.size = 6, node.color = "grey", edge.size = 1, label = TRUE, edge.color = "grey")
