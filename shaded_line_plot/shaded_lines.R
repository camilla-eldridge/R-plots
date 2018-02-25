#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(grid)
library(gtable)
library(ggplot2)

input_main<-args[1]
plot_title <-args[2]
intercept<-args[3]
shaded<-args[4]
pos<-args[5]
output_plot_name<-args[6]

grid.newpage()

### read in csv ##
X_and_Y1<-read.csv(input_main, header = TRUE)
### make y intercept numeric ###
yintercept<-as.numeric(intercept)
### make df from input_main ###
df<-data.frame(X_and_Y1)

### get column names
Lab<-names(df)[1]
x_axis<-names(df)[2]
y_axis<-names(df)[3]

## assign new names to dataframe columns ###
j <-df[1][[Lab]] 
k<-df[2][[x_axis]]
l<-df[3][[y_axis]]

### Make initial plot from input_main ###
T<-ggplot() +
  geom_line(data = X_and_Y1, aes_string(x = x_axis , y = y_axis, group = Lab, colour=Lab), size=0.2) + 
  #facet_wrap(~Label)+ # for individual plots
  theme_bw()  + theme(plot.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank() ) + theme(panel.border= element_blank()) +  geom_hline(yintercept = yintercept, size=0.2) + labs(title=plot_title) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.key = element_blank()) + theme(legend.title = element_text(colour="black", size=8))

### Get max length for x axis data needed for rects ## 
seq_length<-max(k)  
#### makes empty vector of 0's with length max(k) ###
V<-rep(0, seq_length)

#makes string for command and eval runs it (allows args input for shaded regions) ##
runme<-paste("V[c(",pos,")]","<-1")    
eval(parse(text=runme))

#### places 1's for tm regions and defines rects start stop regions ####
inds<- diff(c(0, V))
start<-k[inds == 1]
end<-k[inds == -1]

### make rects data frame to define region to shade ####
if (length(start) > length(end)) end <- c(end, tail(k, 1))
rects<-data.frame(start=start, end=end, group=seq_along(start))

### Add shaded regions to previous plot ##
s<-T + geom_rect(data=rects, inherit.aes = FALSE, aes(xmin=start, xmax=end, ymax=max(l), ymin=min(l), fill=shaded), colour = "transparent",alpha=0.1) 
y<-s + scale_fill_manual("", values = "pink", guide = guide_legend(override.aes = list(alpha = 0.3)))
G<-y + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

## save it ##
ggsave(output_plot_name, device = "png", width = 35, height = 10, units = "cm")


