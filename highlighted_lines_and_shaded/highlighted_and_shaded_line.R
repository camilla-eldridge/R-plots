#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(ggplot2)

input_main<-args[1]
input_highlight<-args[2]
highlighted<-args[3]
plot_title <-args[4]
intercept<-args[5]
shaded<-args[6]
pos<-args[7]
output_plot_name<-args[8]

### read in csvs ####
X_and_Y1<-read.csv(input_main, header = TRUE)
highlight<-read.table(input_highlight, sep="\n")

### make y intercept numeric ###
yintercept<-as.numeric(intercept)

###make df from input_main ###
df<-data.frame(X_and_Y1)

### get column names
Lab<-names(df)[1]
x_axis<-names(df)[2]
y_axis<-names(df)[3]

## assign new names to dataframe columns ###
j <- df[1][[Lab]] 
k<-df[2][[x_axis]]
l<-df[3][[y_axis]]

### Make initial plot from input_main ###
T<-ggplot() +
  geom_line(data = X_and_Y1, aes_string(x = x_axis , y = y_axis),size=0.2) + 
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
#### places 1's for tm regions ####
inds<- diff(c(0, V))
start<-k[inds == 1]
end<-k[inds == -1]
### make rects data frame to define regiosn to shade ####
if (length(start) > length(end)) end <- c(end, tail(k, 1))
rects<-data.frame(start=start, end=end, group=seq_along(start))

### Add shaded regions to previous plot ##
s<-T + geom_rect(data=rects, inherit.aes = FALSE, aes(xmin=start, xmax=end, ymax=max(l), ymin=min(l), fill=shaded), colour = "transparent",alpha=0.5) 
y<-s + scale_fill_manual("", values = "pink", guide = guide_legend(override.aes = list(alpha = 0.5)))
G<-y + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))


#### make a counter####
m<-0
#### make empty d #####
d = NULL
######## for each entry to be highlighted get y data for x axis range #######
for(i in highlight) {
  K <-as.character(i)
  J<-strsplit(K, " ")
  for (L in J){
    m<-m + 1
    B<-as.numeric(L[[2]])
    C<-as.numeric(L[[3]])
    #### get all x positions included in input range ###
    x_range<-seq(B, C)
    ### get y axis data in x axis range ###
    y_range<-l[c(x_range)]
    #### make ids column for new data frame ###
    type<-rep(L[[1]], length(y_range))
    ### specify Group as the unique identifier(counter) ##
    Group<-rep(m, length(y_range))
    ### bind data in new dataframe and loop to empty d
    H<-cbind(Group, type, x_range, y_range)
    d = rbind(d, data.frame(H))   
  }
}

### add in missing axis lines ###
FM<-G + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))
###### plot highlighted lines from new data frame d, specify group as Group not type to colour by type #######
TM<-FM + geom_line(mapping = aes(x = as.numeric(as.character(d$x_range)), y = as.numeric(as.character(d$y_range)), group=d$Group, colour=d$type), lwd=1.3)
###### label highlighted lines from arg #####
GM<-TM + labs(color=highlighted) 
### save it ####
ggsave(output_plot_name, device = "png", width = 35, height = 10, units = "cm")















