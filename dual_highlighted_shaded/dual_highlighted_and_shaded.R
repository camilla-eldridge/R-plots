#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(ggplot2)
library(grid)
library(gtable)

input_Y1<-args[1]
input_Y2<-args[2]
input_highlight<-args[3]
highlighted<-args[4]
shaded<-args[5]
plot_title <-args[6]
intercept<-args[7]
pos<-args[8]
output_plot_name<-args[9]


# ###read in csvs ###
X_and_Y1<-read.csv(input_Y1, header = TRUE)
X_and_Y2<-read.csv(input_Y2,  header = TRUE)

grid.newpage()

### read in csvs ####
#X_and_Y1<-read.csv(input_main, header = TRUE)
highlight<-read.table(input_highlight, sep="\n")

grid.newpage()

### make y intercept numeric ###
yintercept<-as.numeric(intercept)

###make df from input_main ###
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
  geom_line(data = X_and_Y1, aes_string(x = x_axis , y = y_axis, group = Lab), size=0.2) + 
  #facet_wrap(~Label)+ # for individual plots
  theme_bw()  + theme(plot.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank() ) + theme(panel.border= element_blank()) +  geom_hline(yintercept = yintercept, size=0.2) + labs(title=plot_title) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.key = element_blank()) + theme(legend.title = element_text(colour="black", size=8))

#T + labs(color=label)
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
s<-T + geom_rect(data=rects, inherit.aes = FALSE, aes(xmin=start, xmax=end, ymax=max(l), ymin=min(l), fill=shaded), colour = "transparent",alpha=0.1) 
y<-s + scale_fill_manual("", values = "pink", guide = guide_legend(override.aes = list(alpha = 0.3)))
G<-y + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

#### start the counter####
m<-0
#### make empty d #####
d = NULL
######## for each entry to be highlighted get y data for x axis range #######
for(i in highlight) {
  K <-as.character(i)
  J<-strsplit(K, " ")
  for (L in J){
    m<-m + 1
    name<-L[[2]]
    B<-as.numeric(L[[3]])
    C<-as.numeric(L[[4]])
    # #### get all x positions included in input range ###
    x_range<-seq(B, C)
    y_sub<-subset(X_and_Y1, X_and_Y1$Label==name) ## subsets by name
    #define y axis title in df - same as input names  
    yy<-y_sub[3][[y_axis]]  ## assign y axis data as yy
    y_range<-yy[c(x_range)]
    type<-rep(L[[1]], length(y_range))
    species<-rep(L[[2]], length(y_range))
    Group<-rep(m, length(y_range))
    H<-cbind(species, Group, type, x_range, y_range)
    d = rbind(d, data.frame(H))
  }
}


### add in missing axis lines ###
FM<-G + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))
###### plot highlighted lines from new data frame d, specify group as Group not type to colour by type #######
TM<-FM + geom_line(mapping = aes(x = as.numeric(as.character(d$x_range)), y = as.numeric(as.character(d$y_range)), group=d$Group, colour=d$type), lwd=0.9)
###### label highlighted lines from arg #####
GM<-TM + labs(color=highlighted) 

#### reassign for input_y2 #### test x = x , test y = y2
df2<-data.frame(X_and_Y2)

### get column names
x_axis_2<-names(df2)[1]
y_axis_2<-names(df2)[2]

## assign new names to dataframe columns ###
k2<-df2[1][[x_axis_2]]
l2<-df2[2][[y_axis_2]]

Z <- ggplot(X_and_Y2, aes_string(x = k2, y = l2)) + geom_line(colour = "black", size=0.2, linetype = "dashed") + ylim(0,4) + ylab(y_axis_2) + theme_classic() %+replace% theme(panel.background = element_rect(fill = NA))

# #### Everything below taken from: https://stackoverflow.com/questions/36754891/ggplot2-adding-secondary-y-axis-on-top-of-a-plot ######
g1 <- ggplotGrob(GM)
g2 <- ggplotGrob(Z)
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}

index <- which(g2$layout$name == "ylab-l")
ylab <- g2$grobs[[index]]
ylab <- hinvert_title_grob(ylab)

g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")

index <- which(g2$layout$name == "axis-l")
yaxis <- g2$grobs[[index]]
yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
grid.newpage()
grid.draw(g1)

### save it ####
png(output_plot_name,width = 1000, height = 250, units = "px")
grid.draw(g1)
dev.off()

### note ylim for Z changes the axis size for Y2 ####


