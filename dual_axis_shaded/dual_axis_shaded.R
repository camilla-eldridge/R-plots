#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(ggplot2)
library(grid)
library(gtable)

grid.newpage()

input_Y1<-args[1]
input_Y2<-args[2]
plot_title <-args[3]
intercept<-args[4]
shaded<-args[5]
pos<-args[6]
output_file_name<-args[7]

X_and_Y1<-read.csv(input_Y1, header = TRUE)
X_and_Y2<-read.csv(input_Y2,  header = TRUE)
yintercept<-as.numeric(intercept)

attach(X_and_Y1)

T<-ggplot() +
  geom_line(data = X_and_Y1, aes(x = X , y = Y1, group = Label, colour=Label)) + 
  theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  theme(panel.border= element_blank()) +  geom_hline(yintercept = yintercept, size=0.2) + labs(title=plot_title) + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.key = element_blank()) + theme(legend.title = element_text(colour="black", size=8))

seq_length<-max(X_and_Y1$X)
V<-rep(0, seq_length)

runme<-paste("V[c(",pos,")]","<-1")    #makes string for command and eval runs it
eval(parse(text=runme))

inds<- diff(c(0, V))
start<-X_and_Y1$X[inds == 1]
end<-X_and_Y1$X[inds == -1]

if (length(start) > length(end)) end <- c(end, tail(X_and_Y1$X, 1))
rects<-data.frame(start=start, end=end, group=seq_along(start))

s<-T + geom_rect(data=rects, inherit.aes = FALSE, aes(xmin=start, xmax=end, ymax=max(X_and_Y1$Y1), ymin=min(X_and_Y1$Y1), fill=shaded), colour = "transparent", alpha=0.06) 
y<-s + scale_fill_manual("", values = "pink", guide = guide_legend(override.aes = list(alpha = 0.3)))
B<-y + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))
C<-B + theme(legend.position = 'bottom') 

Z <- ggplot(X_and_Y2, aes(x = X, y = Y2)) + geom_line(colour = "black", size=0.3, linetype = "dashed") + ylim(0,3) + ylab("Y2") + theme_classic() %+replace% theme(panel.background = element_rect(fill = NA))

#### Everything below taken from: https://stackoverflow.com/questions/36754891/ggplot2-adding-secondary-y-axis-on-top-of-a-plot ######
g1 <- ggplotGrob(C)
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

png(output_file_name,width = 1000, height = 350, units = "px") 
grid.draw(g1) 
dev.off()

