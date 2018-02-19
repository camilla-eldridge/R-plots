#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

input<-args[1]
plot_title <-args[2]
yintercept<-args[3]
shaded<-args[4]
output_file_name<-args[5]
pos<-args[6]

library(ggplot2)

X_and_Y1<-read.csv(input, header = TRUE)
intercept<-as.numeric(yintercept)

attach(X_and_Y1)

T<-ggplot() +
  geom_line(data = X_and_Y1, aes(x = X , y = Y1, group = Label, colour=Label)) + 
  #facet_wrap(~Species)+# for individual plots
  theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  theme(panel.border= element_blank()) +  geom_hline(yintercept = intercept, size=0.2) + labs(title=plot_title) + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.key = element_blank()) + theme(legend.title = element_text(colour="black", size=8))


seq_length<-max(X_and_Y1$X)
V<-rep(0, seq_length)

runme<-paste("V[c(",pos,")]","<-1")    #makes string for command and eval runs it
eval(parse(text=runme))

inds<- diff(c(0, V))
start<-X_and_Y1$X[inds ==1]
end<-X_and_Y1$X[inds == -1]

if (length(start) > length(end)) end <- c(end, tail(X_and_Y1$X, 1))
rects<-data.frame(start=start, end=end, group=seq_along(start))

s<-T + geom_rect(data=rects, inherit.aes = FALSE, aes(xmin=start, xmax=end, ymax=max(X_and_Y1$Y1), ymin=min(X_and_Y1$Y1), fill=shaded), colour = "transparent", alpha=0.06) 
y<-s + scale_fill_manual("", values = "pink", guide = guide_legend(override.aes = list(alpha = 0.3)))
y + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

ggsave(output_file_name, device = "png", width = 35, height = 10, units = "cm")

