library(ggplot2)
library(gridExtra)
library(ggpubr)


#  NOTE: need to change Threshold lable to neutral for charge predictions..
line_plot<-function(data_in, x, y, groups, yintercept){
  attach(data_in)  
  ggplot() +
    geom_line(data = data_in, aes(x = x  , y = y, group = groups, colour=groups)) + theme_classic() +
    geom_hline(yintercept = yintercept, size=0.2) + annotate("text", min(as.numeric(y)), yintercept, vjust = -1, hjust = 0.3, label = paste("Threshold", as.character(yintercept))) +
    labs(color="Protein variant")
}

# CD63 

# IFN-gamma
cd63_ifn<-read.csv("CD63_IFNepitope_prediction.csv", header = TRUE)
CD63_IFN<-line_plot(cd63_ifn, Peptide, Score, Protein, -0.3)
CD63_IFN2<-CD63_IFN + labs(title = "IFN-gamma epitope prediction for St-CD63 protein variants") + theme(plot.title = element_text(hjust = 0.5)) + ylab(label = "Epitope score") + xlab(label = "Window (width = 9aa)")


# IL-4  - Multi
cd63_il4<-read.csv("IL_4_pred_CD63_variants.csv", header = TRUE)
CD63_IL4<-line_plot(cd63_il4, Window, SVM.score, Protein,0.2)
CD63_IL42<-CD63_IL4 + labs(title = "IL-4 epitope prediction for St-CD63 protein variants") + theme(plot.title = element_text(hjust = 0.5)) + ylab(label = "Epitope score") + xlab(label = "Window (width = 8aa)")


# Bepipred
cd63_bepi<-read.csv("CD63_bepipred.csv", header = TRUE)
cd63_BEPI<-line_plot(cd63_bepi, AminoAcid, EpitopeProbability, Protein, 0.5)
cd63_BEPI2<-cd63_BEPI + labs(title = "Bepipred B-cell epitope prediction for St-CD63 protein variants") + theme(plot.title = element_text(hjust = 0.5)) + ylab(label = "Epitope score") + xlab(label = "Amino Acid position")


# Charge
cd63_char<-read.csv("IL_4_pred_CD63_variants.csv", header = TRUE)
CD63_char<-line_plot(cd63_char, Window, Charge, Protein,0.0)
CD63_char2<-CD63_char + labs(title = "Charge predictions for St-CD63 protein variants") + theme(plot.title = element_text(hjust = 0.5)) + ylab(label = "Charge") + xlab(label = "Window (width = 8aa)")
CD63_char2


ggarrange(cd63_BEPI2, CD63_IL42, CD63_IFN2, CD63_char2, common.legend = TRUE, labels = c('I', 'II', 'III', 'IV'),  ncol = 2, nrow = 2) 