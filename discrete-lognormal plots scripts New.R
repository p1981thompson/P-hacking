######################################################################
# P-hacking plot (discrete and lognormal) 

# DB idea: 30/11/2015
# written by Paul Thompson 01/12/2015
######################################################################

#This script generates figure 3 from the paper. It shows the complete distribution
#of p-values from ghost phacking when T-test is misapplied to non-normal data.

#loads required library for R
library(ggplot2)

######################################################################
# load output data from 'ghostphack_1_6_PT_lognormal_nophacking.R' and 'ghostphack_1_6_PT_discrete_nophacking.R'
######################################################################
#read .csv files
discrete.nophack100k_cor0<-read.csv("C:/Users/pthompson/Desktop/simres_discrete_nophack100k_cor0.csv")
lognorm.nophack100k_cor0<-read.csv("C:/Users/pthompson/Desktop/simres_lognorm_nophack100k_cor0.csv")

#generate histograms and save metadata
big.logN<-hist(lognorm.nophack100k_cor0$X1,100,plot=F)
big.DisN<-hist(discrete.nophack100k_cor0$X1,100,plot=F)

#create data.frame for metadata to plot using ggplot

sim.plot.data <- data.frame(P_value = c(big.logN$mids,big.DisN$mids),Frequency=c(big.logN$counts,big.DisN$counts),Simulation = rep(c('LogNormal','Discrete'),each = 100))

sim.plot.data <- data.frame(P_value = c(discrete.nophack100k_cor0$X1,lognorm.nophack100k_cor0$X1),Simulation = rep(c('Discrete','LogNormal'),each = 100000))

######################################################################
#ggplot plots #
######################################################################
#The plot is relatively standard ggplot. we change the certain formats to make clearer for publication.

p<-ggplot(sim.plot.data,aes(x=P_value,fill=Simulation)) + 
  geom_freqpoly(position = 'identity',aes(colour=Simulation,group=Simulation, linetype=Simulation), size=1)+
  scale_x_continuous(breaks=seq(0, 1, 0.1),limit=c(0,1), expand = c(0, 0))+
  theme_bw()+ylab("Frequency")+xlab("Observed p-value")+
  theme(legend.position="top")+ geom_vline(xintercept = c(0,0.05), linetype = "longdash") +
  theme(text = element_text(size=18),legend.key = element_blank(),legend.position="top",legend.title=element_blank())

########
#This code section adds some annotation outside the plot windows to indicate the p-curve window for clarity.
Text1 = textGrob("P-curve\n range")

# Re-draw the plot with annotation
# Text 1
p1 = p + annotation_custom(grob = Text1,  xmin = 0, xmax = .055, ymin = 16900, ymax = 16900) +
  annotation_custom(grob = linesGrob(), xmin = 0, xmax = .05, ymin = 16000, ymax = 16000) +
  annotation_custom(grob = linesGrob(), xmin = 0, xmax = 0, ymin = 15700, ymax = 16000) +
  annotation_custom(grob = linesGrob(), xmin = .05, xmax = .05, ymin = 15700, ymax = 16000)

p1

# Code to override clipping
gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)


