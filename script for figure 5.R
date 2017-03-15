######################################################################
# P-hacking plot: how right skew showing evidential value can be masked 
#if there is a high proportion of p-hacked and low statistical power

# Dorothy Bishop idea: 29/11/2015
# Written by Paul Thompson 01/12/2015
######################################################################

#read data from .csv file, generated from Luke Holman's ghost p-hacking program. 

fig5.data<-read.csv("H:/DVMB/P hack/fig5data.csv")

#set up function to calculate the percentages of p-values <0.05 (Reviewer's request - D.Lakens)
perc.fun<-function(v,n){as.vector(v/rep(unname(tapply(v, (seq_along(v)-1) %/% n, sum)),each=n)*100)}
fig5.data$Percentage<-perc.fun(fig5.data$Count,10)#save percentage values for plot as an additional column in the data frame.

fig5.data$N<-factor(fig5.data$N)#convert to factor so that plot format is correct.

library(ggplot2) #load R library required for plotting

#the following plot is relatively standard in ggplot2 and has minor adjustments to the labelling of X axis, font size, aesthetic changes to legend.

ggplot(fig5.data, aes(x = as.numeric(Bin), y = Count,col=N,lty=p_hacked)) +
geom_line(lwd = 1) +theme_bw()+ theme(legend.position="top",legend.key = element_rect(colour = NA),text = element_text(size=16, face="bold"),legend.text = element_text(size = 14, face = "bold"),axis.text.x = element_text(angle = 45, hjust = 1))+
xlab("Bin") + ggtitle("P-hacked vs Not p-hacked") + scale_x_continuous(breaks=1:10, labels=levels(fig5.data$Bin)[1:10])+
guides(linetype=guide_legend(title=NULL))  

