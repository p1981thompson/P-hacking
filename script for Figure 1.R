##########################################################################
#P curve example (Figure 1)

# written by Paul Thompson 01/12/2015
##########################################################################
# This program generates an example of a p-curve plot showing the typical curves for differnt power and effect sizes.
# The program does not use real data, instead simulates a series of t-tests under different conditions.


nruns <- 100000 #set number of simulated tests
p.val1<-p.val2<-p.val3 <-vector(mode="numeric",length=nruns) # create empty vector to store simulated p-values
.
#written as a loop to run for each iteration of simulated tests
for(i in 1:nruns){ #for each run
x<-rnorm(n = 200, mean = 0, sd = 1) #generate 100 participants for group 1
y1<-rnorm(n = 200, mean = 0, sd = 1) #generate 100 participants for group 2 (no diff)
y2<-rnorm(n = 200, mean = 0.3, sd = 1) #generate 100 participants for group 2 (with diff)
y3<-rnorm(n = 20, mean = 0.3, sd = 1) #generate 100 participants for group 2 (with diff)
p.val1[i]<-t.test(x,y1)$p.value #perform the t-test and store p.value
p.val2[i]<-t.test(x,y2)$p.value #perform the t-test and store p.value
p.val3[i]<-t.test(x[sample(1:100,20)],y3)$p.value #perform the t-test and store p.value
}

#binds all vectors of data into a single dataframe, labelling with correct power and effect.
p.val.data <- data.frame(P_value = c(p.val1,p.val2,p.val3),Effect = rep(c('Null','True: High power','True: Low power'),each = 100000))

# loads required R package for plotting
library(ggplot2)

#line plots for differnt power and effect sizes. Colors and line type indicate different power and effect sizes.

ggplot(p.val.data,aes(x=P_value,fill=Effect)) +
  geom_freqpoly(position = 'identity',aes(colour=Effect,group=Effect, linetype=Effect), size=1)+
scale_x_continuous(breaks=seq(0, 1, 0.1),limit=c(0,1))+
theme_bw()+ylab("Frequency")+xlab("Observed P-value")+
theme(legend.position="top")+theme(legend.title=element_blank(),text = element_text(size=18),legend.key = element_rect(colour = NA))











