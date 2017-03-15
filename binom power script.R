###########################################################################
#Power analysis for binomial test
# How many studies are needed in a p-curve to detect the p-hacking bump
# 
# Created by Dorothy Bishop, 26th Nov 2015
# Edited by Paul Thompson, 1st December 2015#
#
# based on http://stats.stackexchange.com/questions/38439/power-analysis-for-binomial-data-when-the-null-hypothesis-is-that-p-0
#
###########################################################################

#The program requires some inputs to generate the different levels of power based on a specified range of Ns.

n=seq(100,4000,by=50) # range of Ns to look at power for

mydat=data.frame(n,n) #set up dataframe to store data
colnames(mydat)=c('N','power') # rename columns
myl=0 #initialise loop

# Use a loop to run iterations of power for different N.

for (myn in n){               # sample size
myl=myl+1     # increment loop

p1 <- 0.5   # success probability under null hypothesis

nlow=9533     # nlow and nhi are the frequencies of p-values in the Bins 0.04 < p < 0.045 and  0.045 < p < 0.05 respectively.
              # these values taken from simulation with 100K runs
nhi=10032     # and 8 variables, N =200, covariance .8
              # These values used to estimate N p-values expected in
              # given bins with a given sample size

ntot=nlow+nhi #total frequency of p-values in both top bins.
 
est=round((nhi/ntot)*myn) # calculate the proportion based on the real data and mulitple by current N to give a threshold for binomial test.
cc=est
mydat[myl,2]=1-sum(dbinom(cc:myn, myn, p1))# power of rejecting hypothesis of no difference 

}
# Now plot the power curve based on the generated data in mydat

library(ggplot2) # required library for ggplot function
ggplot(data=mydat, aes(x=N, y=power)) + geom_line() + geom_point()+theme_bw() + 
scale_x_continuous(breaks=seq(0,4000,by=500))+ scale_y_continuous(breaks=seq(0.6,1,by=0.05)) + 
theme(text = element_text(size=18)) + xlab("Sample size, N")+geom_hline(y=0.8,col="red")+geom_vline(x=1200,col="red")

#NB The n used in myn is the number of pvalues between .04 and .05.
# This is much lower than the N studies used for the p-curve
# In simulation with these parameters, only 4% of p-values were in this range


