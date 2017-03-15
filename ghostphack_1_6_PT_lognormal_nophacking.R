#-------------------------------------------------------------------------
#------------------------------------------------------------------------------
#                                   GHOSTPHACK v1.8
#------------------------------------------------------------------------------

# Simulating p-hacking by dropping of ghost variables
#   D. V. M. Bishop, started 18 Jul 2015
#
# last update: 23 Jul 2015, 
#              22 Jul 2015 additions by Paul Thompson
#              26 Aug 2015 additions by Paul Thompson
#              04 Oct 2015 additions by Paul Thompson
#-------------------------------------------------------------------------

# Simulates data from two groups on N random normal and Log-normal variables with pre-specified effect or no effect.
# Looks at the p-curve, i.e. distribution of p-values for a set of studies (one p per study)
# P-curve range is usually taken as 0 to .05: allows one to test if uniform, right-skewed (evidence of true effect)
# or left-skewed close to .05 (evidence of extreme p-hacking)

# See Simonsohn, U., Nelson, L. D., & Simmons, J. P. (2014).P-Curve: A key to the file-drawer. 
# Journal of Experimental Psychology: General, 143(2), 534-547. doi: 10.1037/a0033242 
#-------------------------------------------------------------------------


# Model of p-hacking: experimenter measures N variables but keeps only those with p < .05
#          for p-curve, if there is more than one p < .05, one is selected at random for p-curve
#          but effect size measurement is based on all retained variables 

# model as two forms: 1 p-values selected if low, regardless of direction of effect;
#                          2 low p-values disregarded unless group 1 > group 2.

# With this program can modify parameters to consider effect of:
# Correlation between variables
# Sample size (power)
# Percentage of runs where true effect is null
# Effect size on non-null runs

# Preliminaries
setwd("C:/Users/dbishop/Documents/R/DB_otherscripts/simulate p-hacking") 
#set working directory; user should modify this
library(xlsx)
options(digits=3)
options(scipen=999) #turn off scientific notation
library(foreach) #package for parallel computation
library(snow) #package for parallel computation
library(doParallel) #package for parallel computation
library(MASS) ## package for multivariate normal simulation
#-------------------------------------------------------------------------

# Initialise variables

#This sets up all fixed elelments fed into the simulation, including correlations, Number of iterations, number of variables,
#whether a real effect is present and to what extent, sample size.

#We also set up the empty output data frames for generated infomation to populate. 
#-------------------------------------------------------------------------
Modeltype=1 #1 or 2: 1-tailed, i.e. ignore low pvalues for effect if grp2>grp1; 2-tailed, accept any low p
mytails='greater'
if (Modeltype==2){mytails='two.sided'}
nrun=100000#n runs for each epoch; set to around 200 for testing program; many more for final run
nvar=1 #n variables to simulate; user can modify this
prealeff=0#.5 #proportion of runs where there is a real effect of group 
#           (currently this will apply just to the last variable)
realeffsize=.3 #effect size when there is real effect
allcorr=c(0) #different correlation values to cycle through (divide by 10 for r)
allN=c(40,400) #different sample sizes to cycle through (divide by 2 for group size)
alleffsize=0 #default effect size is null

allepochs=length(allcorr)*length(allN)*length(alleffsize) # calculates all permutations of the model.

myparams=matrix(0,nrow=3,ncol=allepochs) #specify parameters for each epoch
myparams[1,]=rep(allcorr,allepochs/length(allcorr))#myparams allows flexible variation in each epoch of correlation (row 1 x 10)
myparams[2,]=sort(rep(allN,allepochs/length(allN))) # sample size 
myparams[3,]=sort(rep(alleffsize,allepochs/length(alleffsize)))# effect size 

bindef=c(.005,.01,.015,.02,.025,.03,.035,.04,.045,.05) #Bin boundaries

#specifies plot information 
colors <- c('blue','blue') #variables for plots at end
linetype <- c(2,1) #dotted for first 4, plain for last 4


nepochs=ncol(myparams) #each col specifies correl/ndata/effectsize for one epoch
epochsummary=data.frame(matrix(0,nrow=nepochs,ncol=length(bindef)+1));#initialise epochsummary
#summarises pvalues in bins for each parameter set
colnames(epochsummary)[2:(length(bindef)+1)]=bindef
colnames(epochsummary)[1]='Corr'

#for fast processing, myreview now includes bindef
myreview=data.frame(matrix(0, nrow = nepochs, ncol = 29)) #summarises main findings of each epoch

#add corresponding column names to the output data frame
colnames(myreview)=c('Modeltail','epoch','nrun','nvar','corr','gpsize',
                     'trueff','prealeff','realeffsize','hackedeff','estrealeff','FPrate','TPrate',
                     'N<025', 'N025-05', 'N04-045', 'N045-05', 
                     'pEvid','pHack',bindef)
#mydata40<-array(NA,c(40,11,10))
#mydata200<-array(NA,c(200,11,10))

#----------------------------------------------------------------------------------------------
#Outer loop runs the simulation with given parameters 

#This is the main out loop that iterates over the differnt epochs set out earlier. We use parallelized code at this stage,
#so that different epochs run simultaneously. This is designed to speed up processing, so that each epoch is not run serially.
#----------------------------------------------------------------------------------------------

require(doParallel,foreach,car) #R packages required for parallel processing in R
cl<-makeCluster(6)# set up cluster. This is number of processors available on your pc, i.e. 6 processors = 6 epoch at once.
registerDoParallel(cl)#initiates the cluster
ptm<-proc.time()#this is to time the code, so that estimates over computational overhead are possible. (we will return to this later.)
Psaver<-foreach (thisepoch = 1:nepochs,.combine='rbind')%dopar%{ #starts parallel loop
  require(MASS)#the following are R packages required for the operations in this loop. We place them inside the loop as the worker processors require this info to be passed to them.
  require(car)
  require(xlsx)
  #---------------------------------------------------------
  # for (thisepoch in 1:nepochs){ #for testing without Parallel processor
  
  mycorr=myparams[1,thisepoch]/10 #correlation for this run
  ndata=myparams[2,thisepoch]#sample size for this run. 
  myeff=myparams[3,thisepoch] #efect size for this run.
  gpsize=ndata/2 #assume 2 equal groups
  
  myp =matrix(0, nrow = nrun, ncol = nvar+5) #for each epoch initialise myp; 
  #stores p-values, p-status, effect size (specified/max/avg),ps for selected vars
  
  epochsummary[thisepoch,1]=mycorr #column 1 records the correlation for this loop
  #----------------------------------------------------------------------------------------------
  #create covariance matrix for simulated data#
  #----------------------------------------------------------------------------------------------
  
  # We are generating the data from a multivariate normal distribution so we are required to specifiy the covariance between variables.
  
  mymean=rep(0,nvar+1) # all variables have mean equal to zero.
  mycov=diag(nvar+1) #extra variable is for group
  
  for (j in 1:(nvar-1)) {
    for (k in (j+1):nvar){
      mycov[j,k]=mycorr
      mycov[k,j]=mycorr
    }
  }
  
  #----------------------------------------------------------------------------------------------
  #middle loop does nrun runs with current parameters
  #----------------------------------------------------------------------------------------------
  startnull=1+as.integer(nrun*prealeff) #if real effect, these runs occur first; start null is first run with null effect
  for (myrun in 1:nrun) {  
    
    #generate simulated dataset with nvar variables correlated mycorr and one binary (group) variable
    mydata = mvrnorm(ndata,mymean,mycov )
    
    #specify group ID as 0 or 1
    mydata[1:gpsize,nvar+1]=0;
    mydata[(gpsize+1):ndata,nvar+1]=1;
    
    #substitutes lognormal distributed variables in one variable (quick adaptation which could be implemented much better)

          mydata[1:gpsize,1]=rlnorm(gpsize,0,1);
          mydata[(gpsize+1):ndata,1]=rlnorm(gpsize,0,1);

    
    #on prealeff proportion of runs, add real effect to last variable.
    if(myrun<startnull)
    {mydata[1:gpsize,nvar]=mydata[1:gpsize,nvar]+realeffsize
     myp[myrun,nvar+2]=realeffsize} #intended eff size for this run
    
    #----------------------------------------------------------------------------------------------
    #inner loop tests each variable for group difference
    #----------------------------------------------------------------------------------------------
    #compute p-value for t-test on each of nvar variables for group comparison and store in myp
    #note: t.test uses student's t test rather than changing automatically to correect for variance imbalance in groups. SPSS does not do this by default.
    
    thisp=rep(0,nvar) #initialise thisp
    thiseff=rep(0,nvar) #initialise thiseff
    for (j in 1:nvar){
      
      myt=t.test(mydata[,j]~mydata[,nvar+1],alternative=mytails,var.equal=T) # col nvar+1 specifies groups to compare
      thisp[j]=myt$p.value #saves p-value from test to dataframe     
      thiseff[j]=myt$estimate[1]-myt$estimate[2]# NB directional effect
      
    }#end of inner loop , i.e go to next variable
    #----------------------------------------------------------------------------------------------
    #REDUNDANT CODE
    # Testing to see if the Bonferroni correction would give rise to a change in the p-value distribution. 
    #The message is probably yes, but not in the region below p<0.05. The effect will be left skew near 1.
    
    #for(j in 1:3)
    #{
    #thisp[j]<-thisp[j]/10
    #}
    
    myp[myrun,1:nvar]=thisp #writes the vector to output data.
    ps=which(thisp<.05) #selects all p-values below 0.05
    
    if(length(ps)>0) { #check: are there any pvalues < .05
      #r=ps[1] #if so take first value (this will be random as not sorted) 
      #previously used 'sample' but this behaved oddly if only one value
      if(length(ps)==1){r=ps[1]}
      if(length(ps)>1){r=sample(ps,1)}
      
      pbin=min(which(thisp[r]<bindef)) #categorise current p value into a bin according to size
      
      
      #add to counter for this pbin, i.e. this creates the frequency of p-values in each bin by adding 1 to the appropriate bin at each iteration
      epochsummary[thisepoch,pbin+1]=epochsummary[thisepoch,pbin+1]+1       
      myp[myrun,nvar+1]=1 #col nvar+1 set to 1 if any value < .05; denotes this is a 'significant' run
      myp[myrun,nvar+5]=thisp[r] #nvar+5  hold pvalue retained for Pcurve 
      myp[myrun,nvar+3]=mean(thiseff[ps]) #mean of selected effect size on this run
    }
    myp[myrun,nvar+4]=mean(thiseff) #average effect size across all variables, included and not included
    
  } #end of middle loop
  
  
  myresults=data.frame(myp) #contains p value for each var for each of N runs for each epoch
  colnames(myresults)[(nvar+1):(nvar+5)]=c('sigresult','TrueEff','ObsEff','AvgEff','p.sel')
  #additional columns have p values for selected
  
  write.csv(myresults,"C:/Users/pthompson/Desktop/simres_lognorm_nophack100k_cor0.csv")#write results to an external csv file for later analysis.
  
  #compute percent false positives
  allnull=nrun-startnull+1
  myFP=sum(myresults[startnull:nrun,nvar+1])
  myCN=(allnull-myFP)
  FPrate=100*myFP/allnull
  
  #compute percent true positives
  TPrate=0
  allpos=startnull-1
  if (allpos>0)
  {myTP=sum(myresults[1:allpos,nvar+1])
   mymiss=allpos-myTP
   TPrate=100*myTP/allpos
  }
  
  
  Nvlow=sum(epochsummary[thisepoch,2:6]) #pvalues from 0 to .0249
  Nlesslow=sum(epochsummary[thisepoch,7:11]) #pvalues from .025 to .0499
  b=binom.test(Nvlow,Nvlow+Nlesslow,alternative="greater")#binomial test for evidential value
  pEvid=b$p.value#evidential value test p-value
  N4=epochsummary[thisepoch,10] #pvalues from .04 to .0449
  N45=epochsummary[thisepoch,11] #pvalues from .045 to .0499
  b2=binom.test(N4,N45+N4,alternative='less') #binomial test for p-hacking
  pHack=b2$p.value #p-hacking test p-value
  
  hackedeff=mean(myresults[startnull:nvar,13]) # calculate the real effect from data.
  estrealeff=0
  if (startnull>1) {
    estrealeff=mean(myresults[1:startnull-1,13])}
  
  #write all info to the output data file as a summary 
  
  myreview[thisepoch,]=c(Modeltype,thisepoch,nrun,nvar,mycorr,gpsize,myeff,
                         prealeff,realeffsize,hackedeff,estrealeff,FPrate,TPrate,
                         Nvlow,Nlesslow,N4,N45,
                         pEvid,pHack,epochsummary[thisepoch,])
  
  
} #end of outer loop; go to next epoch
---------------------------------------------------------
  proc.time()-ptm # closes the code timing loop 
stopCluster(cl) # stops the parallel processing and gathers information from processors.
myreview<-Psaver[,1:19] # saves summary info for paper.
epochsummary<-Psaver[,20:dim(Psaver)[2]]
# #---------------------------------------------------------
#formatting for the parallel processing of output.
myreview<-as.data.frame(myreview)

epochsummary<-as.data.frame(epochsummary)
colnames(myreview)=c('Modeltail','epoch','nrun','nvar','corr','gpsize',
                     'trueff','prealeff','realeffsize','hackedeff','estrealeff','FPrate','TPrate',
                     'N<025', 'N025-05', 'N04-045', 'N045-05', 
                     'pEvid','pHack')


#--------------------------------------------------------------------------
#plots: showing the p-curve for all epochs 
#--------------------------------------------------------------------------
windows() # opens a new plotting window. (not strictly necessary)
bit=epochsummary[,2:length(epochsummary)] #isolate pbin data
for(i in 1:10){bit[,i]<-as.numeric(bit[,i])}
nmax = max(bit) #find largest value for Y axis range


#calculate the percentage instead of counts (following request of reviewer - D.Lakens)
bit.tot<-apply(bit,1,sum)
bitt<-bit
for(i in 1:2)
{bitt[i,]<-(bit[i,]/bit.tot[i])*100}

# get the range for the x and y axis
xrange =c(0,.05) #xaxis will show range of pvalues
yrange =c(0,25) #xaxis will show percentage

# set up the plot
plot(xrange, yrange, type="n", xlab="p-value",
     ylab="Percentage of p-values (%)",cex=1)

# add lines
for (i in 1:nepochs) {
  bit2=rbind(bindef[1:19],bitt[i,])
  lines(t(bit2),
        col=colors[i],lty=linetype[i],lwd=2)
}

# add a title and subtitle
mytitle=paste("P-curve; 10 variables with ghost variables dropped")
title(mytitle)

text(rep(0.052,4), c(10), srt = 0, adj = 1.9,labels = c("r=0"), xpd = TRUE)

abline(h=10,col="darkgrey")
dev.off()


#save summary data to an excel file

myfilename=paste(Modeltype,'_nrun_',nrun,'_nvar_' ,nvar,'_preal_',
                 prealeff,'_effsize_',realeffsize,'_allN_',allN,
                 '.xls',sep='')
write.xlsx(myreview,myfilename)


###################################################################

