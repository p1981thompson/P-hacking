######################################################################
# P-hacking plot grids 

# Dorothy Bishop idea: 26/11/2015
# Written by Paul Thompson 01/12/2015
######################################################################

#The program takes the output from Luke Holman's ghost variable simulation - in response to Bishop & Thompson(2015) preprint. 

#A grid of four line plots is generated each showing different simulated combinations of Number of variables, sample size, correlation, effect size, and whether P-hacked or not.

#####################################################################
#####################################################################

# Not P-hacked plots

#####################################################################
#####################################################################
#Data read in following script by L.Holman's 'ghosthack.plot',(https://sites.google.com/site/lukeholman/home)

dd <- read.csv("Ghosthack summary 100K nophack.csv") # reads in data into R.


dd <- dd[dd$bin %in% levels(dd$bin)[1:10], ] # Trim off all p-values >= 0.05
dd$vars <- factor(paste(dd$vars, "variables"), levels = c("3 variables", "8 variables")) #rename discrete labels for Number of variables.
dd$Correlation<-factor(dd$co) # change variable type to factor so that plotting is correct.
dd$N<-factor(dd$n) # change variable type to factor so that plotting is correct.


dd1 <- dd[dd$vars %in% levels(dd$vars)[1], ] #subset data for plots A and B in grid
dd1.1 <- dd1[dd1$meanA==0.0, ] # subset data for plot A in grid
dd1.2 <- dd1[dd1$meanA==0.3, ] # subset data for plot B in grid

dd2 <- dd[dd$vars %in% levels(dd$vars)[2], ] #subset data for plots C and D in grid
dd2.1 <- dd2[dd2$meanA==0.0, ] # subset data for plot C in grid
dd2.2 <- dd2[dd2$meanA==0.3, ] # subset data for plot D in grid

#This plot is generated as a dummy, so that the legend info can be stripped from it and repositioned in the main grid of plots.
plot11 <- ggplot(dd1.1, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1)  +theme_bw() + theme(legend.position="top") +
  theme(text = element_text(size=14),legend.key = element_rect(colour = NA),legend.text = element_text(size = 14, face = "bold"),legend.title = element_text(size=14, face="bold"))

#This is the plot for the top left panel of the grid (A). We remove the X axis labels here to conserve space and unclutter the plot. limits are manually set so that the plots are on the same scales as far as possible for comparison.
plot1 <- ggplot(dd1.1, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none", axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks=element_blank(),text = element_text(size=14, face="bold")) + scale_y_continuous(limits = c(0, 1000)) + ggtitle("(A)     3 variables     ") 

#This is the plot for the top left panel of the grid (B). Again, We remove the X axis labels here to conserve space and unclutter the plot. limits are manually set so that the plots are on the same scales as far as possible for comparison.
plot2 <- ggplot(dd1.2, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) + scale_y_continuous(limits = c(0, 75000)) + 
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none", axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks=element_blank(),text = element_text(size=14, face="bold")) + ggtitle("(B)     3 variables     ")

#This is the plot for the top left panel of the grid (C).
plot3 <- ggplot(dd2.1, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none",axis.ticks=element_blank()) + 
  scale_y_continuous(limits = c(0, 1000)) + ggtitle("(C)     8 variables     ") + scale_x_continuous(breaks=1:10, labels=levels(dp2.2$bin)[1:10])+ theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=14,face="bold"))+xlab("Bin")

#This is the plot for the top left panel of the grid (D)
plot4 <- ggplot(dd2.2, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none",axis.ticks=element_blank()) + scale_y_continuous(limits = c(0, 75000)) +
  ggtitle("(D)     8 variables     ") + scale_x_continuous(breaks=1:10, labels=levels(dp2.2$bin)[1:10])+ theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=14, face="bold"))+xlab("Bin")





######################################################################
#Chops Legend from plots and pastes as an overarching legend at top
######################################################################
#NOTE: This function is from: http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(plot11)


#####################################################################
#Function to arrange plots in correct layout
#####################################################################
windows(width=10, height=10)

main.g <- textGrob("No p-hacking", gp=gpar(fontsize=30))
sub.g <- textGrob("True effect size = 0                                               True effect size = .3", gp=gpar(fontsize=15, fontface=3L))
grid.newpage()

#this function places the four plots,titles, and legend in the correct place and resizes everthing accordingly.
grid.arrange(main.g, sub.g,legend,plot1,plot2,plot3,plot4, ncol=2,nrow=5,layout_matrix = rbind(c(1,1),c(2,2),c(3,3),c(4,5),c(6,7)),widths = c(2.7,2.7), heights = c(0.5,0.5,0.75,2.6,2.8))



#####################################################################
#####################################################################

# P hacked plots

#####################################################################
#####################################################################

#Data read in following script by L.Holman's 'ghosthack.plot',(https://sites.google.com/site/lukeholman/home)

dp <- read.csv("Ghosthack summary_100K_phacked.csv")# reads in data into R.


dp <- dp[dp$bin %in% levels(dp$bin)[1:10], ] # Trim off all p-values >= 0.05
dp$vars <- factor(paste(dp$vars, "variables"), levels = c("3 variables", "8 variables"))#rename discrete labels for Number of variables.
dp$Correlation<-factor(dp$co) # change variable type to factor so that plotting is correct.
dp$N<-factor(dp$n) # change variable type to factor so that plotting is correct.


dp1 <- dp[dp$vars %in% levels(dp$vars)[1], ]#subset data for plots A and B in grid
dp1.1 <- dp1[dp1$meanA==0.0, ]#subset data for plots A in grid
dp1.2 <- dp1[dp1$meanA==0.3, ]#subset data for plots B in grid

dp2 <- dp[dp$vars %in% levels(dp$vars)[2], ]#subset data for plots C and D in grid
dp2.1 <- dp2[dp2$meanA==0.0, ]#subset data for plots C in grid
dp2.2 <- dp2[dp2$meanA==0.3, ]#subset data for plots D in grid


#This plot is generated as a dummy, so that the legend info can be stripped from it and repositioned in the main grid of plots.
plot21 <- ggplot(dp1.1, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1)  +theme_bw() + theme(legend.position="top") +
  theme(text = element_text(size=14),legend.key = element_rect(colour = NA),legend.text = element_text(size = 14, face = "bold"),legend.title = element_text(size=14, face="bold"))


#This is the plot for the top left panel of the grid (A). We remove the X axis labels here to conserve space and unclutter the plot. limits are manually set so that the plots are on the same scales as far as possible for comparison.
plot1A <- ggplot(dp1.1, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none", axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks=element_blank(),text = element_text(size=14, face="bold")) + scale_y_continuous(limits = c(0, 5000)) + ggtitle("(A)     3 variables     ") 

#This is the plot for the top left panel of the grid (B). Again, We remove the X axis labels here to conserve space and unclutter the plot. limits are manually set so that the plots are on the same scales as far as possible for comparison.
plot2A <- ggplot(dp1.2, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) + scale_y_continuous(limits = c(0, 75000)) + 
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none", axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks=element_blank(),text = element_text(size=14, face="bold")) + ggtitle("(B)     3 variables     ")

#This is the plot for the top left panel of the grid (C).
plot3A <- ggplot(dp2.1, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none",axis.ticks=element_blank()) + 
  scale_y_continuous(limits = c(0, 5000)) + ggtitle("(C)     8 variables     ") + scale_x_continuous(breaks=1:10, labels=levels(dp2.2$bin)[1:10])+ theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=14,face="bold"))+xlab("Bin")

#This is the plot for the top left panel of the grid (D).
plot4A <- ggplot(dp2.2, aes(x = as.numeric(bin), y = count,col=Correlation,lty=N)) +
  geom_line(lwd = 1) +theme_bw()+ theme(legend.position="none",axis.ticks=element_blank()) + scale_y_continuous(limits = c(0, 75000)) +
  ggtitle("(D)     8 variables     ") + scale_x_continuous(breaks=1:10, labels=levels(dp2.2$bin)[1:10])+ theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=14, face="bold"))+xlab("Bin")


######################################################################
#Chops Legend from plots and pastes as an overarching legend at top
######################################################################
#NOTE: This function is from: http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(plot21)


#####################################################################
#Function to arrange plots in correct layout
#####################################################################
windows(width=10, height=10)

main.g2 <- textGrob("Ghost p-hacked", gp=gpar(fontsize=30))
sub.g <- textGrob("True effect size = 0                                               True effect size = .3", gp=gpar(fontsize=15, fontface=3L))
grid.newpage()

#this function places the four plots,titles, and legend in the correct place and resizes everthing accordingly.
grid.arrange(main.g2, sub.g,legend,plot1A,plot2A,plot3A,plot4A, ncol=2,nrow=5,layout_matrix = rbind(c(1,1),c(2,2),c(3,3),c(4,5),c(6,7)),widths = c(2.7,2.7), heights = c(0.5,0.5,0.75,2.6,2.8))



