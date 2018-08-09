#url <- "https://cran.r-project.org/src/contrib/Archive/gregmisc/gregmisc_2.1.5.tar.gz"
#pkgFile <- "gregmisc_2.1.5.tar.gz"
#download.file(url = url, destfile = pkgFile)

#This code is from: from Desmarais, B.A. and Cranmer, S.J., 2012. Micro‐level interpretation of exponential random graph models with application to estuary networks. Policy Studies Journal, 40(3), pp.402-434.
#Before running this code, it is necessary to run the function in the file: MicroInterpretationFunctions.R

#Read in necessary library
library(gregmisc)
library(gplots)

# Create Matrices to store the dyad probabilities
dyadPrEst <- matrix(0,500,3)
dyadPrNull <- matrix(0,500,3)

# define parameters to use in the estimated probabilities
thetaEst <- coef(For.m5)

# define parameters to use in the null probabilities
#change thetaNull[3] <- 0 for beliefs
#change thetaNull[4] <- 0 for Forums
thetaNull <- coef(For.m5)
thetaNull[4] <- 0

# Iterate through 100 randomly selected dyads

set.seed(5)

for(i in 1:500){
  
  # select dyads for the ith iteration
  dyadi <- sample(1:20,2,rep=F)
  
  # estimate dyad probabilities according to the estimated parameters
  prEst <- pDyad(ergm_formula="net ~ edges + mutual + edgecov(PSMR) + edgecov(forumIre) + nodecov('ForumCount') + edgecov(coopIre) + nodematch ('OrgTypeIre', diff = T) + nodefactor ('OrgTypeIre', base=-c(1,2,4,5)) + gwdsp(1, fixed = TRUE ) + gwesp(1, fixed = TRUE )",
                 
                 theta=thetaEst,i=dyadi[1],j=dyadi[2],net=nw.PAIre)
  
#estimate dyad probabilities setting the mutuality parameter to zero
  prNull <- pDyad(ergm_formula="net ~ edges + mutual + edgecov(PSMR) + edgecov(forumIre) + nodecov('ForumCount') + edgecov(coopIre) + nodematch ('OrgTypeIre', diff = T) + nodefactor ('OrgTypeIre', base=-c(1,2,4,5)) + gwdsp(1, fixed = TRUE ) + gwesp(1, fixed = TRUE )",
                  
                  theta=thetaNull,i=dyadi[1],j=dyadi[2],net=nw.PAIre)
  
# store the respective probabilities in the storage matrices
  dyadPrEst[i,1] <- prEst[1,1]
  dyadPrEst[i,2] <- (prEst[1,2]+prEst[2,1])/2
  dyadPrEst[i,3] <- prEst[2,2]
  dyadPrNull[i,1] <- prNull[1,1]
  dyadPrNull[i,2] <- (prNull[1,2]+prNull[2,1])/2
  dyadPrNull[i,3] <- prNull[2,2]
}

#Function to easily compute 95% confidence intervals organized for barplots

mu_ci <- function(x){
  cix <- t.test(x)$conf.int
  c(cix[1],mean(x),cix[2])
}

#Compute the ratios
dyadSumr <- apply(dyadPrEst/dyadPrNull,2,mu_ci)

# Identify file, will write to working directory
filei <- "/Users/paulwagner/Desktop/Beliefs.pdf"

#Initialize pdf file
pdf(filei,height=3.5,width=5.25,pointsize=13,family="Times")

#Set aesthetic graphic parameters
par(las=1,mar=c(4,5,1,1),cex.lab=1.25,cex.axis=1.25)

#Labels for x‐axis
colnames(dyadSumr) <- c("(0,0)","(0,1)","(1,1)")

#Draw the bars and confidence intervals
barplot2(dyadSumr[2,],col="tan",plot.ci=T,ci.l=dyadSumr[1,],ci.u=dyadSumr[3,],ci.lwd=2,ci.col="grey40", ylab="")

#Title the axes
title(ylab="Estimated Prob./Null Prob.",line=3.5)
title(xlab="Dyadic Edge Values", line=2.7)

# Stop writing to filei
dev.off()



