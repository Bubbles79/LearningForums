library(ergm)
library(network)
library(statnet)
seed <- 12345
set.seed (seed)

#Import Data (need to define location)
PAIre <- read.csv(file="/Users/paulwagner/Desktop/Learning/Revise and Resubmit 3/Data/PAFull.csv",header=TRUE)
coopIre <- read.csv(file="/Users/paulwagner/Desktop/Learning/Revise and Resubmit 3/Data/coopFull.csv",header=TRUE)
ForumIre <- read.csv(file="/Users/paulwagner/Desktop/Learning/Revise and Resubmit 3/Data/ForFull.csv",header=TRUE)
OrgTypeIre <- read.csv(file="/Users/paulwagner/Desktop/Learning/Revise and Resubmit 3/Data/OrgType.csv", header = FALSE, stringsAsFactors=FALSE)

#Remove Non-Respondents
PAIre <- PAIre[-c(10,14,23,37,41),-c(10,14,23,37,41)]
coopIre <- coopIre[-c(10,14,23,37,41),-c(10,14,23,37,41)]
ForumIre <- ForumIre[-c(10,14,23,37,41),-c(10,14,23,37,41)]

#Extract actor names from columns
OrgNames <- as.vector(colnames(PAIre))

#Remove self ties
diag (PAIre) <- 0 
diag (coopIre) <- 0 
diag (ForumIre) <- 0 

#Convert to Matrices
PAIre <- as.matrix(PAIre)
coopIre <- as.matrix(coopIre)
forumIre <- as.matrix(ForumIre)
OrgTypeIre  <- as.matrix(OrgTypeIre)

#Add Actor names to rows and columns
rownames(coopIre) <- OrgNames
colnames(coopIre) <- OrgNames

#Import Policy beliefs data
PBfs = read.csv("/Users/paulwagner/Desktop/Learning/Revise and Resubmit 3/Data/BeliefsIre.csv", header = TRUE, stringsAsFactors=FALSE)
PBfs <- PBfs[-c(10,14,23,37,41),-c(1,2)]
PBfs [PBfs  == 97] <- 3 #Code blank answers as neutral
npol.distR <- dist(PBfs, method = "manhattan")
PrefSimMatR<- max(npol.distR) - npol.distR
PSMR <-as.matrix(PrefSimMatR)

#Add Actor names to rows and columns
rownames(PSMR) <- OrgNames
colnames(PSMR) <- OrgNames

#Remove non-respondents and 
OrgTypeIre <- OrgTypeIre[-c(10,14,23,37,41),2]
OrgTypeIre <- as.character(OrgTypeIre)
OrgTypeIre <- as.vector(OrgTypeIre)

# create network object
nw.PAIre <- network (PAIre) 

#Set Attributes
set.vertex.attribute (nw.PAIre, "OrgTypeIre", OrgTypeIre)
set.vertex.attribute (nw.PAIre, "ForumCount", degree (ForumIre, cmode = "outdegree"))

#Create co-attendence matrix
forumIre <- forumIre %*% t(forumIre) # compute one - mode projection over forums
diag ( forumIre ) <- 0

#Add Actor names to rows and columns
rownames(forumIre) <- OrgNames
colnames(forumIre) <- OrgNames

For.m1 <- ergm(nw.PAIre ~ edges + edgecov(PSMR), 
                eval.loglik = TRUE , check.degeneracy = TRUE , 
                control = control.ergm ( seed = seed , MCMC.samplesize = 5000 , MCMC.interval = 5000))
summary(For.m1)
plot(mcmc.diagnostics(For.m1))
plot(gof(For.m1))

For.m2 <- ergm(nw.PAIre ~ edges + edgecov(PSMR) + 
                  edgecov(forumIre) + nodecov("ForumCount"), 
                eval.loglik = TRUE , check.degeneracy = TRUE , 
                control = control.ergm ( seed = seed , MCMC.samplesize = 5000 , MCMC.interval = 5000))
summary(For.m2)
plot(mcmc.diagnostics(For.m2))
plot(gof(For.m2))

For.m3<- ergm(nw.PAIre ~ edges + mutual + edgecov(PSMR) + 
                  edgecov(forumIre) + nodecov("ForumCount") + 
                  gwdsp(1, fixed = TRUE ) + gwesp(1, fixed = TRUE ), 
                eval.loglik = TRUE , check.degeneracy = TRUE , 
                control = control.ergm ( seed = seed , MCMC.samplesize = 5000 , MCMC.interval = 5000))
summary(For.m3)
plot(mcmc.diagnostics(For.m3))
plot(gof(For.m3))

For.m4 <- ergm(nw.PAIre ~ edges + mutual + edgecov(PSMR) + 
                  edgecov(forumIre) + nodecov("ForumCount") + 
                  edgecov(coopIre) +  
                  gwdsp(1, fixed = TRUE ) + gwesp(1, fixed = TRUE ), 
                eval.loglik = TRUE , check.degeneracy = TRUE , 
                control = control.ergm ( seed = seed , MCMC.samplesize = 5000 , MCMC.interval = 5000))
summary(For.m4)
plot(mcmc.diagnostics(For.m4))
plot(gof(For.m4))

For.m5 <- ergm(nw.PAIre ~ edges + mutual + edgecov(PSMR) + 
                  edgecov(forumIre) + nodecov("ForumCount") + 
                  edgecov(coopIre) + 
                  nodematch ("OrgTypeIre", diff = T) + nodefactor ("OrgTypeIre", base=-c(1,2,4,5)) + 
                  gwdsp(1, fixed = TRUE ) + gwesp(1, fixed = TRUE ), 
                eval.loglik = TRUE , check.degeneracy = TRUE , 
                control = control.ergm ( seed = seed , MCMC.samplesize = 5000 , MCMC.interval = 5000))
summary(For.m5)
plot(mcmc.diagnostics(For.m5))
plot(gof(For.m5))
