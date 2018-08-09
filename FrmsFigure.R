library(RColorBrewer)
library(sinaplot)
library(gdata)

#Import data and remove non respondents
PBfs = read.csv("/Users/paulwagner/Desktop/Learning/Revise and Resubmit 3/Data/BeliefsIre.csv", header = TRUE, stringsAsFactors=FALSE)
PBfs <- PBfs[-c(10,14,23,37,41),-c(1,2)]
PBfs [PBfs  == 97] <- 3 #Code blank answers as neutral (97 refers to blank answers)

#Calculate Distance Matrix
npol.distR <- dist(PBfs, method = "manhattan")
PrefSimMatR<- max(npol.distR) - npol.distR
PSMR<-as.matrix(PrefSimMatR)

#Calculate max diustance between any pair
MaxDif <- max(PSMR)

#create beliefs distance matrix for participants at ten most popular forums (including organiser)
IIEA_bfs <- PSMR[c(1,4,9,17,18,25,28,29,32,33,34,35,36,38,41,47,49,50,51,52),c(1,4,9,17,18,25,28,29,32,33,34,35,36,38,41,47,49,50,51,52)]
DECLG_bfs <- PSMR[c(4,9,14,15,17,20,24,27,28,29,30,31,32,40,47,49,51),c(4,9,14,15,17,20,24,27,28,29,30,31,32,40,47,49,51)] 
EPA_bfs <- PSMR[c(3,10,16,18,24,27,29,32,33,35,43,47,50,51,52),c(3,10,16,18,24,27,29,32,33,35,43,47,50,51,52)]
ESRI_bfs <- PSMR[c(1,9,17,20,22,25,28,29,32,35,41,50,51),c(1,9,17,20,22,25,28,29,32,35,41,50,51)]
NESC_bfs <- PSMR[c(4,9,14,20,25,27,28,29,41,44,50,51),c(4,9,14,20,25,27,28,29,41,44,50,51)]
SEAI_bfs <- PSMR[c(1,8,9,14,20,22,25,28,35,50),c(1,8,9,14,20,22,25,28,35,50)]
IBEC_bfs <- PSMR[c(1,4,9,17,32,35,41,42,50,51),c(1,4,9,17,32,35,41,42,50,51)]
DCENR_bfs <- PSMR[c(4,9,14,15,16,20,24,28,30,40),c(4,9,14,15,16,20,24,28,30,40)] 
EI_bfs <- PSMR[c(17,22,24,29,35,43,50,51),c(17,22,24,29,35,43,50,51)]  
DAFM_bfs <- PSMR[c(12,15,20,24,28,30,51),c(12,15,20,24,28,30,51)]

#Extract lower triangle of matrices
IIEA <- lowerTriangle(IIEA_bfs, diag=FALSE)
DECLG  <- lowerTriangle(DECLG_bfs, diag=FALSE)
EPA <- lowerTriangle(EPA_bfs, diag=FALSE)
ESRI  <- lowerTriangle(ESRI_bfs, diag=FALSE)
NESC  <- lowerTriangle(NESC_bfs, diag=FALSE)
SEAI  <- lowerTriangle(SEAI_bfs, diag=FALSE)
IBEC <- lowerTriangle(IBEC_bfs, diag=FALSE)
DCENR  <- lowerTriangle(DCENR_bfs, diag=FALSE)
Earth_Inst <- lowerTriangle(EI_bfs, diag=FALSE)
DAFM  <- lowerTriangle(DAFM_bfs, diag=FALSE)

#Normaling function 
normalize <- function(x) {
  return ((x - 0) / (MaxDif - 0))
}

#Normalize the data
IIEA <- lapply(IIEA, normalize)
DECLG <- lapply(DECLG, normalize)
EPA <- lapply(EPA, normalize)
ESRI <- lapply(ESRI, normalize)
NESC <- lapply(NESC, normalize)
SEAI <- lapply(SEAI, normalize)
IBEC <- lapply(IBEC, normalize)
DCENR <- lapply(DCENR, normalize)
Earth_Inst <- lapply(Earth_Inst, normalize)
DAFM <- lapply(DAFM, normalize)

#unlist
IIEA <- unlist(IIEA)
DECLG <- unlist(DECLG)
EPA <- unlist(EPA)
ESRI <- unlist(ESRI)
NESC <- unlist(NESC)
SEAI <- unlist(SEAI)
IBEC <- unlist(IBEC)
DCENR <- unlist(DCENR)
Earth_Inst <- unlist(Earth_Inst)
DAFM <- unlist(DAFM)

#Plot
x <- c(IIEA,DECLG,EPA,ESRI,NESC,SEAI,IBEC,DCENR,Earth_Inst,DAFM)
groups <- c(rep("IIEA", 190), rep("DECLG", 136), rep("EPA", 105),rep("ESRI", 78), rep("NESC", 66), rep("SEAI", 45),rep("IBEC", 45), rep("DCENR", 45), rep("Earth Inst.", 28), rep("DAFM", 21))
sinaplot(x, groups , scale = TRUE,method = "counts", pch = 20,
         ylab="Normalized beliefs distance between pairs of actors", xlab="Forum Organisers")

#Descriptives in Table 1
min(IIEA)
min(DECLG)
min(EPA)
min(ESRI)
min(NESC)
min(SEAI)
min(IBEC)
min(DCENR)
min(Earth_Inst)
min(DAFM)

max(IIEA)
max(DECLG)
max(EPA)
max(ESRI)
max(NESC)
max(SEAI)
max(IBEC)
max(DCENR)
max(Earth_Inst)
max(DAFM)

mean(IIEA)
mean(DECLG)
mean(EPA)
mean(ESRI)
mean(NESC)
mean(SEAI)
mean(IBEC)
mean(DCENR)
mean(Earth_Inst)
mean(DAFM)

sd(IIEA)
sd(DECLG)
sd(EPA)
sd(ESRI)
sd(NESC)
sd(SEAI)
sd(IBEC)
sd(DCENR)
sd(Earth_Inst)
sd(DAFM)

IQR(IIEA)
IQR(DECLG)
IQR(EPA)
IQR(ESRI)
IQR(NESC)
IQR(SEAI)
IQR(IBEC)
IQR(DCENR)
IQR(Earth_Inst)
IQR(DAFM)
