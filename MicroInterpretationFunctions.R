require(ergm)
require(combinat)

allBin <- function(n){
	vecs <- rbind(rep(0,n),rep(1,n))
	base <- rep(0,n)
	for(j in 1:(n-1)){
		places <- t(combn(1:n,j))
		for(k in 1:nrow(places)){
			veci <- base
			veci[places[k,]] <- 1
			vecs <- rbind(vecs,veci)
			}
		
		}
		vecs
	}
	

pEdge <- function(ergm_formula,theta,i,j,net,...){
	# ergm_formula is the formula for the model
	# Formula should be given as a character element, e.g., "net ~ edges + mutual"
	# theta is the vector of coefficients 
	# i is the sender node
	# j is the receiver node
	# net is the network 
	# note, net in the ergm formula should be called "net"
	# ... is for additional data called by ergm_formula
	# ... could be, e.g., the argument to dyadcov()
	ergm_formula <- as.formula(ergm_formula)
	net[i,j] <- 0
	mod <- ergm.getmodel(ergm_formula, ergm.getnetwork(ergm_formula),drop=F)
	stat0 <- ergm.getglobalstats(ergm.getnetwork(ergm_formula), mod)
	net[i,j] <- 1
	mod <- ergm.getmodel(ergm_formula, ergm.getnetwork(ergm_formula),drop=F)
	stat1 <- ergm.getglobalstats(ergm.getnetwork(ergm_formula), mod)
	chgstat <- stat1-stat0
	lp <- t(chgstat)%*%cbind(theta)
	1/(1+exp(-lp))
	}

pDyad <- function(ergm_formula,theta,i,j,net,...){
	# ergm_formula is the formula for the model
	# Formula should be given as a character element, e.g., "net ~ edges + mutual"
	# theta is the vector of coefficients 
	# i is node 1
	# j is node 2
	# net is the network 
	# note, net in the ergm formula should be called "net"
	# ... is for additional data called by ergm_formula
	# ... could be, e.g., the argument to dyadcov()
	ergm_formula <- as.formula(ergm_formula)
	eta_mat <- matrix(NA,2,2)
	for(xi in c(0,1)){
		for(xj in c(0,1)){
			net[i,j] <- xi
			net[j,i] <- xj
			mod <- ergm.getmodel(ergm_formula, ergm.getnetwork(ergm_formula),drop=F)
			eta_mat[xi+1,xj+1] <- t(theta)%*%cbind(ergm.getglobalstats(ergm.getnetwork(ergm_formula), mod))
			}
		}
	prob_mat <- matrix(NA,2,2)
	for(xi in c(0,1)){
		for(xj in c(0,1)){
			etas <- c(eta_mat)-eta_mat[xi+1,xj+1]
			prob_mat[xi+1,xj+1] <- 1/(sum(exp(etas)))
			}
		}
	rownames(prob_mat) <- c("i->j = 0", "i->j = 1")
	colnames(prob_mat) <- c("j->i = 0", "j->i = 1")
	prob_mat
	}
	

pNode <- function(ergm_formula,theta,node,others,nodeSend = T,net,...){
	# ergm_formula is the formula for the model
	# Formula should be given as a character element, e.g., "net ~ edges + mutual"
	# theta is the vector of coefficients 
	# node is either the "sender" or the "receiver"
	# others is a vector of indices of the receiving or sending nodes
	# nodeSend is a logical indicating whether "node" should be a sender (T)
	# net is the network 
	# note, net in the ergm formula should be called "net"
	# ... is for additional data called by ergm_formula
	# ... could be, e.g., the argument to dyadcov()
	ergm_formula <- as.formula(ergm_formula)
	allvecs <- allBin(length(others))
	eta <- numeric(nrow(allvecs))
	for(i in 1:nrow(allvecs)){
			if(nodeSend) net[node,others] <- allvecs[i,] else net[others,node] <- allvecs[i,]
			mod <- ergm.getmodel(ergm_formula, ergm.getnetwork(ergm_formula),drop=F)
			eta[i] <- t(theta)%*%cbind(ergm.getglobalstats(ergm.getnetwork(ergm_formula), mod))
		}
	prob <- numeric(nrow(allvecs))
	for(i in 1:nrow(allvecs)){
			prob[i] <- 1/sum(exp(eta-eta[i]))
		}
	if(nodeSend) colnames(allvecs) <- paste("Receiver",others,sep="") else colnames(allvecs) <- paste("Sender",others,sep="")
	cbind(prob,allvecs)

	}
	
	

	
	
	
	
	
