#create empty objects
#T <- H+2
tps <- array(NA,c(S,S,T))
trace <- matrix(nrow = T, ncol = S)
qtime <- rep(NA,len=T)
cost <- rep(NA,len=T)
# note the end of the first cycle of the end of T=2


model <- function(i,pRec,chemo,test,ctest=0) {
#======================= TRANSITION MATRICES ================================================================================================================
T <- H+2
# transition matrix states:  1=DF, 2=LR, 3=DFaLR, 4=DF, 5=CHF, 6=AML, 7=Dead

for (t in 1:T){

tps[1,1,t] <- 1-(1-exp(- mr[startage + t]-ifelse(chemo==1 & t == 1,log(1-mort_chemo[i]),0))) - pRec[t,i] - prob_CHF[startage + t,i] - prob_AML[i]
tps[1,2,t] <- pRec[t,i]*propLR[i]
tps[1,3,t] <- 0
tps[1,4,t] <- pRec[t,i]*(1-propLR[i])
tps[1,5,t] <- prob_CHF[startage + t,i]
tps[1,6,t] <- prob_AML[i]
tps[1,7,t] <- 1-exp(   -  mr[startage + t]-(ifelse(chemo==1 & t==1,log(1-mort_chemo[i]),0)))

tps[2,1,t] <- 0
tps[2,2,t] <- 0
tps[2,3,t] <- 1-mr[startage + t]- pRec[t,i]*(1-propLR[i])
tps[2,4,t] <- pRec[t,i]*(1-propLR[i])
tps[2,5,t] <- 0
tps[2,6,t] <- 0
tps[2,7,t] <- mr[startage + t]

tps[3,1,t] <- 0
tps[3,2,t] <- 0
tps[3,3,t] <- 1-mr[startage + t] - pRec[t,i]*(1-propLR[i]) - ifelse(chemo==1,prob_CHF[startage + t,i],0) - ifelse(chemo==1,prob_AML[i],0)
tps[3,4,t] <- pRec[t,i]*(1-propLR[i])
tps[3,5,t] <- ifelse(chemo==1,prob_CHF[startage + t,i],0)
tps[3,6,t] <- ifelse(chemo==1,prob_AML[i],0)
tps[3,7,t] <- mr[startage + t] 

tps[4,1,t] <- 0
tps[4,2,t] <- 0
tps[4,3,t] <- 0
tps[4,4,t] <- 1-ifelse(chemo==0,pDeath_DR_low[i], pDeath_DR[i])
tps[4,5,t] <- 0
tps[4,6,t] <- 0
tps[4,7,t] <- ifelse(chemo==0,pDeath_DR_low[i], pDeath_DR[i])

tps[5,1,t] <- 0
tps[5,2,t] <- 0
tps[5,3,t] <- 0
tps[5,4,t] <- 0
tps[5,5,t] <- 1-pdeath_CHF[i]
tps[5,6,t] <- 0
tps[5,7,t] <- pdeath_CHF[i]
  
tps[6,1,t] <- 0
tps[6,2,t] <- 0
tps[6,3,t] <- 0
tps[6,4,t] <- 0
tps[6,5,t] <- 0
tps[6,6,t] <- 1-(1-exp(-(mr[startage+t]+rmr_AML[i])))
tps[6,7,t] <- 1-exp(-(mr[startage+t]+rmr_AML[i]))

tps[7,1,t] <- 0
tps[7,2,t] <- 0
tps[7,3,t] <- 0
tps[7,4,t] <- 0
tps[7,5,t] <- 0
tps[7,6,t] <- 0 
tps[7,7,t] <- 1

}		

#==================== MARKOV trace ========================================================================================================================

# trace for t = 1 [all start in state 1]
trace[1,1] <- 1
trace[1,-1] <- 0

# trace for t >= 2
for (t in 2:T) {
	trace[t,] <- trace[t-1,] %*% tps[,,t]	
}

#======================= Outputs ============================================================================================================================

#### QALYs ####

for (t in 2:T) {
	qtime[t] <- ((if (t==2) trace[t,1]*(ifelse(chemo==1,U[startage+t]-uDFdec.chemo[i],U[startage+t]-uDFdec[i])) else trace[t,1]*(U[startage+t]-uDFdec[i])) + trace[t,2]*(U[startage+t]-uLRdec[i]) + trace[t,3]*(U[startage+t]-uDFdec[i]) + trace[t,4]*(U[startage+t]-uDRdec[i]) + trace[t,5]*uCHF[i] + trace[t,6]*uAML[i])/((1+disc.b)^(t-1))
}

QALYs <- qtime[2]/2+sum(qtime[3:(T-1)])+qtime[T]/2

#### COSTs #### (with half cycle correction)

for (t in 2:T) {
	cost[t] <- (  ((if (t==2) trace[t,1]*ifelse(chemo==1,cTreat[i]+cTox[i]+cDF[i]/2,cDF[i]/2) else trace[t,1]*ifelse(t<=10,cDF[i],0)) 
			+ trace[t,2]*cLR[i]
			+ trace[t,3]*cDF[i]
			+ trace[t,4]*cDR[i]
			+ trace[t,5]*cCHF[i]
	    + trace[t,6]*cAML[i]
			+ if(t==1) 0 else trace[t-1,4]*tps[4,6,t-1]*cTerm[i] )  
			/((1+disc.c)^(t-1)))
}

COSTs <- sum(cost[2:(T-1)])+cost[T]/2+ifelse(test==1,ctest,0) 

#Model return:
return(c(QALYs,COSTs))

}
