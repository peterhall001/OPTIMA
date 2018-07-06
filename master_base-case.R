# OPTIMAprelim model, master file  Model version 2-0, including  
# Copyright Peter Hall, Leeds, 2014
#Model 3 = analysis based on SWOG 8814 treatment effect and Adjuvant! online/OPTIMAprelim outcomes
#Tests compared: Onxotype DX (25); MammaPrint; Prosigna Subtype; Prosigna ROR_PT

rm(list=ls(all=TRUE))
library(MASS)
library(gtools)

#set Global variables
seed <- 10         # set seed for random number generator
S <- 7             # number of states 
disc.b <- 0.035    # discount rate for benefits
disc.c <- 0.035    # discount rate for costs
Nsim <- 10000    # Number of simulations   
lambda <- 20000

#load functions
source("draw.R")
source("model.R")


# draw parameters Nsim times
draw()

##-------- MAIN ANALYSIS -----------------

## MCsim for model with chemo for all
# sample from model "Nsim" times
# replace pRec with recurrence probability vector
# model() third operator chemo = 1; no chemo = 0
# model() fourth operator = 1 = include ctest
# model() fifth parameter  = test cost (default = 0)
sim.all <- array(NA,c(Nsim,2))
for (i in 1:Nsim) {
    sim.all[i,]     <- model(i,pRec.all,1,0)
}
costs.all <- sim.all[,2]
QALYs.all <- sim.all[,1]

## MCsim for model with chemo guided by Oncotype DX
source("draw_testDX.R")
draw.testDX()   # define test-specific parameters
#ctest <- rep(0,Nsim)
sim.DX.high <- array(c(NA,NA),c(Nsim,2))
sim.DX.low  <- array(c(NA,NA),c(Nsim,2))
for (i in 1:Nsim) {
    sim.DX.high[i,] <- model(i,pRec.DXhigh.chemo,1,1,ctestDX)
    sim.DX.low[i,]  <- model(i,pRec.DXlow,0,1, ctestDX)
}
costs.DX <- sim.DX.high[,2]*propHigh + sim.DX.low[,2]*propLow
QALYs.DX <- sim.DX.high[,1]*propHigh + sim.DX.low[,1]*propLow

## MCsim for model with chemo guided by MammaPrint
source("draw_testMP.R")
draw.testMP()   # define test-specific parameters

sim.MP.high <- array(c(NA,NA),c(Nsim,2))
sim.MP.low  <- array(c(NA,NA),c(Nsim,2))
for (i in 1:Nsim) {
  sim.MP.high[i,] <- model(i,pRec.MPhigh.chemo,1,1,ctestMP)
  sim.MP.low[i,]  <- model(i,pRec.MPlow,0,1,ctestMP)
}
costs.MP <- sim.MP.high[,2]*propHighMP + sim.MP.low[,2]*propLowMP
QALYs.MP <- sim.MP.high[,1]*propHighMP + sim.MP.low[,1]*propLowMP


## MCsim for model with chemo guided by Prosignia Subtype
source("draw_testPS.R")
draw.testPS()   # define test-specific parameters

sim.PS.high <- array(c(NA,NA),c(Nsim,2))
sim.PS.low  <- array(c(NA,NA),c(Nsim,2))
for (i in 1:Nsim) {
  sim.PS.high[i,] <- model(i,pRec.PShigh.chemo,1,1,ctestPS)
  sim.PS.low[i,]  <- model(i,pRec.PSlow,0,1,ctestPS)
}
costs.PS <- sim.PS.high[,2]*propHighPS + sim.PS.low[,2]*propLowPS
QALYs.PS <- sim.PS.high[,1]*propHighPS + sim.PS.low[,1]*propLowPS

#Rprof(tmp <- tempfile(),interval = 0.001)
##code here
#Rprof()
#MyTimer1=summaryRprof(tmp)#$sampling.time
#unlink(tmp)
#MyTimer1 

## MCsim for model with chemo guided by ROR_PT60       #Added
source("draw_testROR_P60.R")
draw.testROR_P60()   # define test-specific parameters

sim.ROR_P60.high <- array(c(NA,NA),c(Nsim,2))
sim.ROR_P60.low  <- array(c(NA,NA),c(Nsim,2))
for (i in 1:Nsim) {
 sim.ROR_P60.high[i,] <- model(i,pRec.ROR_P60high.chemo,1,1,ctestROR_P60)
 sim.ROR_P60.low[i,]  <- model(i,pRec.ROR_P60low,0,1,ctestROR_P60)
}
costs.ROR_P60 <- sim.ROR_P60.high[,2]*propHighROR_P60 + sim.ROR_P60.low[,2]*propLowROR_P60
QALYs.ROR_P60 <- sim.ROR_P60.high[,1]*propHighROR_P60 + sim.ROR_P60.low[,1]*propLowROR_P60
mean(propHighROR_P60)
mean(propHighROR_P60)


mean(costs.all)
mean(QALYs.all)

mean(costs.DX)
mean(QALYs.DX)

mean(costs.MP)
mean(QALYs.MP)

mean(costs.PS)
mean(QALYs.PS)

mean(costs.ROR_P60)   
mean(QALYs.ROR_P60)   

mean(propHighROR_P60)
mean(propHighPS)

# ICER1
ICER <- (mean(costs.DX)-mean(costs.all))/(mean(QALYs.DX)-mean(QALYs.all))
mean(costs.DX)-mean(costs.all)
mean(QALYs.DX)-mean(QALYs.all)
ICER

#plot(costs.all,QALYs.all)
#points(costs.DX,QALYs.DX, col = "red")
#points(costs.MP,QALYs.MP, col = "blue")
#points(costs.PS,QALYs.PS, col = "black")
#points(costs.ROR_P,QALYs.ROR_P, col = "yellow")

#plot(mean(costs.all),mean(QALYs.all), ylim = c(0,15), xlim = c(0,50000))
#points(mean(costs.DX),mean(QALYs.DX), col = "red")
#points(mean(costs.MP),mean(QALYs.MP), col = "blue")

#plot(costs.DX-costs.all, QALYs.DX - QALYs.all, col = "red")
#points(costs.MP-costs.all, QALYs.MP - QALYs.all, col = "blue")

#plot(mean(costs.DX)-mean(costs.all), mean(QALYs.DX) - mean(QALYs.all), col = "red")
#points(mean(costs.MP)-mean(costs.all), mean(QALYs.MP) - mean(QALYs.all), col = "blue")

#Write output to csv. files to be copied to excel results sheet 
#write.csv(costs.all,"costsall.csv",row.names=FALSE)
#write.csv(QALYs.all,"QALYsall.csv",row.names=FALSE)
#write.csv(costs.DX,"costsDX.csv",row.names=FALSE)
#write.csv(QALYs.DX,"QALYsDX.csv",row.names=FALSE)
#write.csv(costs.MP,"costsMP.csv",row.names=FALSE)
#write.csv(QALYs.MP,"QALYsMP.csv",row.names=FALSE)
#write.csv(costs.PS,"costsPS.csv",row.names=FALSE)
#write.csv(QALYs.PS,"QALYsPS.csv",row.names=FALSE)
#write.csv(costs.ROR_P60,"costsROR_P60.csv",row.names=FALSE)
#write.csv(QALYs.ROR_P60,"QALYsROR_P60.csv",row.names=FALSE)


#### Net Benefit analysis  ###
lambda <- 20000

NB.all      = QALYs.all*lambda - costs.all
NB.DX       = QALYs.DX*lambda - costs.DX
NB.MP       = QALYs.MP*lambda - costs.MP
NB.PS       = QALYs.PS*lambda - costs.PS  
NB.ROR_P60  = QALYs.ROR_P60*lambda - costs.ROR_P60   

mean(NB.all)/lambda
mean(NB.DX)/lambda
mean(NB.MP)/lambda
mean(NB.PS)/lambda   
mean(NB.ROR_P60)/lambda 

maxNB <- ifelse(NB.all >= NB.DX, NB.all, NB.DX)
maxNB <- ifelse(maxNB >= NB.MP, maxNB, NB.MP)
maxNB <- ifelse(maxNB >= NB.PS, maxNB, NB.PS)            
maxNB <- ifelse(maxNB >= NB.ROR_P60, maxNB, NB.ROR_P60)  

CE.all    <- ifelse(NB.all == maxNB, 1, 0)
CE.DX     <- ifelse(NB.DX == maxNB, 1, 0)
CE.MP     <- ifelse(NB.MP == maxNB, 1, 0)
CE.PS     <- ifelse(NB.PS == maxNB, 1, 0)    
CE.ROR_P60<- ifelse(NB.ROR_P60 == maxNB, 1, 0) 

prob.CE.all     <- mean(CE.all) 
prob.CE.DX      <- mean(CE.DX) 
prob.CE.MP      <- mean(CE.MP)
prob.CE.PS      <- mean(CE.PS)       
prob.CE.ROR_P60 <- mean(CE.ROR_P60) 

EVPI <- mean(maxNB) - max(mean(NB.all), mean(NB.DX),mean(NB.MP), mean(NB.PS), mean(NB.ROR_P60))       #Altered
prob.CE.all
prob.CE.DX
prob.CE.MP
prob.CE.PS           
prob.CE.ROR_P60       

EVPI   # per patient EVPI

I <- NA # incident population over 10 years
for (t in 1:10) {
    I[t] <- 4376/1.035^t
}
pop.EVPI = EVPI*sum(I)
pop.EVPI

# incremental net benefit density plots
plot(density(NB.DX-NB.all), col = "red", main = "Incremental Net Benefit (vs. Chemo for all)", 
     xlab = "Net Health Benefit (QALYs)",axes = FALSE)
lines(density(NB.MP-NB.all), col = "orange")
lines(density(NB.PS-NB.all), col = "green")
lines(density(NB.ROR_P60-NB.all), col = "blue")
labels <- c("Oncotype DX (25)","MammaPrint","Prosigna Subtype","Prosigna ROR (60)")
cols <- c("red","orange","green","blue")
legend(200000,6e-06,labels, col = cols, lty = 1, cex = 1)
#axis(2)
pts <- c(-500000,0,5)   #pretty(density(NB.DX-NB.all)[[1]] / 100000)
axis(1, at = c(-5e05,0,5e05), labels = paste(pts, "K", sep = ""))
box()


##   CEAC - vs. CHemo for all    ######################################################################

lamb <- seq(1000,100000,200)
OUT <- array(NA,c(length(lamb),4))

for (i in 1:length(lamb)) {

      NB.all      <- QALYs.all*lamb[i]   - costs.all
      NB.DX       <- QALYs.DX*lamb[i]    - costs.DX
      NB.MP       <- QALYs.MP*lamb[i] - costs.MP
      NB.PS       <- QALYs.PS*lamb[i] - costs.PS  
      NB.ROR_P60  <- QALYs.ROR_P60*lamb[i] - costs.ROR_P60      
      CE.DX     <- ifelse(NB.DX > NB.all, 1, 0)
      CE.MP     <- ifelse(NB.MP > NB.all, 1,0)
      CE.PS     <- ifelse(NB.PS > NB.all, 1, 0)     
      CE.ROR_P60<- ifelse(NB.ROR_P60 > NB.all, 1, 0) 
      prob.CE.DX      <- mean(CE.DX) 
      prob.CE.MP      <- mean(CE.MP)
      prob.CE.PS      <- mean(CE.PS)         
      prob.CE.ROR_P60 <- mean(CE.ROR_P60)   
	  
    OUT[i,] <- c(prob.CE.DX,prob.CE.MP,prob.CE.PS,prob.CE.ROR_P60)
}


plot(lamb,OUT[,1],type="l", ylim = c(0,1), xlim = c(0,50000), xaxp  = c(0, 50000, 5), col = "black", xlab = "Willingness-to-pay threshold", ylab = "Probability cost-effective")
lines(lamb,OUT[,1],type="l", ylim = c(0,1), xlim = c(0,50000), col = "red")
lines(lamb,OUT[,2],type="l", ylim = c(0,1), xlim = c(0,50000), col = "orange")
lines(lamb,OUT[,3],type="l", ylim = c(0,1), xlim = c(0,50000), col = "green")    
lines(lamb,OUT[,4],type="l", ylim = c(0,1), xlim = c(0,50000), col = "blue")  
labels <- c("Oncotype DX (25)","MammaPrint","Prosigna Subtype","Prosigna ROR (60)")
cols <- c("red","orange","green","blue","blue")
legend(20000,0.5,labels, col = cols, lty = 1, cex = 1)

#write.csv(OUT,"CEAC vs chemo for all.csv",row.names=FALSE)

### CEAF ###

OUT <- list()
lamb <- seq(1000,200000,200)

for (i in 1:length(lamb)) {
  
  NB.all      <- QALYs.all*lamb[i]   - costs.all
  NB.DX       <- QALYs.DX*lamb[i]    - costs.DX
  NB.MP       <- QALYs.MP*lamb[i] - costs.MP
  NB.PS       <- QALYs.PS*lamb[i] - costs.PS  
  NB.ROR_P60  <- QALYs.ROR_P60*lamb[i] - costs.ROR_P60    
  maxNB   <- ifelse(NB.all >= NB.DX, NB.all, NB.DX) 
  maxNB   <- ifelse(maxNB >= NB.MP, maxNB, NB.MP)
  maxNB   <- ifelse(maxNB >= NB.PS, maxNB, NB.PS)             
  maxNB   <- ifelse(maxNB >= NB.ROR_P60, maxNB, NB.ROR_P60)   
  CE.all    <- ifelse(NB.all == maxNB, 1,0)
  CE.DX     <- ifelse(NB.DX == maxNB, 1, 0)
  CE.MP     <- ifelse(NB.MP == maxNB, 1,0)
  CE.PS     <- ifelse(NB.PS == maxNB, 1, 0)     
  CE.ROR_P60<- ifelse(NB.ROR_P60 == maxNB, 1, 0) 
  prob.CE.all     <- mean(CE.all) 
  prob.CE.DX      <- mean(CE.DX) 
  prob.CE.MP      <- mean(CE.MP)
  prob.CE.PS      <- mean(CE.PS)         
  prob.CE.ROR_P60 <- mean(CE.ROR_P60)   
  EVPI <- mean(maxNB) - max(mean(NB.all), mean(NB.DX),mean(NB.MP), mean(NB.PS), mean(NB.ROR_P60))       
  mean.NB.all     <- mean(NB.all)
  mean.NB.DX      <- mean(NB.DX)
  mean.NB.MP      <- mean(NB.MP)
  mean.NB.PS      <- mean(NB.PS)         
  mean.NB.ROR_P60 <- mean(NB.ROR_P60)
  mean.NB.max   <- max(mean.NB.all,mean.NB.DX,mean.NB.MP,mean.NB.PS,mean.NB.ROR_P60)    
  ceaf      <- ifelse(mean.NB.max==mean.NB.all,prob.CE.all,0)
  ceaf      <- ifelse(mean.NB.max==mean.NB.DX, prob.CE.DX,ceaf)
  ceaf      <- ifelse(mean.NB.max==mean.NB.MP,prob.CE.MP,ceaf)
  ceaf      <- ifelse(mean.NB.max==mean.NB.PS,prob.CE.PS,ceaf)            
  ceaf      <- ifelse(mean.NB.max==mean.NB.ROR_P60,prob.CE.ROR_P60,ceaf) 
  ceaf.test      <- ifelse(mean.NB.max==mean.NB.all,1,0)
  ceaf.test      <- ifelse(mean.NB.max==mean.NB.DX, 2,ceaf)
  ceaf.test      <- ifelse(mean.NB.max==mean.NB.MP,3,ceaf)
  ceaf.test      <- ifelse(mean.NB.max==mean.NB.PS,5,ceaf)        
  ceaf.test      <- ifelse(mean.NB.max==mean.NB.ROR_P60,6,ceaf)   
  
  OUT[[i]] <- list(NB.all=NB.all,NB.DX=NB.DX,NB.MP=NB.MP,NB.PS=NB.PS,NB.ROR_P60=NB.ROR_P60,
                   maxNB=maxNB,CE.all=CE.all,CE.DX=CE.DX,CE.MP=CE.MP,CE.PS=CE.PS,CE.ROR_P60=CE.ROR_P60,
                   prob.CE.all=prob.CE.all, prob.CE.DX=prob.CE.DX,EVPI=EVPI,prob.CE.MP=prob.CE.MP,
                   prob.CE.PS=prob.CE.PS, prob.CE.ROR_P60=prob.CE.ROR_P60,
                   mean.NB.all = mean.NB.all, mean.NB.DX = mean.NB.DX, mean.NB.MP = mean.NB.MP,  
                   mean.NB.PS=mean.NB.PS, mean.NB.ROR_P60=mean.NB.ROR_P60,
                   ceaf=ceaf,ceaf.test=ceaf.test)
}



CEAF <- data.frame(lambda=lamb,Probability.CE = NA,test=NA)
for (i in 1:length(lamb)){
  CEAF$Probability.CE[i]  <- OUT[[i]]$ceaf
  CEAF$CE.all[i]          <- OUT[[i]]$prob.CE.all
  CEAF$CE.DX[i]           <- OUT[[i]]$prob.CE.DX
  CEAF$CE.MP[i]           <- OUT[[i]]$prob.CE.MP
  CEAF$CE.PS[i]           <- OUT[[i]]$prob.CE.PS        
  CEAF$CE.ROR_P60[i]      <- OUT[[i]]$prob.CE.ROR_P60   
  CEAF$test[i]            <- OUT[[i]]$ceaf.test
  
  CEAF$Probability.CE[i] <- OUT[[i]]$ceaf
}

plot(CEAF$lambda,CEAF$CE.all,type="l", ylim = c(0,1), xlim = c(0,50000), xaxp  = c(0, 50000, 5), col = "black", xlab = "Willingness-to-pay threshold", ylab = "Probability cost-effective")
lines(CEAF$lambda,CEAF$CE.DX,type="l", ylim = c(0,1), xlim = c(0,50000), col = "red")
lines(CEAF$lambda,CEAF$CE.MP,type="l", ylim = c(0,1), xlim = c(0,50000), col = "orange")
lines(CEAF$lambda,CEAF$CE.PS,type="l", ylim = c(0,1), xlim = c(0,50000), col = "green")    
lines(CEAF$lambda,CEAF$CE.ROR_P60,type="l", ylim = c(0,1), xlim = c(0,50000), col = "purple")  
lines(CEAF$lambda[seq(1,200,6)],CEAF$Probability.CE[seq(1,200,6)],type="p", ylim = c(0,1))
labels <- c("Chemo-for-all","Oncotype DX (25)","MammaPrint","Prosigna Subtype","Prosigna ROR (60)")
cols <- c("black","red","orange","green","blue","purple")
legend(25000,0.95,labels, col = cols, lty = 1, cex = 1)
#write.csv(CEAF, "CEAF_SA2.csv", row.names=FALSE)

##NB: to get CEAF data into excel columns, copy over csv data to excel, then go to data tab -> text to Columns-> follow instructions to get data converted in to table columns 



##---------- EVPPI ---------

NB.all = QALYs.all*lambda - costs.all
NB.DX = QALYs.DX*lambda - costs.DX
NB.MP = QALYs.MP*lambda - costs.MP
NB.PS       <- QALYs.PS*lambda - costs.PS 
NB.ROR_P60  <- QALYs.ROR_P60*lambda - costs.ROR_P60   

mean(NB.all)
mean(NB.DX)
mean(NB.MP)
mean(NB.PS)        
mean(NB.ROR_P60) 


NB.current <- max(mean(NB.all), mean(NB.DX), mean(NB.MP),mean(NB.ROR_P60), mean(NB.PS))

NBPPI <- scan("NBPPI_mort_chemo.data")    
EVPPI.mort_chemo <- mean(NBPPI) -  NB.current
EVPPI.mort_chemo*sum(I)

NBPPI <- scan("NBPPI_AML.data")  
EVPPI.AML <- mean(NBPPI) -  NB.current
EVPPI.AML*sum(I)

NBPPI <- scan("NBPPI_CHF.data")    
EVPPI.CHF <- mean(NBPPI) -  NB.current
EVPPI.CHF*sum(I)


NBPPI <- scan("EVPPI/data/SA1/NBPPI_DX.data")  
EVPPI.DX <- mean(NBPPI) -  NB.current
EVPPI.DX*sum(I)

NBPPI <- scan("EVPPI/data/SA1/NBPPI_MP.data")
EVPPI.MP <- mean(NBPPI) -  NB.current
EVPPI.MP*sum(I)

NBPPI <- scan("EVPPI/data/SA1/NBPPI_PS.data")
EVPPI.PS <- mean(NBPPI) -  NB.current
EVPPI.PS*sum(I)

NBPPI <- scan("EVPPI/data/SA1/NBPPI_ROR_P60.data")
EVPPI.ROR_P60 <- mean(NBPPI) -  NB.current
EVPPI.ROR_P60*sum(I)


### EVSI ####

NBSI <- scan("NBSI.data")
EVSI <- mean(NBSI - NB.current)
EVSI
EVSI*sum(I)
