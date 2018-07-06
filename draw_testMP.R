draw.testMP <- function() {

set.seed(seed)
#
#  test costs
ctestMP <<- rep(2208,Nsim)  # cost of MammaPrint

test <- data1$MammaPrint_Result == "Low Risk"   # TRUE = low
test[is.na(test)] <- FALSE

# proportion MP low
propLowMP <<- rbeta(Nsim, sum(test, na.rm = TRUE), nrow(data1) - sum(test, na.rm = TRUE))
propHighMP <<- 1-propLowMP

#define new DX cutpoint for propHighMP
DXcut <- sort(RS)[length(RS)*mean(propLowMP)]

#Recalculate mean hazard rates for oncotype DX defined groups with MP cutpoint
h.chemo.DXhigh.test <- apply(h.chemo.sim[,RS>DXcut],1,mean)
h.chemo.DXlow.test <- apply(h.chemo.sim[,RS<=DXcut],1,mean)


h.chemo.testhigh <- apply(cbind(h.chemo.sim[,RS > DXcut & test == "FALSE"],h.chemo.sim.uncert[,RS <= DXcut & test == "FALSE"]),1,mean)

h.horm.testhigh  <- apply(h.horm.sim[,test == "FALSE"],1,mean) 
h.horm.testlow  <-  apply(h.horm.sim[,test == "TRUE"],1,mean) 



## MP High risk with chemo

rRec.5<- h.chemo.testhigh  #  annual event rate - standard , to yr 5
rRec.10 <- h.horm.testhigh  #  annual event rate - standard , to yr 5 - 10

# Vector pRec for probability of recurrence by cycle ( high risk group with chemo):
pRec.testhigh.chemo <- array(c(c(0:T),rep(NA,Nsim)),c(T,Nsim))
for(cycle in 0:T)  {
  pRec.testhigh.chemo[cycle,] <- if (cycle <= 5) 1-exp(-rRec.5) else (if(cycle >5 & cycle <=10) 1-exp(-rRec.10) else 0)
} 
pRec.MPhigh.chemo <<- pRec.testhigh.chemo

## MP Low risk no chemo
rRec.5 <- h.horm.testlow  #  annual event rate - standard , to yr 5
rRec.10 <- h.horm.testlow  #  annual event rate - standard , to yr 5 - 10

# Vector pRec for probability of recurrence by cycle ( high risk group with chemo):
pRec.testlow <- array(c(c(0:T),rep(NA,Nsim)),c(T,Nsim))
for(cycle in 0:T)  {
  pRec.testlow[cycle,] <- if (cycle <= 5) 1-exp(-rRec.5) else (if(cycle >5 & cycle <=10) 1-exp(-rRec.10) else 0)
} 
pRec.MPlow <<- pRec.testlow

}