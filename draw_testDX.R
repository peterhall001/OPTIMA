draw.testDX <- function() {

set.seed(seed)

## Oncotype DX (test 1)
propLow <<- rbeta(Nsim,sum(RS <=25, na.rm = TRUE),sum(RS >25, na.rm = TRUE)) 
propHigh <<- 1-propLow
propLowDX <<- propLow

#  test costs
ctestDX <<- rep(2576,Nsim)  # cost of Oncotype DX        

## recurrence rates

h.chemo.DXhigh <- apply(h.chemo.sim[,RS>25],1,mean)
#h.chemo.DXlow <- apply(h.chemo.sim[,RS<=25],1,mean)

h.horm.DXhigh  <- apply(h.horm.sim[,RS>25],1,mean)
h.horm.DXlow  <- apply(h.horm.sim[,RS<=25],1,mean)

## High risk (RS>25) with chemo

rRec.5<- h.chemo.DXhigh  #  annual event rate - standard , to yr 5
rRec.10 <- h.horm.DXhigh  #  annual event rate - standard , to yr 5 - 10
   
# Vector pRec for probability of recurrence by cycle ( high risk group with chemo):
pRec.DXhigh.chemo <- array(c(c(0:T),rep(NA,Nsim)),c(T,Nsim))
for(cycle in 0:T)  {
	pRec.DXhigh.chemo[cycle,] <- if (cycle <= 5) 1-exp(-rRec.5) else (if(cycle >5 & cycle <=10) 1-exp(-rRec.10) else 0)
} 
pRec.DXhigh.chemo <<- pRec.DXhigh.chemo

## Low risk (RS<=25) no chemo
rRec.5 <- h.horm.DXlow  #  annual event rate - standard , to yr 5
rRec.10 <- h.horm.DXlow  #  annual event rate - standard , to yr 5 - 10

# Vector pRec for probability of recurrence by cycle ( high risk group with chemo):
pRec.DXlow <- array(c(c(0:T),rep(NA,Nsim)),c(T,Nsim))
for(cycle in 0:T)  {
  pRec.DXlow[cycle,] <- if (cycle <= 5) 1-exp(-rRec.5) else (if(cycle >5 & cycle <=10) 1-exp(-rRec.10) else 0)
} 
pRec.DXlow <<- pRec.DXlow


## Low risk (RS<=25) chemo -- not used
#rRec.5 <- h.chemo.DXlow  #  annual event rate - standard , to yr 5
#rRec.10 <- h.horm.DXlow  #  annual event rate - standard , to yr 5 - 10

# Vector pRec for probability of recurrence by cycle ( high risk group with chemo):
#pRec.DXlow.chemo <<- array(c(c(0:T),rep(NA,Nsim)),c(T,Nsim))
#for(cycle in 0:T)  {
 # pRec.DXlow.chemo[cycle,] <<- if (cycle <= 5) 1-exp(-rRec.5) else (if(cycle >5 & cycle <=10) 1-exp(-rRec.10) else 0)
#} 

}