draw.testROR_P60 <- function() {
  
  set.seed(seed)
  #
  #  test costs
  ctestROR_P60 <<- rlnorm(Nsim,7.422 ,0.030)  # cost of test
  
  test.temp <- data1$ROR_PT > 60 | is.na(data1$ROR_PT)  # TRUE = high
  test <- test.temp == "FALSE"  # TRUE = low
  test[is.na(test)] <- FALSE
  
  # proportion MP low
  propLowROR_P60 <<- rbeta(Nsim, sum(test, na.rm = TRUE), nrow(data1) - sum(test, na.rm = TRUE))
  propHighROR_P60 <<- 1-propLowROR_P60
  
  
  #define new DX cutpoint for propHighMP
  DXcut <- sort(RS)[length(RS)*mean(propLowROR_P60)]
  
  #Recalculate mean hazard rates for oncotype DX defined grouROR_P with MP cutpoint
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
  pRec.ROR_P60high.chemo <<- pRec.testhigh.chemo
  
  
  ## MP Low risk no chemo
  rRec.5 <- h.horm.testlow  #  annual event rate - standard , to yr 5
  rRec.10 <- h.horm.testlow  #  annual event rate - standard , to yr 5 - 10
  
  # Vector pRec for probability of recurrence by cycle ( high risk group with chemo):
  pRec.testlow <- array(c(c(0:T),rep(NA,Nsim)),c(T,Nsim))
  for(cycle in 0:T)  {
    pRec.testlow[cycle,] <- if (cycle <= 5) 1-exp(-rRec.5) else (if(cycle >5 & cycle <=10) 1-exp(-rRec.10) else 0)
  } 
  pRec.ROR_P60low <<- pRec.testlow
  
}