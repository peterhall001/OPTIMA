
#data <- read.csv("data2-4-14.csv")
data <- read.csv("302_tests_10-10-14.csv")
#data.nodes <- data[data$randnode == "None",]

data1 <- data[!is.na(data$Recurscore),]

startage <- round(mean(data1$Age,rm.na = TRUE))
H <- 100 - startage # set time horizon in years
T <- H+2  # set length of Markov trace

draw <- function() {



#  OPTIMAprelim parameter draw, 
# assuming Oncotype DX cutoff >25 for High risk group, outcomes from SWOG8814 trial

set.seed(seed)

# inflator:
inf <<- 1.066 # 2008 --> 2011


#===== Chemotherapy assignment  ==============================================================================================================================


#1 FEC75-80
#2 FEC90-100
#3 TC
#4 FEC-T
#5 E-CMF
#6 FEC-Pw

#chemo.type <- rdirichlet(Nsim,c(sum(data1$IntendedCT == 1, na.rm = TRUE),
#                                sum(data1$IntendedCT == 2, na.rm = TRUE),
#                                sum(data1$IntendedCT == 3, na.rm = TRUE),
#                                sum(data1$IntendedCT == 4, na.rm = TRUE),
#                                sum(data1$IntendedCT == 5, na.rm = TRUE),
#                               sum(data1$IntendedCT == 6, na.rm = TRUE)))
# 
chemo.type <- rdirichlet(Nsim,c(sum(data1$randIntendedCT == "FEC75-80", na.rm = TRUE),
                                sum(data1$randIntendedCT == "FEC90-100", na.rm = TRUE),
                                sum(data1$randIntendedCT == "TC", na.rm = TRUE),
                                sum(data1$randIntendedCT == "FEC-T", na.rm = TRUE),
                                sum(data1$randIntendedCT == "E-CMF", na.rm = TRUE),
                                sum(data1$randIntendedCT == "FEC-Pw", na.rm = TRUE)))
								
pFEC75  <<- chemo.type[,1]
pFEC    <<- chemo.type[,2]
pTC     <<- chemo.type[,3]
pFECT   <<- chemo.type[,4]
pEpiCMF <<- chemo.type[,5]
pFECPw  <<- chemo.type[,6] 

#============= TRANSITION PROBABILITIES =====================================================================================================================

propLR <<- rbeta(Nsim,292,663)      # mean = 0.31   # proportion of recurrences that are local

### disease recurrence 

rfs10 <<- data1$totARFS_10yr_horm/100 # 10 year RFS from Adjuvant!
RS    <<- data1$Recurscore

h <<- -log(rfs10)/10 # annual hazard, assuming exponential survival distribution

h.mean <- mean(h)
h.var  <- (sd(h)*2)^2

h.mu <- log(h^2/sqrt(h.var+h^2))
h.sigma <- sqrt(log(1+h.var/h^2))

h.horm.sim <- array(NA,c(Nsim,(length(h))))
for (z in 1:length(h)) {
  h.horm.sim[,z] <- rlnorm(Nsim,h.mu[z],h.sigma[z])# rep(mean(h),Nsim)
}
h.horm.sim <<- h.horm.sim

h.horm <<- apply(h.horm.sim,1,mean)

# logHR for RFS = alpha + beta*RS (RS = recurrence score) = predictive effect from SWOG-8814
alpha.mu    <<- 0.4541
alpha.se <- 0.03749
beta.mu     <<- -0.0238
beta.se  <- 0.00418
covar <- -0.5*alpha.se*beta.se

coeff <- mvrnorm(Nsim,c(alpha.mu,beta.mu),matrix(c(alpha.se^2,covar,covar,beta.se^2),2,2))

logHR.sim <- array(NA,c(Nsim,length(rfs10)))
h.chemo.sim <- logHR.sim
for (j in 1:Nsim) {
  logHR.sim[j,] <- coeff[j,1] + coeff[j,2]*RS 
  h.chemo.sim[j,] <- h.horm.sim[j,]*exp(logHR.sim[j,])
}
h.chemo.sim <<- h.chemo.sim

h.chemo <- apply(h.chemo.sim,1,mean)

# Calculate uncertain prior for HR for alternative tests
m <- 1
v <- 10
mu <- log(m^2/sqrt(v+m^2))
sigma <- sqrt(log(1+v/m^2))
HR.uncert <<- exp(rnorm(Nsim,mu,sigma))

#Calculate mean hazard rates based on uncertain prior and MP defined groups

h.chemo.sim.uncert <- array(NA,c(Nsim,length(rfs10)))
for (j in 1:Nsim) {
  h.chemo.sim.uncert[j,] <- h.horm.sim[j,]*HR.uncert[j]
}
h.chemo.sim.uncert <<- h.chemo.sim.uncert




## All chemo ; all RS
rRec.5 <- h.chemo  #  annual event rate - standard , to yr 5
rRec.10 <- h.horm  #  annual event rate - standard , to yr 6 - 10
# Vector pRec for probability of recurrence by cycle (all chemo):
pRec.all <- array(c(c(0:T),rep(NA,Nsim)),c(T,Nsim))
for(cycle in 0:T)  {
	pRec.all[cycle,] <- if (cycle <= 5) 1-exp(-rRec.5) else (if(cycle >5 & cycle <=10) 1-exp(-rRec.10) else 0)
} 
pRec.all <<- pRec.all

## constant post-recurrence survival (base case)
pDeath_DR <<- rbeta(Nsim,1.00,2.35) #rbeta(Nsim, 2.790256, 4.185383)     #  = for all, with chemo ; old paprameter = mean(rbeta(Nsim,100,235))
pDeath_DR_low <<- rbeta(Nsim,1.00,2.35)# rbeta(Nsim,0.4201557,2.580957)  # SWOG 8814 no chemo <18

## variable post-recurrence survival (sens analysis 1)
#pDeath_DR <<- rbeta(Nsim, 2.790256, 4.185383)     #  = for all, with chemo ; old paprameter = mean(rbeta(Nsim,100,235))
#pDeath_DR_low <<- rbeta(Nsim,0.4201557,2.580957)  # SWOG 8814 no chemo <18


## congestive heart failure
pop_CHF <- rep(NA,102)
b1 <- -12.9273605 #rnorm(1,-12.9273605,0.67000518)            # fit to ONS CHF female incidence data per age log(annual prob) = b1 + b2.age
b2 <- 0.09409354  #rnorm(1,0.09409354,0.027661548)
for (a in 1:102){
	pop_CHF[a] <- exp(b1 + b2*a)                # annual probability of developing CHF
}   #  pop_CHF = non-linear least squares regression fit to UK female population incidence data


pdeath_CHF <<- rbeta(Nsim,136,84)                      # mean = 0.6 # annual probability of death from CHF

hr_CHF_anthra <- exp(rnorm(Nsim,0.458,0.191))  # HR for CHF after anthracycline treatment
#hr_CHF_anthra <- rep(1,Nsim) # HR for CHF after anthracycline treatment - set at 1

prob_CHF <<- array(c(0,0),c(102,Nsim)) # = annual age adjusted probability of CHF attributable to chemo (i.e. excess)
r_CHF <- array(c(0,0),c(102,Nsim)) # = annual age adjusted rate of CHF attributable to chemo (i.e. excess)
for (c in 1:102) {
	r_CHF[c,] <- (-log(1-pop_CHF[c])*hr_CHF_anthra - (-log(1-pop_CHF[c]))) *(pFEC+pFECT)
	prob_CHF[c,] <- 1-exp(-r_CHF[c,])
	prob_CHF[c,] <- ifelse(prob_CHF[c,] >= 0, prob_CHF[c,], 0)
}
prob_CHF <<- prob_CHF


## AML
pop_AML <- rep(296/100000,Nsim)              # annual rate of AML (females >65)
rr_AML <- rlnorm(Nsim,1.71,0.79)         # rate ratio AML with chemo
prob_AML <<- 1-exp(-pop_AML*rr_AML)                     # annual excess rate of AML with chemo

rmr_AML <<-  -log(1-rbeta(Nsim,1524,60))/5  # excess annual hazard after AML



# lifetable - e.g. mr[1]=annual probability of death at age 1 year (minus breast cancer specific mortality)

mr <<- c(0.000353, 0.000193, 0.000161, 0.000117, 0.000096, 0.000098, 0.000082, 0.000090, 0.000078, 0.000093,
	0.000097, 0.000100, 0.000118, 0.000119, 0.000158, 0.000173, 0.000245, 0.000271, 0.000257, 0.000241, 
	0.000264, 0.000263, 0.000247, 0.000294, 0.000287, 0.000337, 0.000311, 0.000358, 0.000381, 0.000416, 
	0.000406, 0.000487, 0.000539, 0.000575, 0.000598, 0.000644, 0.000727, 0.000795, 0.000897, 0.000980, 
	0.001056, 0.001151, 0.001267, 0.001341, 0.001530, 0.001648, 0.001828, 0.002067, 0.002151, 0.002559, 
	0.002692, 0.002861, 0.003158, 0.003537, 0.003755, 0.004141, 0.004390, 0.004717, 0.005303, 0.005696, 
	0.006452, 0.006907, 0.007798, 0.008516, 0.009178, 0.010084, 0.011251, 0.012330, 0.013535, 0.015072, 
	0.016561, 0.018374, 0.020832, 0.023483, 0.025871, 0.029214, 0.032712, 0.036780, 0.041825, 0.047053, 
	0.052661, 0.058476, 0.066223, 0.074507, 0.083203, 0.092440, 0.101085, 0.114035, 0.124557, 0.140443, 
	0.160738, 0.179041, 0.197859, 0.215442, 0.234414, 0.255550, 0.271716, 0.300529, 0.314942, 0.5,0.5,0.5)    #  0.341322)

# excess mortality during first year on chemo
mort_chemo <<- rbeta(Nsim,1.6,677.6)

#======================= UTILITY PARAMETERS =================================================================================================================

# female age-group-specific norms
U <<- NA
U[1:24] <<- 0.94
U[25:34] <<- 0.93
U[35:44] <<- 0.91
U[45:54] <<- 0.85
U[55:64] <<- 0.81
U[65:74] <<- 0.78
U[75:102] <<- 0.71

#utility decrements
uDFdec       <<- rlnorm(Nsim,-8.117,2.148) # disease free
uDFdec.chemo <<- rlnorm(Nsim,-2.365,0.325) # disease free first year on chemo
uLRdec       <<- rlnorm(Nsim,-2.290,0.359) # local recurrence
uDRdec       <<- rlnorm(Nsim,-1.317,0.496) # distant recurrence

uCHF <<- rbeta(Nsim,103.2988905,92.34294761) # mean 0.528	se 0.356 (congestive cardiac failure)	
uAML <<- rbeta(Nsim,2,2)                   # mean 0.5 se 0.21 (AML - estimate )

#======================= COST PARAMETERS ====================================================================================================================

# Recurrence states				
cDF    <<- rlnorm(Nsim,6.91,0.004)
cLR    <<- rlnorm(Nsim,8.72,0.08)  # exp(rnorm(Nsim,9.575578252,0.255341826))   # Local recurrence (first year)
cDFaLR <<- rlnorm(Nsim,7.20,0.111) # Disease free after local recurrence (2nd and subsequent years)
cDR    <<- rlnorm(Nsim,7.43,0.019) # exp(rnorm(Nsim,9.351999553,0.072394583))   # Distant recurrence        
#cTerm6 <-  exp(rnorm(Nsim,8.578, 0.0228)) # Terminal 6 months 
cTerm3 <-  rlnorm(Nsim,7.63,0.003) # terminal 3 months
cTerm  <<- cTerm3 - (cDR/4)        # Extra terminal cost		    
				
# Follow-up 				
cMedOnc1 <- 172 # Medical Oncology clinic  mean = ?296.42
cMedOnc2 <- 139 #exp(rnorm(Nsim,5.517925478,0.024919329))  # Medical Oncology clinic  mean = ?296.42
cMammo  <- exp(rnorm(Nsim,3.695585469,0.192024247))  # Mammogram  mean = ?39.93


cSpN <- exp(rnorm(Nsim,4.63,0.04)) #  medical oncology specialist nurse mean 102.18
				
# Drug treatment Costs

cBloods <- 10.39
cDeliver <- exp(rnorm(Nsim,5.37,0.03))  # Delivery cost	214
cLine <- 18.17

cFEC <<- 6*(57.73 + 145.21 + cBloods + cDeliver + cSpN) + cMedOnc1 + cMedOnc2 + cLine
cFEC75 <<- 6*(57.73 + 13.46 + cBloods + cDeliver + cSpN) + cMedOnc1 + cMedOnc2  + cLine   
cTC <<- 4*(52.80 + 3.98 + cDeliver + cBloods + cSpN) +  cMedOnc1 + cMedOnc2 + cLine
cFECT <<- 3*(57.73 + 145.21 + cBloods + cDeliver + cSpN) + 3*(44.55 + 267.48 + cBloods + cDeliver + cSpN) + cMedOnc1 + cMedOnc2*2 + cLine
cFECPw <<- 3*(57.73 + 145.21 + cBloods + cDeliver + cSpN) + 3*(33.78 + 0.08 +cBloods*3 + cDeliver*3 + cSpN) + cMedOnc1 + cMedOnc2*2 + cLine  #drug costs changed to 33.78 and 0.08 (were 44.55 and 267.48)
cEpiCMF <<- 4*(27.80 + 0.03 + cBloods + cDeliver + cSpN) + 4*(70.20 + 0.36 +cBloods*2 + cDeliver*2 + cSpN) + cMedOnc1 + cMedOnc2*2 + cLine
#cFECA  <<- 3*(57.73 + 145.21 + cBloods + cDeliver + cSpN) + 1*(44.55 + 267.48 + cBloods + cDeliver + cSpN) + 3*(1230 + 267.48 + cBloods +cDeliver + cSpN) + cMedOnc1 + cMedOnc2*3 + cLine
cTreat   <<- pFEC*cFEC + pFEC75*cFEC75 + pTC*cTC + pFECT*cFECT + cFECPw*pFECPw + cEpiCMF*pEpiCMF #+ pFECA*cFECA  # 1st year total treatment costs 

cMUGA    <<- exp(rnorm(Nsim,5.014643234,0.206042899)) # MUGA	
						
# Heart failure				
cCHF <<-  exp(rnorm(Nsim,log(2338.71)-(2.5^2)/2,2.5)) # Annual cost of heart failure ?2,338.71

# AML
cAML <<- exp(rnorm(Nsim,8.401479,0.85))

      
#======================= TOXICITY PARAMETERS =========================================================================================================================================


# Febrile neutropenia	
toxNeut.FEC <- rbeta(Nsim,84,911)   
toxNeut.FECT <- rbeta(Nsim,112,889)
toxNeut.TC <- rbeta(Nsim,23,483)
toxNeut.EpiCMF <- rbeta(Nsim,137,892)

ctoxNeut.short <- rlnorm(Nsim,6.67,0.46974)        #Using hospital short stay cost for toxicity event
ctoxNeut.long <- rlnorm(Nsim,8.156,0.03800)      #Using hospital long stay cost for toxicity event
ctoxNeut <- ctoxNeut.short*0.5 + ctoxNeut.long*0.5

# Allergic reaction	
toxAll <- rbeta(Nsim,1.46,363.54)

ctoxAll.short <- rlnorm(Nsim,6.19,0.055)         
ctoxAll.long <- rlnorm(Nsim,7.4951,0.0966)       
ctoxAll <- ctoxAll.short*0.5 + ctoxAll.long*0.5

# Nausea	0.021
toxNau.FEC <- rbeta(Nsim,204,791)
toxNau.FECT <- rbeta(Nsim,112,889)
toxNau.TC <- rbeta(Nsim,15,491)        
toxNau.EpiCMF <- rbeta(Nsim,24,1005)

ctoxNau.short <- rlnorm(Nsim,5.83,0.018)         
ctoxNau.long <- rlnorm(Nsim,6.749, 0.080)        
ctoxNau <- ctoxNau.short*0.5 + ctoxNau.long*0.5

# Symptomatic heart failure      -applied to population incidence instead. 
#toxCHF <- rbeta(Nsim,4,991)                          

#ctoxCHF <- rlnorm(Nsim,7.757354774,0.25)    #Assuming short and long stay costs are equal here (from Lorgelly et al., no data for separate short/long stay costs?)
                                            	
													
# Diarrhoea
toxDiarr.FEC <- rbeta(Nsim,1,996)
toxDiarr.FECT <- rbeta(Nsim,1,1002)
toxDiarr.TC <- rbeta(Nsim,12,494)
toxDiarr.EpiCMF <- rbeta(Nsim,46,983)

ctoxDiarr.short <- exp(rnorm(Nsim,5.87, 0.018))  
ctoxDiarr.long <- exp(rnorm(Nsim,7.009,0.039))   
ctoxDiarr <- ctoxDiarr.short*0.5 + ctoxDiarr.long*0.5

# Anaemia
toxAn.FEC <- rbeta(Nsim,14,981)
toxAn.FECT <- rbeta(Nsim,7,994)
toxAn.TC <- rbeta(Nsim,5,501)
toxAn.EpiCMF <- rbeta(Nsim,31,998)

ctoxAn.short <- rlnorm(Nsim,6.47,0.043)          
ctoxAn.long <- rlnorm(Nsim,6.994,0.129)          
ctoxAn <- ctoxAn.short*0.5 + ctoxAn.long*0.5

# thrombocytopenia
toxThrom.FEC <- rbeta(Nsim,3,992)    
toxThrom.FECT <- rbeta(Nsim,4,997)
toxThrom.TC <- rbeta(Nsim,2,504)
toxThrom.EpiCMF <- rbeta(Nsim,10,1019)  

ctoxThrom.short <- rlnorm(Nsim,6.29,0.0001)    #Short stay cost
ctoxThrom.long <- rlnorm(Nsim,7.141,0.274)       #Long stay cost
ctoxThrom <- ctoxThrom.short*0.5 + ctoxThrom.long*0.5

# Stomatitis
toxStom.FEC <- rbeta(Nsim,40,955)    
toxStom.FECT <- rbeta(Nsim,59,942)
toxStom.TC <- rbeta(Nsim,4,502)
toxStom.EpiCMF <- rbeta(Nsim,1,1030)

ctoxStom.short <- rlnorm(Nsim,5.95,0.144)        
ctoxStom.long <- rlnorm(Nsim,7.33,0.20116)        
ctoxStom <- ctoxStom.short*0.5 + ctoxStom.long*0.5

cTox.FEC  <- toxNeut.FEC*ctoxNeut + toxNau.FEC*ctoxNau + toxDiarr.FEC*ctoxDiarr +  toxAn.FEC*ctoxAn + toxThrom.FEC*ctoxThrom + toxStom.FEC*ctoxStom
cTox.FEC75 <- cTox.FEC*0.66666
cTox.FECT <- toxNeut.FECT*ctoxNeut + toxNau.FECT*ctoxNau + toxDiarr.FECT*ctoxDiarr +  toxAn.FECT*ctoxAn + toxThrom.FECT*ctoxThrom + toxStom.FECT*ctoxStom
cTox.TC   <- toxNeut.TC*ctoxNeut + toxNau.TC*ctoxNau + toxDiarr.TC*ctoxDiarr +  toxAn.TC*ctoxAn + toxThrom.TC*ctoxThrom + toxStom.TC*ctoxStom
cTox.FECPw   <- cTox.FECT
cTox.EpiCMF <- toxNeut.EpiCMF*ctoxNeut + toxNau.EpiCMF*ctoxNau + toxDiarr.EpiCMF*ctoxDiarr +  toxAn.EpiCMF*ctoxAn + toxThrom.EpiCMF*ctoxThrom + toxStom.EpiCMF*ctoxStom

cTox <<- cTox.FEC75*pFEC75 + cTox.FEC*pFEC + cTox.FECT*pFECT + cTox.TC*pTC + cTox.FECPw*pFECPw + cTox.EpiCMF*pEpiCMF

}

