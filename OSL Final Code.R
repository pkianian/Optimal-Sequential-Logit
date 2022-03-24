#                 SEQUENTIAL DECISIONS

## R code to estimate a sequential decision model 



#                          PART 1:  THE SETUP
start=Sys.time()  # start time
start
# Put a title for the run in the quotes below, to be printed at the top of the output file.

cat("sequential decision makin", "\n")
setwd("C:/Users/pkianian/Dropbox/Sequential Decisions/")
# DATA
# 
library(DEoptim)
library(maxLik)
library(foreign)  #Load package that reads .csv files
spmkt1 <- read.csv("C:/Users/pkianian/Dropbox/Sequential Decisions/Datasets/bootstrap datasets/XMATBOOT50 1.csv", header=TRUE, sep=",", fill=TRUE, row.names=NULL)
spmkt2 <- read.csv("C:/Users/pkianian/Dropbox/Sequential Decisions/Datasets/bootstrap datasets/XMATBOOT50 2.csv", header=TRUE, sep=",", fill=TRUE, row.names=NULL) 


d <- rbind(spmkt1, spmkt2)

attach(d)

start=Sys.time()  # start time

# Number of choice situations in dataset.

NCS=6187 

# Number of alternatives in each choice situation

NALT=3  

#1. impulse purchase
#2. no buy
#3. put on shopping list


# This is the number of rows of data in XMAT below.

NROWS= NCS*NALT

# Load and/or create XMAT, a matrix that contains the data.
#
# XMAT must contain one row of data for each alternative in each choice situation.
# The rows are grouped by choice situations.
# The number of rows in XMAT must be NROWS, specified above.
# The columns in XMAT are variable that describe the alternative.
# 
# The *first* column of XMAT identifies the choice situation. 
# The choice situations must be numbered sequentially from 1 to NCS, in ascending order.
# The *second* column of XMAT identifies the alternative for each choice situation.
# All alternatives for a given choice situation must be grouped together.
# The alternatives are numbered sequentially from 1 to NALT, in ascending order.
# The *third* column of XMAT identifies the chosen alternatives (1 for
# chosen, 0 for not). One and only one alternative must be chosen for each
# choice situation.
# The remaining columns of XMAT can be any variables.


XMAT=cbind(idcase, 
           idalt, 
           depvar, 
           Age, 
           household_size, 
           Preplan_shopping, 
           Sex, 
           Trait_impulsiveness, 
           Hedonic, 
           Ready_To_Use, 
           Sale_proneness, 
           Unhealthiness_Index, 
           Value_consciousness, 
           Variety_seeking, 
           Sale, 
           Display, 
           New_Product, 
           price_difference_ratio)   #Matrix including all variables in the dataset



bootsample = 6187                 #Bootstrapping sample size
bootiter = 100                    #Number of bootstrap iterations

NROWS2 = bootsample*NALT


Age_q25 = matrix(data =  quantile(Age, c(0.25)) ,nrow = NCS, ncol=1)
Age_q75 = matrix(data =  quantile(Age, c(0.75)) ,nrow = NCS, ncol=1)

household_size_q25 = matrix(data =  quantile(household_size , c(0.25)) ,nrow = NCS, ncol=1)
household_size_q75 = matrix(data = quantile(household_size , c(0.75)) ,nrow = NCS, ncol=1)

Preplan_shopping_q25 = matrix(data =  quantile(Preplan_shopping, c(0.25)) ,nrow = NCS, ncol=1)
Preplan_shopping_q75 = matrix(data =  quantile(Preplan_shopping, c(0.75)) ,nrow = NCS, ncol=1)

Unhealthiness_Index_q25 = matrix(data = quantile(Unhealthiness_Index, c(0.25)) ,nrow = NCS, ncol=1)
Unhealthiness_Index_q75 = matrix(data = quantile(Unhealthiness_Index, c(0.75)) ,nrow = NCS, ncol=1)

Trait_impulsiveness_q25 = matrix(data =  quantile(Trait_impulsiveness, c(0.25)) ,nrow = NCS, ncol=1)
Trait_impulsiveness_q75 = matrix(data =  quantile(Trait_impulsiveness, c(0.75)) ,nrow = NCS, ncol=1)

Sale_proneness_q25 = matrix(data = quantile(Sale_proneness, c(0.25)) ,nrow = NCS, ncol=1)
Sale_proneness_q75 = matrix(data = quantile(Sale_proneness, c(0.75)) ,nrow = NCS, ncol=1) 

Value_consciousness_q25 = matrix(data = quantile(Value_consciousness, c(0.25))  ,nrow = NCS, ncol=1)
Value_consciousness_q75 = matrix(data = quantile(Value_consciousness, c(0.75))  ,nrow = NCS, ncol=1)

Variety_seeking_q25 = matrix(data = quantile(Variety_seeking, c(0.25))  ,nrow = NCS, ncol=1)
Variety_seeking_q75 = matrix(data = quantile(Variety_seeking, c(0.75))  ,nrow = NCS, ncol=1) 

price_difference_ratio_q25 = matrix(data =  quantile(price_difference_ratio, c(0.25)) ,nrow = NCS, ncol=1)
price_difference_ratio_q75 = matrix(data =  quantile(price_difference_ratio, c(0.75)) ,nrow = NCS, ncol=1) 

Sex_1 = matrix(data = 1 ,nrow = NCS, ncol=1)
Sex_0 = matrix(data = 0 ,nrow = NCS, ncol=1)

Hedonic_1 = matrix(data = 1 ,nrow = NCS, ncol=1)
Hedonic_0 = matrix(data = 0 ,nrow = NCS, ncol=1)

Ready_To_Use_1 = matrix(data = 1 ,nrow = NCS, ncol=1)
Ready_To_Use_0 = matrix(data = 0 ,nrow = NCS, ncol=1)

Sale_1 = matrix(data = 1 ,nrow = NCS, ncol=1)
Sale_0 = matrix(data = 0 ,nrow = NCS, ncol=1)

Display_1 = matrix(data = 1 ,nrow = NCS, ncol=1)
Display_0 = matrix(data = 0 ,nrow = NCS, ncol=1)

New_Product_1 = matrix(data = 1 ,nrow = NCS, ncol=1)
New_Product_0 = matrix(data = 0 ,nrow = NCS, ncol=1)


#empty vectors to put results of bootsrapping in:

omegaboot= vector()
c_phiboot= vector()

ksi1_boot = vector()
ksi2_boot = vector()
ksi12_boot = vector()

chi11boot = vector()
chi12boot = vector()
chi13boot = vector()
chi14boot = vector()


beta31boot= vector()
beta32boot= vector()
beta33boot= vector()


alpha11boot= vector()
alpha12boot= vector()
alpha13boot= vector()
alpha14boot= vector()
alpha15boot= vector()
alpha16boot= vector()
alpha17boot= vector()
alpha18boot= vector()


alpha21boot= vector()
alpha22boot= vector()
alpha23boot= vector()
alpha24boot= vector()
alpha25boot= vector()
alpha26boot= vector()
alpha27boot= vector()
alpha28boot= vector()


E_Age_impulse = vector()
E_Age_nobuy = vector()
E_Age_plan = vector()

E_household_size_impulse = vector()
E_household_size_nobuy = vector()
E_household_size_plan = vector()

E_Preplan_shopping_impulse = vector()
E_Preplan_shopping_nobuy = vector()
E_Preplan_shopping_plan = vector()

E_Unhealthiness_Index_impulse = vector()
E_Unhealthiness_Index_nobuy = vector()
E_Unhealthiness_Index_plan = vector()

E_Sex_impulse = vector()
E_Sex_nobuy = vector()
E_Sex_plan = vector()

E_Trait_impulsiveness_impulse = vector()
E_Trait_impulsiveness_nobuy = vector()
E_Trait_impulsiveness_plan = vector()

E_Hedonic_impulse = vector()
E_Hedonic_nobuy = vector()
E_Hedonic_plan = vector()

E_Ready_To_Use_impulse = vector()
E_Ready_To_Use_nobuy = vector()
E_Ready_To_Use_plan = vector()

E_Sale_proneness_impulse = vector()
E_Sale_proneness_nobuy = vector()
E_Sale_proneness_plan = vector()

E_Value_consciousness_impulse = vector()
E_Value_consciousness_nobuy = vector()
E_Value_consciousness_plan = vector()

E_Variety_seeking_impulse = vector()
E_Variety_seeking_nobuy = vector()
E_Variety_seeking_plan = vector()

E_Sale_impulse = vector()
E_Sale_nobuy = vector()
E_Sale_plan = vector()

E_Display_impulse = vector()
E_Display_nobuy = vector()
E_Display_plan = vector()

E_New_Product_impulse = vector()
E_New_Product_nobuy = vector()
E_New_Product_plan = vector()

E_price_difference_ratio_impulse = vector()
E_price_difference_ratio_nobuy = vector()
E_price_difference_ratio_plan = vector()


# UB = c(8.87E+01, 
#        9.26E+01, 
#        8.25E+01, 
#        -5.96E+00, 
#        4.54E+00, 
#        6.17E+00, 
#        1.64E+01, 
#        3.04E-01, 
#        -1.91E+01, 
#        5.11E+00, 
#        2.51E+02, 
#        8.71E+01, 
#        -9.10E-01, 
#        3.98E+00, 
#        8.51E+00, 
#        4.40E+00, 
#        7.62E+01, 
#        2.56E+00, 
#        1.61E+02, 
#        1.43E+01, 
#        5.79E+00, 
#        -4.59E-01, 
#        4.36E+00, 
#        6.69E+00, 
#        7.15E+01, 
#        3.78E+01, 
#        4.38E+02, 
#        1.00E-02, 
#        6.11E-03)
# 
# 
# LB = c(6.21E+01, 
#        6.00E+01, 
#        4.88E+01, 
#        -2.27E+01, 
#        7.73E-01, 
#        -9.53E+00, 
#        -5.00E+00, 
#        -7.72E-01, 
#        -9.11E+01, 
#        2.28E-01, 
#        8.20E+01, 
#        5.96E+00, 
#        -1.11E+01, 
#        -9.03E+00, 
#        4.09E+00, 
#        -4.31E+00, 
#        1.76E+01, 
#        -3.47E+00, 
#        -3.93E+01, 
#        -4.56E+01, 
#        -2.10E+00, 
#        -6.93E+00, 
#        -8.93E-01, 
#        5.34E-01, 
#        -1.37E+02, 
#        -1.55E+02, 
#        1.28E+02, 
#        0, 
#        0)


UB = c(8.87E+01,
       9.26E+01,
       8.25E+01,
       -5.96E+00,
       
       4.54E+00,
       6.17E+00,
       1.64E+01,
       
       -1.91E+01,
       5.11E+00,
       2.51E+02,
       8.71E+01,
       -9.10E-01,
       3.98E+00,
       8.51E+00,
       4.40E+00,
       
       7.62E+01,
       2.56E+00,
       1.61E+02,
       1.43E+01,
       5.79E+00,
       -4.59E-01,
       4.36E+00,
       6.69E+00,
       
       7.15E+01,
       3.78E+01,
       4.38E+02,
       
       1.00E-02,
       6.11E-03)


LB = c(6.21E+01, 
       6.00E+01, 
       4.88E+01, 
       -2.27E+01, 
       
       7.73E-01, 
       -9.53E+00, 
       -5.00E+00, 
       
       -9.11E+01, 
       2.28E-01, 
       8.20E+01, 
       5.96E+00, 
       -1.11E+01, 
       -9.03E+00, 
       4.09E+00, 
       -4.31E+00, 
       
       1.76E+01, 
       -3.47E+00, 
       -3.93E+01, 
       -4.56E+01, 
       -2.10E+00, 
       -6.93E+00, 
       -8.93E-01, 
       5.34E-01, 
       
       -1.37E+02, 
       -1.55E+02, 
       1.28E+02, 
       
       0, 
       0)

one3=c(1, 1, 1)

for (bootjunk in 1:bootiter) {      # start bootstrap loop
  
  
  
  XMATBOOT = matrix(data = NA,nrow = NROWS, ncol=18)
  
  
  XMATBOOT<-XMAT[(1+((bootjunk-1)*NROWS)):(bootjunk*NROWS),]
  
  
  
  
  x=XMATBOOT[seq(1,nrow(XMATBOOT),3),4:6]		  #a (bootsample*NALT)x column vector
  q=XMATBOOT[seq(1,nrow(XMATBOOT),3),7:14]			#a (bootsample*NALT)x column vector
  z=XMATBOOT[seq(1,nrow(XMATBOOT),3),15:18]			#a (bootsample*NALT)x column vector
  y=XMATBOOT[,3]			    #a (bootsample*NALT)x column vector of 0 and 1
  
  dim(y)=c(NALT,bootsample)	#restructure y  so there are NALT rows and bootsample cols
  
  
  minus_x = x*-1		#vector of -x for each choice situation
  minus_q = q*-1		#vector of -q for each choice situation
  minus_z = z*-1		#vector of -z for each choice situation
  
  one=rep(1,bootsample)	    		#a column vector of ones with bootsample rows
  minus_one=rep(-1,bootsample)	#a column vector of -1 with bootsample rows
  
  
  M1or2=cbind(one,minus_q,q,minus_z)
  M121=cbind(one,q,minus_x)
  M122=cbind(one,q,minus_x)
  
  
  #choice parameters:
  #parameters include: chi1, beta3, alpha1, alpha2, gamma1, gamma3, miu1, omega, c_phi
  #B=(chi1(4), beta3(3), alpha1(8), alpha2(8), ksi1, ksi2, ksi12) initial value of parameters
  # chi1 = B[1:4]
  # beta3 = B[5:7]
  # alpha1 = B[8:15]
  # alpha2 = B[16:23]
  
  # ksi1 = B[24]
  # ksi2 = B[25]
  # ksi12 = B[26]
  
  
  
  #							PART 1 COMPUTING THE LOGLIKELIHOOD
  
  # Function to calculate loglikelihood function for nested model
  
  parama = vector(mode = "logical",length = 29)
  
  loglik=function(parama) { #curly bracket is the start of subrutine
    
    
    coef1or2=c(parama[26],parama[8:15],parama[16:23],parama[1:4])  	# Formula1or2: ksi12-q*alpha1+q*alpha2-z*chi1
    coef121=c(parama[24],parama[8:15],parama[5:7])		          # Formula121: ksi1+q*alpha1-x*beta3  
    coef122=c(parama[25],parama[16:23],parama[5:7])		          # Formula122: ksi2+q*alpha2-x*beta3
    
    
    # omega = parama[27]
    # c_phi = parama[28]
    
    V1=(M1or2%*%coef1or2)*parama[27]
    V2= -V1
    
    VV1=M121%*%coef121
    VV2=M122%*%coef122
    
    
    
    V1=exp(V1)
    V2=exp(V2)
    VV1=exp(VV1)
    VV2=exp(VV2)
    
    
    V3=(VV1+VV2)**(-parama[28])
    V4=(VV1+VV2)**(parama[28])
    
    
    #choice probabilities:
    
    p1=(one/(one+V1))*(one/(one+V3))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
    p2=(one/(one+V2))*(one/(one+V3))	#probability of no buy, a vector in which each element is p2 in each choice situation
    p3= one/(one+V4) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
    
    
    P=rbind(t(p1), t(p2), t(p3))			#(NALT x NCS) matrix in which each column has p1, p2 and p3 for each choice situation
    
    
    L1=y*P					#(NALT x NCS) matrix, elemenwise multiplication of prpbability of choice by dummy of chosen alternative
    
    
    prob = one3%*%L1
    add=t(rep(1e-16,bootsample))
    prob=prob+add
    
    
    LL=log(prob)				#one3%*%P gives a 1 x NCS vector of probability of choice for the chosen alternative in each choice situation and L is log of those probabilities
    
    
    LLsum = sum(LL, na.rm = TRUE)
    return(-LLsum)
    
  } #end of LOGLIK
  
  
  
  #        		                                                 PART 3. DO IT
  itmax = 1000
  population = 290
  
  cat('Start estimation',"\n")
  cat('Loop Number:',bootjunk,"\n")
  
  results=DEoptim(loglik,lower = LB, upper = UB, control = list(NP= population, storepopfrom = 1, trace=TRUE,itermax = itmax))
  
  
  #                                            PART 4.  HERE ARE THE RESULTS
  
  
  
  
  
  chi11boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,1]]
  chi12boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,2]]
  chi13boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,3]]
  chi14boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,4]]
  
  
  beta31boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,5]]
  beta32boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,6]]
  beta33boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,7]]
  
  
  
  alpha11boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,8]]
  alpha12boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,9]]
  alpha13boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,10]]
  alpha14boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,11]]
  alpha15boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,12]]
  alpha16boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,13]]
  alpha17boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,14]]
  alpha18boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,15]]
  
  
  
  alpha21boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,16]]
  alpha22boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,17]]
  alpha23boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,18]]
  alpha24boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,19]]
  alpha25boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,20]]
  alpha26boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,21]]
  alpha27boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,22]]
  alpha28boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,23]]
  
  
  
  ksi1_boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,24]]
  ksi2_boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,25]]
  ksi12_boot[bootjunk]=results[["member"]][["bestmemit"]][[itmax,26]]
  
  omegaboot[bootjunk] = results[["member"]][["bestmemit"]][[itmax,27]]
  c_phiboot[bootjunk] = results[["member"]][["bestmemit"]][[itmax,28]]
  
  
  #### start calculating partial elasticities:
  
  
  coef1or2=c(ksi12_boot[bootjunk],
             alpha11boot[bootjunk], alpha12boot[bootjunk], alpha13boot[bootjunk], alpha14boot[bootjunk], alpha15boot[bootjunk], alpha16boot[bootjunk], alpha17boot[bootjunk], alpha18boot[bootjunk], 
             alpha21boot[bootjunk], alpha22boot[bootjunk], alpha23boot[bootjunk], alpha24boot[bootjunk], alpha25boot[bootjunk], alpha26boot[bootjunk], alpha27boot[bootjunk], alpha28boot[bootjunk],
             chi11boot[bootjunk], chi12boot[bootjunk],chi13boot[bootjunk], chi14boot[bootjunk])  	# Formula1or2: ksi12-q*alpha1+q*alpha2-z*chi1
  
  coef121=c(ksi1_boot[bootjunk],
            alpha11boot[bootjunk], alpha12boot[bootjunk], alpha13boot[bootjunk], alpha14boot[bootjunk], alpha15boot[bootjunk], alpha16boot[bootjunk], alpha17boot[bootjunk], alpha18boot[bootjunk], 
            beta31boot[bootjunk], beta32boot[bootjunk], beta33boot[bootjunk])		          # Formula121: ksi1+q*alpha1-x*beta3  
  
  coef122=c(ksi2_boot[bootjunk],
            alpha21boot[bootjunk], alpha22boot[bootjunk], alpha23boot[bootjunk], alpha24boot[bootjunk], alpha25boot[bootjunk], alpha26boot[bootjunk], alpha27boot[bootjunk], alpha28boot[bootjunk],
            beta31boot[bootjunk], beta32boot[bootjunk], beta33boot[bootjunk])		          # Formula122: ksi2+q*alpha2-x*beta3
  
  
  #partial elasticities:
  
  # Age:
  
  M1or2_age_q25=cbind(one,minus_q,q,minus_z)
  M121_age_q25 =cbind(one,q,-Age_q25,minus_x[,2:3])
  M122_age_q25 =cbind(one,q,-Age_q25,minus_x[,2:3])
  
  
  V1_age_q25=(M1or2_age_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_age_q25= -V1_age_q25
  
  VV1_age_q25=M121_age_q25%*%coef121
  VV2_age_q25=M122_age_q25%*%coef122
  
  V1_age_q25=exp(V1_age_q25)
  V2_age_q25=exp(V2_age_q25)
  VV1_age_q25=exp(VV1_age_q25)
  VV2_age_q25=exp(VV2_age_q25)
  
  
  V3_age_q25=(VV1_age_q25+VV2_age_q25)**(-c_phiboot[bootjunk])
  V4_age_q25=(VV1_age_q25+VV2_age_q25)**(c_phiboot[bootjunk])
  
  
  
  p_age_imp_q25=(one/(one+V1_age_q25))*(one/(one+V3_age_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_age_nby_q25=(one/(one+V2_age_q25))*(one/(one+V3_age_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_age_pln_q25= one/(one+V4_age_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_age_q75=cbind(one,minus_q,q,minus_z)
  M121_age_q75 =cbind(one,q,-Age_q75,minus_x[,2:3])
  M122_age_q75 =cbind(one,q,-Age_q75,minus_x[,2:3])
  
  
  V1_age_q75=(M1or2_age_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_age_q75= -V1_age_q75
  
  VV1_age_q75=M121_age_q75%*%coef121
  VV2_age_q75=M122_age_q75%*%coef122
  
  V1_age_q75=exp(V1_age_q75)
  V2_age_q75=exp(V2_age_q75)
  VV1_age_q75=exp(VV1_age_q75)
  VV2_age_q75=exp(VV2_age_q75)
  
  
  V3_age_q75=(VV1_age_q75+VV2_age_q75)**(-c_phiboot[bootjunk])
  V4_age_q75=(VV1_age_q75+VV2_age_q75)**(c_phiboot[bootjunk])
  
  
  p_age_imp_q75=(one/(one+V1_age_q75))*(one/(one+V3_age_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_age_nby_q75=(one/(one+V2_age_q75))*(one/(one+V3_age_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_age_pln_q75= one/(one+V4_age_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_age_impulse = ((p_age_imp_q75-p_age_imp_q25)/p_age_imp_q25)*100
  PE_age_nobuy = ((p_age_nby_q75-p_age_nby_q25)/p_age_nby_q25)*100
  PE_age_plan = ((p_age_pln_q75-p_age_pln_q25)/p_age_pln_q25)*100
  
  
  E_Age_impulse[bootjunk] <- mean(PE_age_impulse)
  E_Age_nobuy[bootjunk] <- mean(PE_age_nobuy)
  E_Age_plan[bootjunk] <- mean(PE_age_plan)
  
  
  
  #Household size:
  
  
  M1or2_household_size_q25=cbind(one,minus_q,q,minus_z)
  M121_household_size_q25 =cbind(one,q,minus_x[,1],-household_size_q25,minus_x[,3])
  M122_household_size_q25 =cbind(one,q,minus_x[,1],-household_size_q25,minus_x[,3])
  
  
  V1_household_size_q25=(M1or2_household_size_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_household_size_q25= -V1_household_size_q25
  
  VV1_household_size_q25=M121_household_size_q25%*%coef121
  VV2_household_size_q25=M122_household_size_q25%*%coef122
  
  V1_household_size_q25=exp(V1_household_size_q25)
  V2_household_size_q25=exp(V2_household_size_q25)
  VV1_household_size_q25=exp(VV1_household_size_q25)
  VV2_household_size_q25=exp(VV2_household_size_q25)
  
  
  V3_household_size_q25=(VV1_household_size_q25+VV2_household_size_q25)**(-c_phiboot[bootjunk])
  V4_household_size_q25=(VV1_household_size_q25+VV2_household_size_q25)**(c_phiboot[bootjunk])
  
  
  
  p_household_size_imp_q25=(one/(one+V1_household_size_q25))*(one/(one+V3_household_size_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_household_size_nby_q25=(one/(one+V2_household_size_q25))*(one/(one+V3_household_size_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_household_size_pln_q25= one/(one+V4_household_size_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_household_size_q75=cbind(one,minus_q,q,minus_z)
  M121_household_size_q75 =cbind(one,q,minus_x[1],-household_size_q75,minus_x[,3])
  M122_household_size_q75 =cbind(one,q,minus_x[1],-household_size_q75,minus_x[,3])
  
  
  V1_household_size_q75=(M1or2_household_size_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_household_size_q75= -V1_household_size_q75
  
  VV1_household_size_q75=M121_household_size_q75%*%coef121
  VV2_household_size_q75=M122_household_size_q75%*%coef122
  
  V1_household_size_q75=exp(V1_household_size_q75)
  V2_household_size_q75=exp(V2_household_size_q75)
  VV1_household_size_q75=exp(VV1_household_size_q75)
  VV2_household_size_q75=exp(VV2_household_size_q75)
  
  
  V3_household_size_q75=(VV1_household_size_q75+VV2_household_size_q75)**(-c_phiboot[bootjunk])
  V4_household_size_q75=(VV1_household_size_q75+VV2_household_size_q75)**(c_phiboot[bootjunk])
  
  
  p_household_size_imp_q75=(one/(one+V1_household_size_q75))*(one/(one+V3_household_size_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_household_size_nby_q75=(one/(one+V2_household_size_q75))*(one/(one+V3_household_size_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_household_size_pln_q75= one/(one+V4_household_size_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_household_size_impulse = ((p_household_size_imp_q75-p_household_size_imp_q25)/p_household_size_imp_q25)*100
  PE_household_size_nobuy = ((p_household_size_nby_q75-p_household_size_nby_q25)/p_household_size_nby_q25)*100
  PE_household_size_plan = ((p_household_size_pln_q75-p_household_size_pln_q25)/p_household_size_pln_q25)*100
  
  
  E_household_size_impulse[bootjunk] <- mean(PE_household_size_impulse)
  E_household_size_nobuy[bootjunk] <- mean(PE_household_size_nobuy)
  E_household_size_plan[bootjunk] <- mean(PE_household_size_plan)
  
  #preplan shopping
  
  
  M1or2_Preplan_shopping_q25=cbind(one,minus_q,q,minus_z)
  M121_Preplan_shopping_q25 =cbind(one,q,minus_x[,1:2],-Preplan_shopping_q25)
  M122_Preplan_shopping_q25 =cbind(one,q,minus_x[,1:2],-Preplan_shopping_q25)
  
  
  V1_Preplan_shopping_q25=(M1or2_Preplan_shopping_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_Preplan_shopping_q25= -V1_Preplan_shopping_q25
  
  VV1_Preplan_shopping_q25=M121_Preplan_shopping_q25%*%coef121
  VV2_Preplan_shopping_q25=M122_Preplan_shopping_q25%*%coef122
  
  V1_Preplan_shopping_q25=exp(V1_Preplan_shopping_q25)
  V2_Preplan_shopping_q25=exp(V2_Preplan_shopping_q25)
  VV1_Preplan_shopping_q25=exp(VV1_Preplan_shopping_q25)
  VV2_Preplan_shopping_q25=exp(VV2_Preplan_shopping_q25)
  
  
  V3_Preplan_shopping_q25=(VV1_Preplan_shopping_q25+VV2_Preplan_shopping_q25)**(-c_phiboot[bootjunk])
  V4_Preplan_shopping_q25=(VV1_Preplan_shopping_q25+VV2_Preplan_shopping_q25)**(c_phiboot[bootjunk])
  
  
  
  p_Preplan_shopping_imp_q25=(one/(one+V1_Preplan_shopping_q25))*(one/(one+V3_Preplan_shopping_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Preplan_shopping_nby_q25=(one/(one+V2_Preplan_shopping_q25))*(one/(one+V3_Preplan_shopping_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Preplan_shopping_pln_q25= one/(one+V4_Preplan_shopping_q25) 			                              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_Preplan_shopping_q75=cbind(one,minus_q,q,minus_z)
  M121_Preplan_shopping_q75 =cbind(one,q,minus_x[,1:2], -Preplan_shopping_q75)
  M122_Preplan_shopping_q75 =cbind(one,q,minus_x[,1:2], -Preplan_shopping_q75)
  
  
  V1_Preplan_shopping_q75=(M1or2_Preplan_shopping_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_Preplan_shopping_q75= -V1_Preplan_shopping_q75
  
  VV1_Preplan_shopping_q75=M121_Preplan_shopping_q75%*%coef121
  VV2_Preplan_shopping_q75=M122_Preplan_shopping_q75%*%coef122
  
  V1_Preplan_shopping_q75=exp(V1_Preplan_shopping_q75)
  V2_Preplan_shopping_q75=exp(V2_Preplan_shopping_q75)
  VV1_Preplan_shopping_q75=exp(VV1_Preplan_shopping_q75)
  VV2_Preplan_shopping_q75=exp(VV2_Preplan_shopping_q75)
  
  
  V3_Preplan_shopping_q75=(VV1_Preplan_shopping_q75+VV2_Preplan_shopping_q75)**(-c_phiboot[bootjunk])
  V4_Preplan_shopping_q75=(VV1_Preplan_shopping_q75+VV2_Preplan_shopping_q75)**(c_phiboot[bootjunk])
  
  
  p_Preplan_shopping_imp_q75=(one/(one+V1_Preplan_shopping_q75))*(one/(one+V3_Preplan_shopping_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Preplan_shopping_nby_q75=(one/(one+V2_Preplan_shopping_q75))*(one/(one+V3_Preplan_shopping_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_Preplan_shopping_pln_q75= one/(one+V4_Preplan_shopping_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Preplan_shopping_impulse = ((p_Preplan_shopping_imp_q75-p_Preplan_shopping_imp_q25)/p_Preplan_shopping_imp_q25)*100
  PE_Preplan_shopping_nobuy = ((p_Preplan_shopping_nby_q75-p_Preplan_shopping_nby_q25)/p_Preplan_shopping_nby_q25)*100
  PE_Preplan_shopping_plan = ((p_Preplan_shopping_pln_q75-p_Preplan_shopping_pln_q25)/p_Preplan_shopping_pln_q25)*100
  
  
  E_Preplan_shopping_impulse[bootjunk] <- mean(PE_Preplan_shopping_impulse)
  E_Preplan_shopping_nobuy[bootjunk] <- mean(PE_Preplan_shopping_nobuy)
  E_Preplan_shopping_plan[bootjunk] <- mean(PE_Preplan_shopping_plan)
  
  
  #sex
  
  M1or2_Sex_0=cbind(one,-Sex_0,minus_q[,2:8],Sex_0,q[,2:8],minus_z)
  M121_Sex_0 =cbind(one,Sex_0,q[,2:8],minus_x)
  M122_Sex_0 =cbind(one,Sex_0,q[,2:8],minus_x)
  
  
  V1_Sex_0=(M1or2_Sex_0%*%coef1or2)*omegaboot[bootjunk]
  V2_Sex_0= -V1_Sex_0
  
  VV1_Sex_0=M121_Sex_0%*%coef121
  VV2_Sex_0=M122_Sex_0%*%coef122
  
  V1_Sex_0=exp(V1_Sex_0)
  V2_Sex_0=exp(V2_Sex_0)
  VV1_Sex_0=exp(VV1_Sex_0)
  VV2_Sex_0=exp(VV2_Sex_0)
  
  
  V3_Sex_0=(VV1_Sex_0+VV2_Sex_0)**(-c_phiboot[bootjunk])
  V4_Sex_0=(VV1_Sex_0+VV2_Sex_0)**(c_phiboot[bootjunk])
  
  
  
  p_Sex_imp_0=(one/(one+V1_Sex_0))*(one/(one+V3_Sex_0))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Sex_nby_0=(one/(one+V2_Sex_0))*(one/(one+V3_Sex_0))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Sex_pln_0= one/(one+V4_Sex_0) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_Sex_1=cbind(one,-Sex_1,minus_q[,2:8],Sex_1,q[,2:8],minus_z)
  M121_Sex_1 =cbind(one,Sex_1,q[,2:8],minus_x)
  M122_Sex_1 =cbind(one,Sex_1,q[,2:8],minus_x)
  
  
  V1_Sex_1=(M1or2_Sex_1%*%coef1or2)*omegaboot[bootjunk]
  V2_Sex_1= -V1_Sex_1
  
  VV1_Sex_1=M121_Sex_1%*%coef121
  VV2_Sex_1=M122_Sex_1%*%coef122
  
  V1_Sex_1=exp(V1_Sex_1)
  V2_Sex_1=exp(V2_Sex_1)
  VV1_Sex_1=exp(VV1_Sex_1)
  VV2_Sex_1=exp(VV2_Sex_1)
  
  
  V3_Sex_1=(VV1_Sex_1+VV2_Sex_1)**(-c_phiboot[bootjunk])
  V4_Sex_1=(VV1_Sex_1+VV2_Sex_1)**(c_phiboot[bootjunk])
  
  
  
  p_Sex_imp_1=(one/(one+V1_Sex_1))*(one/(one+V3_Sex_1))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Sex_nby_1=(one/(one+V2_Sex_1))*(one/(one+V3_Sex_1))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Sex_pln_1= one/(one+V4_Sex_1) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Sex_impulse = ((p_Sex_imp_1-p_Sex_imp_0)/p_Sex_imp_0)*100
  PE_Sex_nobuy = ((p_Sex_nby_1-p_Sex_nby_0)/p_Sex_nby_0)*100
  PE_Sex_plan = ((p_Sex_pln_1-p_Sex_pln_0)/p_Sex_pln_0)*100
  
  
  E_Sex_impulse[bootjunk] <- mean(PE_Sex_impulse)
  E_Sex_nobuy[bootjunk] <- mean(PE_Sex_nobuy)
  E_Sex_plan[bootjunk] <- mean(PE_Sex_plan)
  
  
  
  # Trait_impulsiveness:
  
  M1or2_Trait_impulsiveness_q25=cbind(one,minus_q[,1],-Trait_impulsiveness_q25,minus_q[,3:8],q[,1],Trait_impulsiveness_q25,q[,3:8],minus_z)
  M121_Trait_impulsiveness_q25 =cbind(one,q[,1],Trait_impulsiveness_q25,q[,3:8],minus_x)
  M122_Trait_impulsiveness_q25 =cbind(one,q[,1],Trait_impulsiveness_q25,q[,3:8],minus_x)
  
  
  V1_Trait_impulsiveness_q25=(M1or2_Trait_impulsiveness_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_Trait_impulsiveness_q25= -V1_Trait_impulsiveness_q25
  
  VV1_Trait_impulsiveness_q25=M121_Trait_impulsiveness_q25%*%coef121
  VV2_Trait_impulsiveness_q25=M122_Trait_impulsiveness_q25%*%coef122
  
  V1_Trait_impulsiveness_q25=exp(V1_Trait_impulsiveness_q25)
  V2_Trait_impulsiveness_q25=exp(V2_Trait_impulsiveness_q25)
  VV1_Trait_impulsiveness_q25=exp(VV1_Trait_impulsiveness_q25)
  VV2_Trait_impulsiveness_q25=exp(VV2_Trait_impulsiveness_q25)
  
  
  V3_Trait_impulsiveness_q25=(VV1_Trait_impulsiveness_q25+VV2_Trait_impulsiveness_q25)**(-c_phiboot[bootjunk])
  V4_Trait_impulsiveness_q25=(VV1_Trait_impulsiveness_q25+VV2_Trait_impulsiveness_q25)**(c_phiboot[bootjunk])
  
  
  
  p_Trait_impulsiveness_imp_q25=(one/(one+V1_Trait_impulsiveness_q25))*(one/(one+V3_Trait_impulsiveness_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Trait_impulsiveness_nby_q25=(one/(one+V2_Trait_impulsiveness_q25))*(one/(one+V3_Trait_impulsiveness_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Trait_impulsiveness_pln_q25= one/(one+V4_Trait_impulsiveness_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_Trait_impulsiveness_q75=cbind(one,minus_q[,1],-Trait_impulsiveness_q75,minus_q[,3:8],q[,1],Trait_impulsiveness_q75,q[,3:8],minus_z)
  M121_Trait_impulsiveness_q75 =cbind(one,q[,1],Trait_impulsiveness_q75,q[,3:8],minus_x)
  M122_Trait_impulsiveness_q75 =cbind(one,q[,1],Trait_impulsiveness_q75,q[,3:8],minus_x)
  
  
  V1_Trait_impulsiveness_q75=(M1or2_Trait_impulsiveness_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_Trait_impulsiveness_q75= -V1_Trait_impulsiveness_q75
  
  VV1_Trait_impulsiveness_q75=M121_Trait_impulsiveness_q75%*%coef121
  VV2_Trait_impulsiveness_q75=M122_Trait_impulsiveness_q75%*%coef122
  
  V1_Trait_impulsiveness_q75=exp(V1_Trait_impulsiveness_q75)
  V2_Trait_impulsiveness_q75=exp(V2_Trait_impulsiveness_q75)
  VV1_Trait_impulsiveness_q75=exp(VV1_Trait_impulsiveness_q75)
  VV2_Trait_impulsiveness_q75=exp(VV2_Trait_impulsiveness_q75)
  
  
  V3_Trait_impulsiveness_q75=(VV1_Trait_impulsiveness_q75+VV2_Trait_impulsiveness_q75)**(-c_phiboot[bootjunk])
  V4_Trait_impulsiveness_q75=(VV1_Trait_impulsiveness_q75+VV2_Trait_impulsiveness_q75)**(c_phiboot[bootjunk])
  
  
  p_Trait_impulsiveness_imp_q75=(one/(one+V1_Trait_impulsiveness_q75))*(one/(one+V3_Trait_impulsiveness_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Trait_impulsiveness_nby_q75=(one/(one+V2_Trait_impulsiveness_q75))*(one/(one+V3_Trait_impulsiveness_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_Trait_impulsiveness_pln_q75= one/(one+V4_Trait_impulsiveness_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Trait_impulsiveness_impulse = ((p_Trait_impulsiveness_imp_q75-p_Trait_impulsiveness_imp_q25)/p_Trait_impulsiveness_imp_q25)*100
  PE_Trait_impulsiveness_nobuy = ((p_Trait_impulsiveness_nby_q75-p_Trait_impulsiveness_nby_q25)/p_Trait_impulsiveness_nby_q25)*100
  PE_Trait_impulsiveness_plan = ((p_Trait_impulsiveness_pln_q75-p_Trait_impulsiveness_pln_q25)/p_Trait_impulsiveness_pln_q25)*100
  
  
  E_Trait_impulsiveness_impulse[bootjunk] <- mean(PE_Trait_impulsiveness_impulse)
  E_Trait_impulsiveness_nobuy[bootjunk] <- mean(PE_Trait_impulsiveness_nobuy)
  E_Trait_impulsiveness_plan[bootjunk] <- mean(PE_Trait_impulsiveness_plan)
  
  
  # Hedonic
  
  M1or2_Hedonic_0 =cbind(one,minus_q[,1:2],-Hedonic_0,minus_q[,4:8],q[,1:2],Hedonic_0,q[,4:8],minus_z)
  M121_Hedonic_0 =cbind(one,q[,1:2],Hedonic_0,q[,4:8],minus_x)
  M122_Hedonic_0 =cbind(one,q[,1:2],Hedonic_0,q[,4:8],minus_x)
  
  
  
  V1_Hedonic_0=(M1or2_Hedonic_0%*%coef1or2)*omegaboot[bootjunk]
  V2_Hedonic_0= -V1_Hedonic_0
  
  VV1_Hedonic_0=M121_Hedonic_0%*%coef121
  VV2_Hedonic_0=M122_Hedonic_0%*%coef122
  
  V1_Hedonic_0=exp(V1_Hedonic_0)
  V2_Hedonic_0=exp(V2_Hedonic_0)
  VV1_Hedonic_0=exp(VV1_Hedonic_0)
  VV2_Hedonic_0=exp(VV2_Hedonic_0)
  
  
  V3_Hedonic_0=(VV1_Hedonic_0+VV2_Hedonic_0)**(-c_phiboot[bootjunk])
  V4_Hedonic_0=(VV1_Hedonic_0+VV2_Hedonic_0)**(c_phiboot[bootjunk])
  
  
  
  p_Hedonic_imp_0=(one/(one+V1_Hedonic_0))*(one/(one+V3_Hedonic_0))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Hedonic_nby_0=(one/(one+V2_Hedonic_0))*(one/(one+V3_Hedonic_0))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Hedonic_pln_0= one/(one+V4_Hedonic_0) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  
  M1or2_Hedonic_1 =cbind(one,minus_q[,1:2],-Hedonic_1,minus_q[,4:8],q[,1:2],Hedonic_1,q[,4:8],minus_z)
  M121_Hedonic_1 =cbind(one,q[,1:2],Hedonic_1,q[,4:8],minus_x)
  M122_Hedonic_1 =cbind(one,q[,1:2],Hedonic_1,q[,4:8],minus_x)
  
  
  V1_Hedonic_1=(M1or2_Hedonic_1%*%coef1or2)*omegaboot[bootjunk]
  V2_Hedonic_1= -V1_Hedonic_1
  
  VV1_Hedonic_1=M121_Hedonic_1%*%coef121
  VV2_Hedonic_1=M122_Hedonic_1%*%coef122
  
  V1_Hedonic_1=exp(V1_Hedonic_1)
  V2_Hedonic_1=exp(V2_Hedonic_1)
  VV1_Hedonic_1=exp(VV1_Hedonic_1)
  VV2_Hedonic_1=exp(VV2_Hedonic_1)
  
  
  V3_Hedonic_1=(VV1_Hedonic_1+VV2_Hedonic_1)**(-c_phiboot[bootjunk])
  V4_Hedonic_1=(VV1_Hedonic_1+VV2_Hedonic_1)**(c_phiboot[bootjunk])
  
  
  
  p_Hedonic_imp_1=(one/(one+V1_Hedonic_1))*(one/(one+V3_Hedonic_1))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Hedonic_nby_1=(one/(one+V2_Hedonic_1))*(one/(one+V3_Hedonic_1))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Hedonic_pln_1= one/(one+V4_Hedonic_1) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Hedonic_impulse = ((p_Hedonic_imp_1-p_Hedonic_imp_0)/p_Hedonic_imp_0)*100
  PE_Hedonic_nobuy = ((p_Hedonic_nby_1-p_Hedonic_nby_0)/p_Hedonic_nby_0)*100
  PE_Hedonic_plan = ((p_Hedonic_pln_1-p_Hedonic_pln_0)/p_Hedonic_pln_0)*100
  
  
  E_Hedonic_impulse[bootjunk] <- mean(PE_Hedonic_impulse)
  E_Hedonic_nobuy[bootjunk] <- mean(PE_Hedonic_nobuy)
  E_Hedonic_plan[bootjunk] <- mean(PE_Hedonic_plan)
  
  
  # ready to use
  
  
  M1or2_Ready_To_Use_0 =cbind(one,minus_q[,1:3],-Ready_To_Use_0,minus_q[,5:8],q[,1:3],Ready_To_Use_0,q[,5:8],minus_z)
  M121_Ready_To_Use_0 =cbind(one,q[,1:3],Ready_To_Use_0,q[,5:8],minus_x)
  M122_Ready_To_Use_0 =cbind(one,q[,1:3],Ready_To_Use_0,q[,5:8],minus_x)
  
  
  
  V1_Ready_To_Use_0=(M1or2_Ready_To_Use_0%*%coef1or2)*omegaboot[bootjunk]
  V2_Ready_To_Use_0= -V1_Ready_To_Use_0
  
  VV1_Ready_To_Use_0=M121_Ready_To_Use_0%*%coef121
  VV2_Ready_To_Use_0=M122_Ready_To_Use_0%*%coef122
  
  V1_Ready_To_Use_0=exp(V1_Ready_To_Use_0)
  V2_Ready_To_Use_0=exp(V2_Ready_To_Use_0)
  VV1_Ready_To_Use_0=exp(VV1_Ready_To_Use_0)
  VV2_Ready_To_Use_0=exp(VV2_Ready_To_Use_0)
  
  
  V3_Ready_To_Use_0=(VV1_Ready_To_Use_0+VV2_Ready_To_Use_0)**(-c_phiboot[bootjunk])
  V4_Ready_To_Use_0=(VV1_Ready_To_Use_0+VV2_Ready_To_Use_0)**(c_phiboot[bootjunk])
  
  
  
  p_Ready_To_Use_imp_0=(one/(one+V1_Ready_To_Use_0))*(one/(one+V3_Ready_To_Use_0))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Ready_To_Use_nby_0=(one/(one+V2_Ready_To_Use_0))*(one/(one+V3_Ready_To_Use_0))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Ready_To_Use_pln_0= one/(one+V4_Ready_To_Use_0) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  
  M1or2_Ready_To_Use_1 =cbind(one,minus_q[,1:3],-Ready_To_Use_1,minus_q[,5:8],q[,1:3],Ready_To_Use_1,q[,5:8],minus_z)
  M121_Ready_To_Use_1 =cbind(one,q[,1:3],Ready_To_Use_1,q[,5:8],minus_x)
  M122_Ready_To_Use_1 =cbind(one,q[,1:3],Ready_To_Use_1,q[,5:8],minus_x)
  
  
  V1_Ready_To_Use_1=(M1or2_Ready_To_Use_1%*%coef1or2)*omegaboot[bootjunk]
  V2_Ready_To_Use_1= -V1_Ready_To_Use_1
  
  VV1_Ready_To_Use_1=M121_Ready_To_Use_1%*%coef121
  VV2_Ready_To_Use_1=M122_Ready_To_Use_1%*%coef122
  
  V1_Ready_To_Use_1=exp(V1_Ready_To_Use_1)
  V2_Ready_To_Use_1=exp(V2_Ready_To_Use_1)
  VV1_Ready_To_Use_1=exp(VV1_Ready_To_Use_1)
  VV2_Ready_To_Use_1=exp(VV2_Ready_To_Use_1)
  
  
  V3_Ready_To_Use_1=(VV1_Ready_To_Use_1+VV2_Ready_To_Use_1)**(-c_phiboot[bootjunk])
  V4_Ready_To_Use_1=(VV1_Ready_To_Use_1+VV2_Ready_To_Use_1)**(c_phiboot[bootjunk])
  
  
  
  p_Ready_To_Use_imp_1=(one/(one+V1_Ready_To_Use_1))*(one/(one+V3_Ready_To_Use_1))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Ready_To_Use_nby_1=(one/(one+V2_Ready_To_Use_1))*(one/(one+V3_Ready_To_Use_1))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Ready_To_Use_pln_1= one/(one+V4_Ready_To_Use_1) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Ready_To_Use_impulse = ((p_Ready_To_Use_imp_1-p_Ready_To_Use_imp_0)/p_Ready_To_Use_imp_0)*100
  PE_Ready_To_Use_nobuy = ((p_Ready_To_Use_nby_1-p_Ready_To_Use_nby_0)/p_Ready_To_Use_nby_0)*100
  PE_Ready_To_Use_plan = ((p_Ready_To_Use_pln_1-p_Ready_To_Use_pln_0)/p_Ready_To_Use_pln_0)*100
  
  
  E_Ready_To_Use_impulse[bootjunk] <- mean(PE_Ready_To_Use_impulse)
  E_Ready_To_Use_nobuy[bootjunk] <- mean(PE_Ready_To_Use_nobuy)
  E_Ready_To_Use_plan[bootjunk] <- mean(PE_Ready_To_Use_plan)
  
  
  # sale proneness
  
  
  
  M1or2_Sale_proneness_q25=cbind(one,minus_q[,1:4],-Sale_proneness_q25,minus_q[,6:8],q[,1:4],Sale_proneness_q25,q[,6:8],minus_z)
  M121_Sale_proneness_q25 =cbind(one,q[,1:4],Sale_proneness_q25,q[,6:8],minus_x)
  M122_Sale_proneness_q25 =cbind(one,q[,1:4],Sale_proneness_q25,q[,6:8],minus_x)
  
  
  V1_Sale_proneness_q25=(M1or2_Sale_proneness_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_Sale_proneness_q25= -V1_Sale_proneness_q25
  
  VV1_Sale_proneness_q25=M121_Sale_proneness_q25%*%coef121
  VV2_Sale_proneness_q25=M122_Sale_proneness_q25%*%coef122
  
  V1_Sale_proneness_q25=exp(V1_Sale_proneness_q25)
  V2_Sale_proneness_q25=exp(V2_Sale_proneness_q25)
  VV1_Sale_proneness_q25=exp(VV1_Sale_proneness_q25)
  VV2_Sale_proneness_q25=exp(VV2_Sale_proneness_q25)
  
  
  V3_Sale_proneness_q25=(VV1_Sale_proneness_q25+VV2_Sale_proneness_q25)**(-c_phiboot[bootjunk])
  V4_Sale_proneness_q25=(VV1_Sale_proneness_q25+VV2_Sale_proneness_q25)**(c_phiboot[bootjunk])
  
  
  
  p_Sale_proneness_imp_q25=(one/(one+V1_Sale_proneness_q25))*(one/(one+V3_Sale_proneness_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Sale_proneness_nby_q25=(one/(one+V2_Sale_proneness_q25))*(one/(one+V3_Sale_proneness_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Sale_proneness_pln_q25= one/(one+V4_Sale_proneness_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_Sale_proneness_q75=cbind(one,minus_q[,1:4],-Sale_proneness_q75,minus_q[,6:8],q[,1:4],Sale_proneness_q75,q[,6:8],minus_z)
  M121_Sale_proneness_q75 =cbind(one,q[,1:4],Sale_proneness_q75,q[,6:8],minus_x)
  M122_Sale_proneness_q75 =cbind(one,q[,1:4],Sale_proneness_q75,q[,6:8],minus_x)
  
  
  V1_Sale_proneness_q75=(M1or2_Sale_proneness_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_Sale_proneness_q75= -V1_Sale_proneness_q75
  
  VV1_Sale_proneness_q75=M121_Sale_proneness_q75%*%coef121
  VV2_Sale_proneness_q75=M122_Sale_proneness_q75%*%coef122
  
  V1_Sale_proneness_q75=exp(V1_Sale_proneness_q75)
  V2_Sale_proneness_q75=exp(V2_Sale_proneness_q75)
  VV1_Sale_proneness_q75=exp(VV1_Sale_proneness_q75)
  VV2_Sale_proneness_q75=exp(VV2_Sale_proneness_q75)
  
  
  V3_Sale_proneness_q75=(VV1_Sale_proneness_q75+VV2_Sale_proneness_q75)**(-c_phiboot[bootjunk])
  V4_Sale_proneness_q75=(VV1_Sale_proneness_q75+VV2_Sale_proneness_q75)**(c_phiboot[bootjunk])
  
  
  p_Sale_proneness_imp_q75=(one/(one+V1_Sale_proneness_q75))*(one/(one+V3_Sale_proneness_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Sale_proneness_nby_q75=(one/(one+V2_Sale_proneness_q75))*(one/(one+V3_Sale_proneness_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_Sale_proneness_pln_q75= one/(one+V4_Sale_proneness_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Sale_proneness_impulse = ((p_Sale_proneness_imp_q75-p_Sale_proneness_imp_q25)/p_Sale_proneness_imp_q25)*100
  PE_Sale_proneness_nobuy = ((p_Sale_proneness_nby_q75-p_Sale_proneness_nby_q25)/p_Sale_proneness_nby_q25)*100
  PE_Sale_proneness_plan = ((p_Sale_proneness_pln_q75-p_Sale_proneness_pln_q25)/p_Sale_proneness_pln_q25)*100
  
  
  E_Sale_proneness_impulse[bootjunk] <- mean(PE_Sale_proneness_impulse)
  E_Sale_proneness_nobuy[bootjunk] <- mean(PE_Sale_proneness_nobuy)
  E_Sale_proneness_plan[bootjunk] <- mean(PE_Sale_proneness_plan)
  
  
  # Unhealthiness_Index
  
  
  M1or2_Unhealthiness_Index_q25=cbind(one,minus_q[,1:5],-Unhealthiness_Index_q25,minus_q[,7:8],q[,1:5],Unhealthiness_Index_q25,q[,7:8],minus_z)
  M121_Unhealthiness_Index_q25 =cbind(one,q[,1:5],Unhealthiness_Index_q25,q[,7:8],minus_x)
  M122_Unhealthiness_Index_q25 =cbind(one,q[,1:5],Unhealthiness_Index_q25,q[,7:8],minus_x)
  
  
  V1_Unhealthiness_Index_q25=(M1or2_Unhealthiness_Index_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_Unhealthiness_Index_q25= -V1_Unhealthiness_Index_q25
  
  VV1_Unhealthiness_Index_q25=M121_Unhealthiness_Index_q25%*%coef121
  VV2_Unhealthiness_Index_q25=M122_Unhealthiness_Index_q25%*%coef122
  
  V1_Unhealthiness_Index_q25=exp(V1_Unhealthiness_Index_q25)
  V2_Unhealthiness_Index_q25=exp(V2_Unhealthiness_Index_q25)
  VV1_Unhealthiness_Index_q25=exp(VV1_Unhealthiness_Index_q25)
  VV2_Unhealthiness_Index_q25=exp(VV2_Unhealthiness_Index_q25)
  
  
  V3_Unhealthiness_Index_q25=(VV1_Unhealthiness_Index_q25+VV2_Unhealthiness_Index_q25)**(-c_phiboot[bootjunk])
  V4_Unhealthiness_Index_q25=(VV1_Unhealthiness_Index_q25+VV2_Unhealthiness_Index_q25)**(c_phiboot[bootjunk])
  
  
  
  p_Unhealthiness_Index_imp_q25=(one/(one+V1_Unhealthiness_Index_q25))*(one/(one+V3_Unhealthiness_Index_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Unhealthiness_Index_nby_q25=(one/(one+V2_Unhealthiness_Index_q25))*(one/(one+V3_Unhealthiness_Index_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Unhealthiness_Index_pln_q25= one/(one+V4_Unhealthiness_Index_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_Unhealthiness_Index_q75=cbind(one,minus_q[,1:5],-Unhealthiness_Index_q75,minus_q[,7:8],q[,1:5],Unhealthiness_Index_q75,q[,7:8],minus_z)
  M121_Unhealthiness_Index_q75 =cbind(one,q[,1:5],Unhealthiness_Index_q75,q[,7:8],minus_x)
  M122_Unhealthiness_Index_q75 =cbind(one,q[,1:5],Unhealthiness_Index_q75,q[,7:8],minus_x)
  
  
  V1_Unhealthiness_Index_q75=(M1or2_Unhealthiness_Index_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_Unhealthiness_Index_q75= -V1_Unhealthiness_Index_q75
  
  VV1_Unhealthiness_Index_q75=M121_Unhealthiness_Index_q75%*%coef121
  VV2_Unhealthiness_Index_q75=M122_Unhealthiness_Index_q75%*%coef122
  
  V1_Unhealthiness_Index_q75=exp(V1_Unhealthiness_Index_q75)
  V2_Unhealthiness_Index_q75=exp(V2_Unhealthiness_Index_q75)
  VV1_Unhealthiness_Index_q75=exp(VV1_Unhealthiness_Index_q75)
  VV2_Unhealthiness_Index_q75=exp(VV2_Unhealthiness_Index_q75)
  
  
  V3_Unhealthiness_Index_q75=(VV1_Unhealthiness_Index_q75+VV2_Unhealthiness_Index_q75)**(-c_phiboot[bootjunk])
  V4_Unhealthiness_Index_q75=(VV1_Unhealthiness_Index_q75+VV2_Unhealthiness_Index_q75)**(c_phiboot[bootjunk])
  
  
  p_Unhealthiness_Index_imp_q75=(one/(one+V1_Unhealthiness_Index_q75))*(one/(one+V3_Unhealthiness_Index_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Unhealthiness_Index_nby_q75=(one/(one+V2_Unhealthiness_Index_q75))*(one/(one+V3_Unhealthiness_Index_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_Unhealthiness_Index_pln_q75= one/(one+V4_Unhealthiness_Index_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Unhealthiness_Index_impulse = ((p_Unhealthiness_Index_imp_q75-p_Unhealthiness_Index_imp_q25)/p_Unhealthiness_Index_imp_q25)*100
  PE_Unhealthiness_Index_nobuy = ((p_Unhealthiness_Index_nby_q75-p_Unhealthiness_Index_nby_q25)/p_Unhealthiness_Index_nby_q25)*100
  PE_Unhealthiness_Index_plan = ((p_Unhealthiness_Index_pln_q75-p_Unhealthiness_Index_pln_q25)/p_Unhealthiness_Index_pln_q25)*100
  
  
  E_Unhealthiness_Index_impulse[bootjunk] <- mean(PE_Unhealthiness_Index_impulse)
  E_Unhealthiness_Index_nobuy[bootjunk] <- mean(PE_Unhealthiness_Index_nobuy)
  E_Unhealthiness_Index_plan[bootjunk] <- mean(PE_Unhealthiness_Index_plan)
  
  
  # Value_consciousness
  
  M1or2_Value_consciousness_q25=cbind(one,minus_q[,1:6],-Value_consciousness_q25,minus_q[,8],q[,1:6],Value_consciousness_q25,q[,8],minus_z)
  M121_Value_consciousness_q25 =cbind(one,q[,1:6],Value_consciousness_q25,q[,8],minus_x)
  M122_Value_consciousness_q25 =cbind(one,q[,1:6],Value_consciousness_q25,q[,8],minus_x)
  
  
  V1_Value_consciousness_q25=(M1or2_Value_consciousness_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_Value_consciousness_q25= -V1_Value_consciousness_q25
  
  VV1_Value_consciousness_q25=M121_Value_consciousness_q25%*%coef121
  VV2_Value_consciousness_q25=M122_Value_consciousness_q25%*%coef122
  
  V1_Value_consciousness_q25=exp(V1_Value_consciousness_q25)
  V2_Value_consciousness_q25=exp(V2_Value_consciousness_q25)
  VV1_Value_consciousness_q25=exp(VV1_Value_consciousness_q25)
  VV2_Value_consciousness_q25=exp(VV2_Value_consciousness_q25)
  
  
  V3_Value_consciousness_q25=(VV1_Value_consciousness_q25+VV2_Value_consciousness_q25)**(-c_phiboot[bootjunk])
  V4_Value_consciousness_q25=(VV1_Value_consciousness_q25+VV2_Value_consciousness_q25)**(c_phiboot[bootjunk])
  
  
  
  p_Value_consciousness_imp_q25=(one/(one+V1_Value_consciousness_q25))*(one/(one+V3_Value_consciousness_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Value_consciousness_nby_q25=(one/(one+V2_Value_consciousness_q25))*(one/(one+V3_Value_consciousness_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Value_consciousness_pln_q25= one/(one+V4_Value_consciousness_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_Value_consciousness_q75=cbind(one,minus_q[,1:6],-Value_consciousness_q75,minus_q[,8],q[,1:6],Value_consciousness_q75,q[,8],minus_z)
  M121_Value_consciousness_q75 =cbind(one,q[,1:6],Value_consciousness_q75,q[,8],minus_x)
  M122_Value_consciousness_q75 =cbind(one,q[,1:6],Value_consciousness_q75,q[,8],minus_x)
  
  
  V1_Value_consciousness_q75=(M1or2_Value_consciousness_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_Value_consciousness_q75= -V1_Value_consciousness_q75
  
  VV1_Value_consciousness_q75=M121_Value_consciousness_q75%*%coef121
  VV2_Value_consciousness_q75=M122_Value_consciousness_q75%*%coef122
  
  V1_Value_consciousness_q75=exp(V1_Value_consciousness_q75)
  V2_Value_consciousness_q75=exp(V2_Value_consciousness_q75)
  VV1_Value_consciousness_q75=exp(VV1_Value_consciousness_q75)
  VV2_Value_consciousness_q75=exp(VV2_Value_consciousness_q75)
  
  
  V3_Value_consciousness_q75=(VV1_Value_consciousness_q75+VV2_Value_consciousness_q75)**(-c_phiboot[bootjunk])
  V4_Value_consciousness_q75=(VV1_Value_consciousness_q75+VV2_Value_consciousness_q75)**(c_phiboot[bootjunk])
  
  
  p_Value_consciousness_imp_q75=(one/(one+V1_Value_consciousness_q75))*(one/(one+V3_Value_consciousness_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Value_consciousness_nby_q75=(one/(one+V2_Value_consciousness_q75))*(one/(one+V3_Value_consciousness_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_Value_consciousness_pln_q75= one/(one+V4_Value_consciousness_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Value_consciousness_impulse = ((p_Value_consciousness_imp_q75-p_Value_consciousness_imp_q25)/p_Value_consciousness_imp_q25)*100
  PE_Value_consciousness_nobuy = ((p_Value_consciousness_nby_q75-p_Value_consciousness_nby_q25)/p_Value_consciousness_nby_q25)*100
  PE_Value_consciousness_plan = ((p_Value_consciousness_pln_q75-p_Value_consciousness_pln_q25)/p_Value_consciousness_pln_q25)*100
  
  
  E_Value_consciousness_impulse[bootjunk] <- mean(PE_Value_consciousness_impulse)
  E_Value_consciousness_nobuy[bootjunk] <- mean(PE_Value_consciousness_nobuy)
  E_Value_consciousness_plan[bootjunk] <- mean(PE_Value_consciousness_plan)
  
  # Variety_seeking
  
  
  M1or2_Variety_seeking_q25=cbind(one,minus_q[,1:7],-Variety_seeking_q25,q[,1:7],Variety_seeking_q25,minus_z)
  M121_Variety_seeking_q25 =cbind(one,q[,1:7],Variety_seeking_q25,minus_x)
  M122_Variety_seeking_q25 =cbind(one,q[,1:7],Variety_seeking_q25,minus_x)
  
  
  V1_Variety_seeking_q25=(M1or2_Variety_seeking_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_Variety_seeking_q25= -V1_Variety_seeking_q25
  
  VV1_Variety_seeking_q25=M121_Variety_seeking_q25%*%coef121
  VV2_Variety_seeking_q25=M122_Variety_seeking_q25%*%coef122
  
  V1_Variety_seeking_q25=exp(V1_Variety_seeking_q25)
  V2_Variety_seeking_q25=exp(V2_Variety_seeking_q25)
  VV1_Variety_seeking_q25=exp(VV1_Variety_seeking_q25)
  VV2_Variety_seeking_q25=exp(VV2_Variety_seeking_q25)
  
  
  V3_Variety_seeking_q25=(VV1_Variety_seeking_q25+VV2_Variety_seeking_q25)**(-c_phiboot[bootjunk])
  V4_Variety_seeking_q25=(VV1_Variety_seeking_q25+VV2_Variety_seeking_q25)**(c_phiboot[bootjunk])
  
  
  
  p_Variety_seeking_imp_q25=(one/(one+V1_Variety_seeking_q25))*(one/(one+V3_Variety_seeking_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Variety_seeking_nby_q25=(one/(one+V2_Variety_seeking_q25))*(one/(one+V3_Variety_seeking_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Variety_seeking_pln_q25= one/(one+V4_Variety_seeking_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_Variety_seeking_q75=cbind(one,minus_q[,1:7],-Variety_seeking_q75,q[,1:7],Variety_seeking_q75,minus_z)
  M121_Variety_seeking_q75 =cbind(one,q[,1:7],Variety_seeking_q75,minus_x)
  M122_Variety_seeking_q75 =cbind(one,q[,1:7],Variety_seeking_q75,minus_x)
  
  
  V1_Variety_seeking_q75=(M1or2_Variety_seeking_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_Variety_seeking_q75= -V1_Variety_seeking_q75
  
  VV1_Variety_seeking_q75=M121_Variety_seeking_q75%*%coef121
  VV2_Variety_seeking_q75=M122_Variety_seeking_q75%*%coef122
  
  V1_Variety_seeking_q75=exp(V1_Variety_seeking_q75)
  V2_Variety_seeking_q75=exp(V2_Variety_seeking_q75)
  VV1_Variety_seeking_q75=exp(VV1_Variety_seeking_q75)
  VV2_Variety_seeking_q75=exp(VV2_Variety_seeking_q75)
  
  
  V3_Variety_seeking_q75=(VV1_Variety_seeking_q75+VV2_Variety_seeking_q75)**(-c_phiboot[bootjunk])
  V4_Variety_seeking_q75=(VV1_Variety_seeking_q75+VV2_Variety_seeking_q75)**(c_phiboot[bootjunk])
  
  
  p_Variety_seeking_imp_q75=(one/(one+V1_Variety_seeking_q75))*(one/(one+V3_Variety_seeking_q75))	  #probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Variety_seeking_nby_q75=(one/(one+V2_Variety_seeking_q75))*(one/(one+V3_Variety_seeking_q75))	  #probability of no buy, a vector in which each element is p2 in each choice situation
  p_Variety_seeking_pln_q75= one/(one+V4_Variety_seeking_q75) 			                  	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Variety_seeking_impulse = ((p_Variety_seeking_imp_q75-p_Variety_seeking_imp_q25)/p_Variety_seeking_imp_q25)*100
  PE_Variety_seeking_nobuy = ((p_Variety_seeking_nby_q75-p_Variety_seeking_nby_q25)/p_Variety_seeking_nby_q25)*100
  PE_Variety_seeking_plan = ((p_Variety_seeking_pln_q75-p_Variety_seeking_pln_q25)/p_Variety_seeking_pln_q25)*100
  
  
  E_Variety_seeking_impulse[bootjunk] <- mean(PE_Variety_seeking_impulse)
  E_Variety_seeking_nobuy[bootjunk] <- mean(PE_Variety_seeking_nobuy)
  E_Variety_seeking_plan[bootjunk] <- mean(PE_Variety_seeking_plan)
  
  # Sale
  
  
  
  M1or2_Sale_0 =cbind(one,minus_q,q,-Sale_0,minus_z[,2:4])
  M121_Sale_0 =cbind(one,q,minus_x)
  M122_Sale_0 =cbind(one,q,minus_x)
  
  
  
  V1_Sale_0=(M1or2_Sale_0%*%coef1or2)*omegaboot[bootjunk]
  V2_Sale_0= -V1_Sale_0
  
  VV1_Sale_0=M121_Sale_0%*%coef121
  VV2_Sale_0=M122_Sale_0%*%coef122
  
  V1_Sale_0=exp(V1_Sale_0)
  V2_Sale_0=exp(V2_Sale_0)
  VV1_Sale_0=exp(VV1_Sale_0)
  VV2_Sale_0=exp(VV2_Sale_0)
  
  
  V3_Sale_0=(VV1_Sale_0+VV2_Sale_0)**(-c_phiboot[bootjunk])
  V4_Sale_0=(VV1_Sale_0+VV2_Sale_0)**(c_phiboot[bootjunk])
  
  
  
  p_Sale_imp_0=(one/(one+V1_Sale_0))*(one/(one+V3_Sale_0))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Sale_nby_0=(one/(one+V2_Sale_0))*(one/(one+V3_Sale_0))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Sale_pln_0= one/(one+V4_Sale_0) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  
  M1or2_Sale_1 =cbind(one,minus_q,q,-Sale_1,minus_z[,2:4])
  M121_Sale_1 =cbind(one,q,minus_x)
  M122_Sale_1 =cbind(one,q,minus_x)
  
  
  V1_Sale_1=(M1or2_Sale_1%*%coef1or2)*omegaboot[bootjunk]
  V2_Sale_1= -V1_Sale_1
  
  VV1_Sale_1=M121_Sale_1%*%coef121
  VV2_Sale_1=M122_Sale_1%*%coef122
  
  V1_Sale_1=exp(V1_Sale_1)
  V2_Sale_1=exp(V2_Sale_1)
  VV1_Sale_1=exp(VV1_Sale_1)
  VV2_Sale_1=exp(VV2_Sale_1)
  
  
  V3_Sale_1=(VV1_Sale_1+VV2_Sale_1)**(-c_phiboot[bootjunk])
  V4_Sale_1=(VV1_Sale_1+VV2_Sale_1)**(c_phiboot[bootjunk])
  
  
  
  p_Sale_imp_1=(one/(one+V1_Sale_1))*(one/(one+V3_Sale_1))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Sale_nby_1=(one/(one+V2_Sale_1))*(one/(one+V3_Sale_1))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Sale_pln_1= one/(one+V4_Sale_1) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Sale_impulse = ((p_Sale_imp_1-p_Sale_imp_0)/p_Sale_imp_0)*100
  PE_Sale_nobuy = ((p_Sale_nby_1-p_Sale_nby_0)/p_Sale_nby_0)*100
  PE_Sale_plan = ((p_Sale_pln_1-p_Sale_pln_0)/p_Sale_pln_0)*100
  
  
  E_Sale_impulse[bootjunk] <- mean(PE_Sale_impulse)
  E_Sale_nobuy[bootjunk] <- mean(PE_Sale_nobuy)
  E_Sale_plan[bootjunk] <- mean(PE_Sale_plan)
  
  # Display
  
  
  M1or2_Display_0 =cbind(one,minus_q,q,minus_z[,1],-Display_0,minus_z[,3:4])
  M121_Display_0 =cbind(one,q,minus_x)
  M122_Display_0 =cbind(one,q,minus_x)
  
  
  
  V1_Display_0=(M1or2_Display_0%*%coef1or2)*omegaboot[bootjunk]
  V2_Display_0= -V1_Display_0
  
  VV1_Display_0=M121_Display_0%*%coef121
  VV2_Display_0=M122_Display_0%*%coef122
  
  V1_Display_0=exp(V1_Display_0)
  V2_Display_0=exp(V2_Display_0)
  VV1_Display_0=exp(VV1_Display_0)
  VV2_Display_0=exp(VV2_Display_0)
  
  
  V3_Display_0=(VV1_Display_0+VV2_Display_0)**(-c_phiboot[bootjunk])
  V4_Display_0=(VV1_Display_0+VV2_Display_0)**(c_phiboot[bootjunk])
  
  
  
  p_Display_imp_0=(one/(one+V1_Display_0))*(one/(one+V3_Display_0))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Display_nby_0=(one/(one+V2_Display_0))*(one/(one+V3_Display_0))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Display_pln_0= one/(one+V4_Display_0) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  
  M1or2_Display_1 =cbind(one,minus_q,q,minus_z[,1],-Display_1,minus_z[,3:4])
  M121_Display_1 =cbind(one,q,minus_x)
  M122_Display_1 =cbind(one,q,minus_x)
  
  
  V1_Display_1=(M1or2_Display_1%*%coef1or2)*omegaboot[bootjunk]
  V2_Display_1= -V1_Display_1
  
  VV1_Display_1=M121_Display_1%*%coef121
  VV2_Display_1=M122_Display_1%*%coef122
  
  V1_Display_1=exp(V1_Display_1)
  V2_Display_1=exp(V2_Display_1)
  VV1_Display_1=exp(VV1_Display_1)
  VV2_Display_1=exp(VV2_Display_1)
  
  
  V3_Display_1=(VV1_Display_1+VV2_Display_1)**(-c_phiboot[bootjunk])
  V4_Display_1=(VV1_Display_1+VV2_Display_1)**(c_phiboot[bootjunk])
  
  
  
  p_Display_imp_1=(one/(one+V1_Display_1))*(one/(one+V3_Display_1))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_Display_nby_1=(one/(one+V2_Display_1))*(one/(one+V3_Display_1))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_Display_pln_1= one/(one+V4_Display_1) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_Display_impulse = ((p_Display_imp_1-p_Display_imp_0)/p_Display_imp_0)*100
  PE_Display_nobuy = ((p_Display_nby_1-p_Display_nby_0)/p_Display_nby_0)*100
  PE_Display_plan = ((p_Display_pln_1-p_Display_pln_0)/p_Display_pln_0)*100
  
  
  E_Display_impulse[bootjunk] <- mean(PE_Display_impulse)
  E_Display_nobuy[bootjunk] <- mean(PE_Display_nobuy)
  E_Display_plan[bootjunk] <- mean(PE_Display_plan)
  
  
  # New Product
  
  
  M1or2_New_Product_0 =cbind(one,minus_q,q,minus_z[,1:2],-New_Product_0,minus_z[,4])
  M121_New_Product_0 =cbind(one,q,minus_x)
  M122_New_Product_0 =cbind(one,q,minus_x)
  
  
  
  V1_New_Product_0=(M1or2_New_Product_0%*%coef1or2)*omegaboot[bootjunk]
  V2_New_Product_0= -V1_New_Product_0
  
  VV1_New_Product_0=M121_New_Product_0%*%coef121
  VV2_New_Product_0=M122_New_Product_0%*%coef122
  
  V1_New_Product_0=exp(V1_New_Product_0)
  V2_New_Product_0=exp(V2_New_Product_0)
  VV1_New_Product_0=exp(VV1_New_Product_0)
  VV2_New_Product_0=exp(VV2_New_Product_0)
  
  
  V3_New_Product_0=(VV1_New_Product_0+VV2_New_Product_0)**(-c_phiboot[bootjunk])
  V4_New_Product_0=(VV1_New_Product_0+VV2_New_Product_0)**(c_phiboot[bootjunk])
  
  
  
  p_New_Product_imp_0=(one/(one+V1_New_Product_0))*(one/(one+V3_New_Product_0))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_New_Product_nby_0=(one/(one+V2_New_Product_0))*(one/(one+V3_New_Product_0))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_New_Product_pln_0= one/(one+V4_New_Product_0) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  
  M1or2_New_Product_1 =cbind(one,minus_q,q,minus_z[,1:2],-New_Product_1,minus_z[,4])
  M121_New_Product_1 =cbind(one,q,minus_x)
  M122_New_Product_1 =cbind(one,q,minus_x)
  
  
  V1_New_Product_1=(M1or2_New_Product_1%*%coef1or2)*omegaboot[bootjunk]
  V2_New_Product_1= -V1_New_Product_1
  
  VV1_New_Product_1=M121_New_Product_1%*%coef121
  VV2_New_Product_1=M122_New_Product_1%*%coef122
  
  V1_New_Product_1=exp(V1_New_Product_1)
  V2_New_Product_1=exp(V2_New_Product_1)
  VV1_New_Product_1=exp(VV1_New_Product_1)
  VV2_New_Product_1=exp(VV2_New_Product_1)
  
  
  V3_New_Product_1=(VV1_New_Product_1+VV2_New_Product_1)**(-c_phiboot[bootjunk])
  V4_New_Product_1=(VV1_New_Product_1+VV2_New_Product_1)**(c_phiboot[bootjunk])
  
  
  
  p_New_Product_imp_1=(one/(one+V1_New_Product_1))*(one/(one+V3_New_Product_1))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_New_Product_nby_1=(one/(one+V2_New_Product_1))*(one/(one+V3_New_Product_1))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_New_Product_pln_1= one/(one+V4_New_Product_1) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_New_Product_impulse = ((p_New_Product_imp_1-p_New_Product_imp_0)/p_New_Product_imp_0)*100
  PE_New_Product_nobuy = ((p_New_Product_nby_1-p_New_Product_nby_0)/p_New_Product_nby_0)*100
  PE_New_Product_plan = ((p_New_Product_pln_1-p_New_Product_pln_0)/p_New_Product_pln_0)*100
  
  
  E_New_Product_impulse[bootjunk] <- mean(PE_New_Product_impulse)
  E_New_Product_nobuy[bootjunk] <- mean(PE_New_Product_nobuy)
  E_New_Product_plan[bootjunk] <- mean(PE_New_Product_plan)
  
  
  # Price Difference
  
  
  
  M1or2_price_difference_ratio_q25 =cbind(one,minus_q,q,minus_z[,1:3],-price_difference_ratio_q25)
  M121_price_difference_ratio_q25 =cbind(one,q,minus_x)
  M122_price_difference_ratio_q25 =cbind(one,q,minus_x)
  
  
  
  V1_price_difference_ratio_q25=(M1or2_price_difference_ratio_q25%*%coef1or2)*omegaboot[bootjunk]
  V2_price_difference_ratio_q25= -V1_price_difference_ratio_q25
  
  VV1_price_difference_ratio_q25=M121_price_difference_ratio_q25%*%coef121
  VV2_price_difference_ratio_q25=M122_price_difference_ratio_q25%*%coef122
  
  V1_price_difference_ratio_q25=exp(V1_price_difference_ratio_q25)
  V2_price_difference_ratio_q25=exp(V2_price_difference_ratio_q25)
  VV1_price_difference_ratio_q25=exp(VV1_price_difference_ratio_q25)
  VV2_price_difference_ratio_q25=exp(VV2_price_difference_ratio_q25)
  
  
  V3_price_difference_ratio_q25=(VV1_price_difference_ratio_q25+VV2_price_difference_ratio_q25)**(-c_phiboot[bootjunk])
  V4_price_difference_ratio_q25=(VV1_price_difference_ratio_q25+VV2_price_difference_ratio_q25)**(c_phiboot[bootjunk])
  
  
  
  p_price_difference_ratio_imp_q25=(one/(one+V1_price_difference_ratio_q25))*(one/(one+V3_price_difference_ratio_q25))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_price_difference_ratio_nby_q25=(one/(one+V2_price_difference_ratio_q25))*(one/(one+V3_price_difference_ratio_q25))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_price_difference_ratio_pln_q25= one/(one+V4_price_difference_ratio_q25) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  
  
  M1or2_price_difference_ratio_q75 =cbind(one,minus_q,q,minus_z[,1:3],-price_difference_ratio_q75)
  M121_price_difference_ratio_q75 =cbind(one,q,minus_x)
  M122_price_difference_ratio_q75 =cbind(one,q,minus_x)
  
  
  V1_price_difference_ratio_q75=(M1or2_price_difference_ratio_q75%*%coef1or2)*omegaboot[bootjunk]
  V2_price_difference_ratio_q75= -V1_price_difference_ratio_q75
  
  VV1_price_difference_ratio_q75=M121_price_difference_ratio_q75%*%coef121
  VV2_price_difference_ratio_q75=M122_price_difference_ratio_q75%*%coef122
  
  V1_price_difference_ratio_q75=exp(V1_price_difference_ratio_q75)
  V2_price_difference_ratio_q75=exp(V2_price_difference_ratio_q75)
  VV1_price_difference_ratio_q75=exp(VV1_price_difference_ratio_q75)
  VV2_price_difference_ratio_q75=exp(VV2_price_difference_ratio_q75)
  
  
  V3_price_difference_ratio_q75=(VV1_price_difference_ratio_q75+VV2_price_difference_ratio_q75)**(-c_phiboot[bootjunk])
  V4_price_difference_ratio_q75=(VV1_price_difference_ratio_q75+VV2_price_difference_ratio_q75)**(c_phiboot[bootjunk])
  
  
  
  p_price_difference_ratio_imp_q75=(one/(one+V1_price_difference_ratio_q75))*(one/(one+V3_price_difference_ratio_q75))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
  p_price_difference_ratio_nby_q75=(one/(one+V2_price_difference_ratio_q75))*(one/(one+V3_price_difference_ratio_q75))	#probability of no buy, a vector in which each element is p2 in each choice situation
  p_price_difference_ratio_pln_q75= one/(one+V4_price_difference_ratio_q75) 			              	#probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
  
  PE_price_difference_ratio_impulse = ((p_price_difference_ratio_imp_q75-p_price_difference_ratio_imp_q25)/p_price_difference_ratio_imp_q25)*100
  PE_price_difference_ratio_nobuy = ((p_price_difference_ratio_nby_q75-p_price_difference_ratio_nby_q25)/p_price_difference_ratio_nby_q25)*100
  PE_price_difference_ratio_plan = ((p_price_difference_ratio_pln_q75-p_price_difference_ratio_pln_q25)/p_price_difference_ratio_pln_q25)*100
  
  
  E_price_difference_ratio_impulse[bootjunk] <- mean(PE_price_difference_ratio_impulse)
  E_price_difference_ratio_nobuy[bootjunk] <- mean(PE_price_difference_ratio_nobuy)
  E_price_difference_ratio_plan[bootjunk] <- mean(PE_price_difference_ratio_plan)
  
  
} # end of bootstrapping loop

#sink("smarket.txt")

#finish=Sys.time()  #print stop time so we can see how long it took to simulate

#duration = difftime(finish, start,  units = "secs")

#cat("The estimation took", duration,"seconds","\n","\n")

variables= rbind("Sale:", "Display:", "New Product:", "Price Difference:", 
                 "Age:", "Household Size:", "Preplan Shopping:",   
                 "Sex:", "Trait Impulsiveness:", "Hedonic:", "Ready to Use:", "Sale Proneness:", "Unhealthiness:", "Value consciousness:", "Variety seeking:", 
                 "Sex:", "Trait Impulsiveness:", "Hedonic:", "Ready to Use:", "Sale Proneness:", "Unhealthiness:", "Value consciousness:", "Variety seeking:",  
                 "ksi1:", "ksi2:", "ksi12:",  
                 "omega:", "c_phi:")

coefnames= rbind("chi11:", "chi12:", "chi13:", "chi14:", 
                 "beta31:", "beta32:", "beta33:", 
                 "alpha11:", "alpha12:", "alpha13:", "alpha14:", "alpha15:", "alpha16:", "alpha17:", "alpha18:", 
                 "alpha21:", "alpha22:", "alpha23:", "alpha24:", "alpha25:", "alpha26:", "alpha27:", "alpha28:",  
                 "ksi1:", "ksi2:", "ksi12:",  
                 "omega:", "c_phi:")

bootcoef = rbind(chi11boot, chi12boot, chi13boot, chi14boot,  
                 beta31boot, beta32boot, beta33boot,   
                 alpha11boot, alpha12boot, alpha13boot, alpha14boot, alpha15boot, alpha16boot, alpha17boot, alpha18boot, 
                 alpha21boot, alpha22boot, alpha23boot, alpha24boot, alpha25boot, alpha26boot, alpha27boot, alpha28boot, 
                 ksi1_boot, ksi2_boot, ksi12_boot,
                 omegaboot, c_phiboot)

alpha_variables= rbind("Sex:", "Trait Impulsiveness:", "Hedonic:", "Ready to Use:", "Sale Proneness:", "Unhealthiness:", "Value consciousness:", "Variety seeking:")

alpha1 = rbind(alpha11boot, alpha12boot, alpha13boot, alpha14boot, alpha15boot, alpha16boot, alpha17boot, alpha18boot)
alpha2 = rbind(alpha21boot, alpha22boot, alpha23boot, alpha24boot, alpha25boot, alpha26boot, alpha27boot, alpha28boot)
alpha_diff = alpha1 - alpha2
alpha_diff1 = alpha_diff[1,]
alpha_diff2 = alpha_diff[2,]
alpha_diff3 = alpha_diff[3,]
alpha_diff4 = alpha_diff[4,]
alpha_diff5 = alpha_diff[5,]
alpha_diff6 = alpha_diff[6,]
alpha_diff7 = alpha_diff[7,]
alpha_diff8 = alpha_diff[8,]

alpha_diff_mean = rbind(mean(alpha_diff1), mean(alpha_diff2), mean(alpha_diff3), mean(alpha_diff4), mean(alpha_diff5), mean(alpha_diff6), mean(alpha_diff7), mean(alpha_diff8))
alpha_diff_sd = rbind(sd(alpha_diff1), sd(alpha_diff2), sd(alpha_diff3), sd(alpha_diff4), sd(alpha_diff5), sd(alpha_diff6), sd(alpha_diff7), sd(alpha_diff8))
alpha_diff_tstat = alpha_diff_mean/alpha_diff_sd

# beta34alpha16 = beta34boot - alpha16boot
# b34a16mean = mean(beta34alpha16)
# b34a16sd = sd(beta34alpha16)
# b34a16tstat = b34a16mean/b34a16sd
# 
# beta34alpha26 = beta34boot - alpha26boot
# b34a26mean = mean(beta34alpha26)
# b34a26sd = sd(beta34alpha26)
# b34a26tstat = b34a26mean/b34a26sd

bootcoefmean = rbind(mean(chi11boot), mean(chi12boot), mean(chi13boot), mean(chi14boot),  
                     mean(beta31boot), mean(beta32boot), mean(beta33boot),  
                     mean(alpha11boot), mean(alpha12boot), mean(alpha13boot), mean(alpha14boot), mean(alpha15boot), mean(alpha16boot), mean(alpha17boot), mean(alpha18boot), 
                     mean(alpha21boot), mean(alpha22boot), mean(alpha23boot), mean(alpha24boot), mean(alpha25boot), mean(alpha26boot), mean(alpha27boot), mean(alpha28boot), 
                     mean(ksi1_boot),
                     mean(ksi2_boot),
                     mean(ksi12_boot), 
                     mean(omegaboot), mean(c_phiboot))


bootsd = rbind(sd(chi11boot), sd(chi12boot), sd(chi13boot), sd(chi14boot),  
               sd(beta31boot), sd(beta32boot), sd(beta33boot), 
               sd(alpha11boot), sd(alpha12boot), sd(alpha13boot), sd(alpha14boot), sd(alpha15boot), sd(alpha16boot), sd(alpha17boot), sd(alpha18boot), 
               sd(alpha21boot), sd(alpha22boot), sd(alpha23boot), sd(alpha24boot), sd(alpha25boot), sd(alpha26boot), sd(alpha27boot), sd(alpha28boot), 
               sd(ksi1_boot),
               sd(ksi2_boot),
               sd(ksi12_boot), 
               sd(omegaboot), sd(c_phiboot))

boottstat= bootcoefmean / bootsd

resultsc = cbind(variables, coefnames, bootcoefmean, bootsd, boottstat)

resultsc2 = cbind(alpha_variables, alpha_diff_mean, alpha_diff_sd, alpha_diff_tstat)



elasticities_mean = rbind(mean(E_Age_impulse), 
                          mean(E_Age_nobuy), 
                          mean(E_Age_plan), 
                          
                          mean(E_household_size_impulse),
                          mean(E_household_size_nobuy),
                          mean(E_household_size_plan),
                          
                          mean(E_Preplan_shopping_impulse),
                          mean(E_Preplan_shopping_nobuy),
                          mean(E_Preplan_shopping_plan),
                          
                          mean(E_Unhealthiness_Index_impulse),
                          mean(E_Unhealthiness_Index_nobuy), 
                          mean(E_Unhealthiness_Index_plan),
                          
                          mean(E_Sex_impulse), 
                          mean(E_Sex_nobuy), 
                          mean(E_Sex_plan), 
                          
                          mean(E_Trait_impulsiveness_impulse), 
                          mean(E_Trait_impulsiveness_nobuy), 
                          mean(E_Trait_impulsiveness_plan),
                          
                          mean(E_Hedonic_impulse), 
                          mean(E_Hedonic_nobuy), 
                          mean(E_Hedonic_plan),
                          
                          mean(E_Ready_To_Use_impulse), 
                          mean(E_Ready_To_Use_nobuy), 
                          mean(E_Ready_To_Use_plan), 
                          
                          mean(E_Sale_proneness_impulse), 
                          mean(E_Sale_proneness_nobuy),
                          mean(E_Sale_proneness_plan),
                          
                          mean(E_Value_consciousness_impulse), 
                          mean(E_Value_consciousness_nobuy),
                          mean(E_Value_consciousness_plan),
                          
                          mean(E_Variety_seeking_impulse), 
                          mean(E_Variety_seeking_nobuy),
                          mean(E_Variety_seeking_plan),
                          
                          mean(E_Sale_impulse), 
                          mean(E_Sale_nobuy),
                          mean(E_Sale_plan),
                          
                          mean(E_Display_impulse),
                          mean(E_Display_nobuy),
                          mean(E_Display_plan),
                          
                          mean(E_New_Product_impulse),
                          mean(E_New_Product_nobuy),
                          mean(E_New_Product_plan),
                          
                          mean(E_price_difference_ratio_impulse),
                          mean(E_price_difference_ratio_nobuy),
                          mean(E_price_difference_ratio_plan))

elasticities_sd = rbind(sd(E_Age_impulse), 
                        sd(E_Age_nobuy), 
                        sd(E_Age_plan), 
                        
                        sd(E_household_size_impulse),
                        sd(E_household_size_nobuy),
                        sd(E_household_size_plan),
                        
                        sd(E_Preplan_shopping_impulse),
                        sd(E_Preplan_shopping_nobuy),
                        sd(E_Preplan_shopping_plan),
                        
                        sd(E_Unhealthiness_Index_impulse),
                        sd(E_Unhealthiness_Index_nobuy), 
                        sd(E_Unhealthiness_Index_plan),
                        
                        sd(E_Sex_impulse), 
                        sd(E_Sex_nobuy), 
                        sd(E_Sex_plan), 
                        
                        sd(E_Trait_impulsiveness_impulse), 
                        sd(E_Trait_impulsiveness_nobuy), 
                        sd(E_Trait_impulsiveness_plan),
                        
                        sd(E_Hedonic_impulse), 
                        sd(E_Hedonic_nobuy), 
                        sd(E_Hedonic_plan),
                        
                        sd(E_Ready_To_Use_impulse), 
                        sd(E_Ready_To_Use_nobuy), 
                        sd(E_Ready_To_Use_plan), 
                        
                        sd(E_Sale_proneness_impulse), 
                        sd(E_Sale_proneness_nobuy),
                        sd(E_Sale_proneness_plan),
                        
                        sd(E_Value_consciousness_impulse), 
                        sd(E_Value_consciousness_nobuy),
                        sd(E_Value_consciousness_plan),
                        
                        sd(E_Variety_seeking_impulse), 
                        sd(E_Variety_seeking_nobuy),
                        sd(E_Variety_seeking_plan),
                        
                        sd(E_Sale_impulse), 
                        sd(E_Sale_nobuy),
                        sd(E_Sale_plan),
                        
                        sd(E_Display_impulse),
                        sd(E_Display_nobuy),
                        sd(E_Display_plan),
                        
                        sd(E_New_Product_impulse),
                        sd(E_New_Product_nobuy),
                        sd(E_New_Product_plan),
                        
                        sd(E_price_difference_ratio_impulse),
                        sd(E_price_difference_ratio_nobuy),
                        sd(E_price_difference_ratio_plan))

tstat_E = elasticities_mean/elasticities_sd

cnames2 = c("Age_impulse", 
            "Age_nobuy",
            "Age_plan",
            
            "household_size_impulse",
            "household_size_nobuy",
            "household_size_plan",
            
            "Preplan_shopping_impulse",
            "Preplan_shopping_nobuy",
            "Preplan_shopping_plan",
            
            "Unhealthiness_Index_impulse",
            "Unhealthiness_Index_nobuy",
            "Unhealthiness_Index_plan",
            
            "Sex_impulse", 
            "Sex_nobuy", 
            "Sex_plan",
            
            "Trait_impulsiveness_impulse", 
            "Trait_impulsiveness_nobuy",
            "Trait_impulsiveness_plan",
            
            "Hedonic_impulse", 
            "Hedonic_nobuy",
            "Hedonic_plan",
            
            "Ready_To_Use_impulse", 
            "Ready_To_Use_nobuy",
            "Ready_To_Use_plan",
            
            "Sale_proneness_impulse", 
            "Sale_proneness_nobuy", 
            "Sale_proneness_plan",
            
            "Value_consciousness_impulse", 
            "Value_consciousness_nobuy", 
            "Value_consciousness_plan", 
            
            "Variety_seeking_impulse", 
            "Variety_seeking_nobuy", 
            "Variety_seeking_plan",
            
            "Sale_impulse", 
            "Sale_nobuy",
            "Sale_plan",
            
            "Display_impulse",
            "Display_nobuy",
            "Display_plan",
            
            "New_Product_impulse",
            "New_Product_nobuy",
            "New_Product_plan",
            
            "price_difference_ratio_impulse",
            "price_difference_ratio_nobuy",
            "price_difference_ratio_plan")



E_all= rbind(E_Age_impulse, 
             E_Age_nobuy, 
             E_Age_plan, 
             
             E_household_size_impulse, 
             E_household_size_nobuy, 
             E_household_size_plan, 
             
             E_Preplan_shopping_impulse, 
             E_Preplan_shopping_nobuy, 
             E_Preplan_shopping_plan, 
             
             E_Unhealthiness_Index_impulse, 
             E_Unhealthiness_Index_nobuy, 
             E_Unhealthiness_Index_plan, 
             
             E_Sex_impulse, 
             E_Sex_nobuy, 
             E_Sex_plan, 
             
             E_Trait_impulsiveness_impulse, 
             E_Trait_impulsiveness_nobuy, 
             E_Trait_impulsiveness_plan, 
             
             E_Hedonic_impulse, 
             E_Hedonic_nobuy, 
             E_Hedonic_plan, 
             
             E_Ready_To_Use_impulse, 
             E_Ready_To_Use_nobuy, 
             E_Ready_To_Use_plan, 
             
             E_Sale_proneness_impulse, 
             E_Sale_proneness_nobuy, 
             E_Sale_proneness_plan, 
             
             E_Value_consciousness_impulse, 
             E_Value_consciousness_nobuy, 
             E_Value_consciousness_plan, 
             
             E_Variety_seeking_impulse, 
             E_Variety_seeking_nobuy, 
             E_Variety_seeking_plan, 
             
             E_Sale_impulse, 
             E_Sale_nobuy, 
             E_Sale_plan, 
             
             E_Display_impulse, 
             E_Display_nobuy, 
             E_Display_plan, 
             
             E_New_Product_impulse, 
             E_New_Product_nobuy, 
             E_New_Product_plan, 
             
             E_price_difference_ratio_impulse, 
             E_price_difference_ratio_nobuy, 
             E_price_difference_ratio_plan)


results_E = cbind(cnames2, elasticities_mean, elasticities_sd, tstat_E)


# beta_diff1 = cbind(b34a16mean, b34a16sd, b34a16tstat)
# beta_diff2 = cbind(b34a26mean, b34a26sd, b34a26tstat)

finish=Sys.time()  #print stop time so we can see how long it took to simulate
duration = difftime(finish, start,  units = "mins")

options(max.print=1000000)

sink("DEopt.txt")

cat("Number of loops =", bootiter, "#of generations = ", itmax,  "NP = ", population, "\n","\n")

cat("The estimation duration =", duration, "minutes", "\n","\n")

# cat('bootstrap coefficient means',"\n")
# print(bootcoefmean)
# cat('bootstrap coefficient std',"\n")
# print(bootsd)
# cat('bootstrap coefficient Tstat',"\n")
# print(boottstat)


cat('coefficient',"\t","\t", 'mean', "\t", "\t", 'sd', "\t", 't-stat', "\n")
print(resultsc)


cat('Alpha 1 - Alpha2',"\n")
cat('coefficient',"\t","\t", 'mean', "\t", "\t", 'sd', "\t", 't-stat', "\n")
print(resultsc2)

# cat('Beta34 - Alpha16',"\n")
# cat('coefficient',"\t","\t", 'mean', "\t", "\t", 'sd', "\t", 't-stat', "\n")
# print(beta_diff1)
# 
# cat('Beta34 - Alpha26',"\n")
# cat('coefficient',"\t","\t", 'mean', "\t", "\t", 'sd', "\t", 't-stat', "\n")
# print(beta_diff2)

cat('coefficient elasticities ',"\t","\t", 'mean', "\t", "\t", 'sd', "\t", 't-stat', "\n")
print(results_E)

cat('elasticities from all bootstarp rounds', "\n")
print(E_all)


cat('Coefficient Estimates from all bootstarp rounds', "\n")
print(bootcoef)

sink()
file.show("DEopt.txt")
