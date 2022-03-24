#                 SEQUENTIAL DECISIONS

## R code to estimate a sequential decision model 



#                          PART 1:  THE SETUP
start=Sys.time()  # start time
start
# Put a title for the run in the quotes below, to be printed at the top of the output file.

cat("sequential decision makin", "\n")
setwd("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R")
# DATA
# 
library(maxLik)
library(foreign)  #Load package that reads .csv files

spmkt1 <- read.csv("C:/Users/pkianian/Dropbox/Sequential Decisions/Datasets/bootstrap datasets/XMATBOOT50 1.csv", header=TRUE, sep=",", fill=TRUE, row.names=NULL)
spmkt2 <- read.csv("C:/Users/pkianian/Dropbox/Sequential Decisions/Datasets/bootstrap datasets/XMATBOOT50 2.csv", header=TRUE, sep=",", fill=TRUE, row.names=NULL) 

spmkt <- rbind(spmkt1,spmkt2)


attach(spmkt)


start=Sys.time()  # start time

# Number of alternatives in each choice situation

NALT=3 

# This is the number of rows of data in XMAT below.

NROWS= nrow(spmkt)

# Number of choice situations in dataset.

NCS= NROWS/3



attach(spmkt)

#1. put on shopping list
#2. impulse purchase
#3. no buy


# This is the number of rows of data in XMAT below.

chi21boot = vector()
chi22boot = vector()
chi23boot = vector()
chi24boot = vector()

beta11boot = vector()
beta12boot = vector()
beta13boot = vector()
beta14boot = vector()


alpha21boot = vector()
alpha22boot = vector()
alpha23boot = vector()
alpha24boot = vector()
alpha25boot = vector()
alpha26boot = vector()
alpha27boot = vector()
alpha28boot = vector()
alpha29boot = vector()

ksi1boot = vector()
ksi2boot = vector()

#x variables:

#1 Age, 
#2 household_size, 
#3 Preplan_shopping,
#4 Sex,

# q variables:

#1 Sex, 
#2 household_size,
#3 Trait_impulsiveness, 
#4 Hedonic, 
#5 Ready_To_Use, 
#6 Sale_proneness, 
#7 Unhealthiness_Index, 
#8 Value_consciousness, 
#9 Variety_seeking, 

# z variables:

#1 Sale, 
#2 Display, 
#3 New_Product, 
#4 price_difference_ratio

# Load and/or create XMAT, a matrix that contains the data.



XMAT=cbind(idcase, 
           idalt, 
           depvar, 
           
           Age, 
           household_size, 
           Preplan_shopping, 
           Sex,
           
           Sex,
           household_size,
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
           price_difference_ratio)    #Matrix including all variables in the dataset


bootsample = 6187                 #Bootstrapping sample size
bootiter = 100                    #Number of bootstrap iterations

NROWS2 = bootsample*NALT


one3=c(1, 1, 1)

for (bootjunk in 1:bootiter) {      # start bootstrap loop
  
  
  XMATBOOT<-XMAT[(1+((bootjunk-1)*NROWS2)):(bootjunk*NROWS2),]
  
  
  x=XMATBOOT[seq(1,nrow(XMATBOOT),3),4:7]		  #a (bootsample*NALT)x column vector
  q=XMATBOOT[seq(1,nrow(XMATBOOT),3),8:16]			#a (bootsample*NALT)x column vector
  z=XMATBOOT[seq(1,nrow(XMATBOOT),3),17:20]			#a (bootsample*NALT)x column vector
  y=XMATBOOT[,3]			    #a (bootsample*NALT)x column vector of 0 and 1
  
  
  dim(y)=c(NALT,bootsample)	#restructure y  so there are NALT rows and bootsample cols
  
  one=rep(1,bootsample)	    		#a column vector of ones with bootsample rows

  M1or23=cbind(one,x)         #ksi1+x*beta1
  M2or3=cbind(one,q,z)        #ksi2+q*alpha2+z*chi2
  
  #choice parameters:
  #B=(chi2(4), beta1(4), alpha2(9), ksi1, ksi2) initial value of parameters (7 parameters)

  # chi21 = B[1]
  # chi22 = B[2]
  # chi23 = B[3]
  # chi24 = B[4]
  # 
  # beta11 = B[5]
  # beta12 = B[6]
  # beta13 = B[7]
  # beta14 = B[8]
  #
  # alpha21 = B[9]
  # alpha22 = B[10]
  # alpha23 = B[11]
  # alpha24 = B[12]
  # alpha25 = B[13]
  # alpha26 = B[14]
  # alpha27 = B[15]
  # alpha28 = B[16]
  # alpha29 = B[17]
  #
  #ksi1 = B[18]
  #ksi2 = B[19] : Ksi2-3
  
  #							         COMPUTING THE LOGLIKELIHOOD
  # Function to calculate loglikelihood function for nested model
  
  param=rep(0,19)
  
  loglik=function(parama) { #curly bracket is the start of subrutine
    
    coef1or23=c(parama[18],parama[5],parama[6],parama[7],parama[8])     # Formula1or23: ksi1+x*beta1
    coef2or3=c(parama[19],parama[9],parama[10],parama[11],parama[12],parama[13],parama[14],parama[15],parama[16],parama[17],
               parama[1],parama[2],parama[3],parama[4])	# Formula2or3:  ksi2+q*alpha2+z*chi2  
    
    
    
    V1=-(M1or23%*%coef1or23)
    V2= -V1
    
    V1e=exp(V1)
    V2e=exp(V2)
    
    V2or3= -(M2or3%*%coef2or3)
    MV2or3 = -V2or3
    
    V2or3e = exp(V2or3)
    MV2or3e = exp(MV2or3)
    
    #choice probabilities:
    
    p3=(one/(one+V1e))	                       #probability of plan
    p1=(one/(one+V2or3e))*(one/(one+V2e))   	 #probability of impulse
    p2=(one/(one+MV2or3e))*(one/(one+V2e))     #probability of No buy			              	
    
    P=rbind(t(p1), t(p2), t(p3))			#(NALT x NCS) matrix in which each column has p1, p2 and p3 for each choice situation
    
    
    L1=y*P					#(NALT x NCS) matrix, elemenwise multiplication of prpbability of choice by dummy of chosen alternative
    
    
    prob = one3%*%L1
    add=t(rep(1e-16,bootsample))
    prob=prob+add
    
    
    LL=log(prob)				#one3%*%P gives a 1 x NCS vector of probability of choice for the chosen alternative in each choice situation and L is log of those probabilities
    
    
    return(LL)
  } #end of LOGLIK
  
  
  #Seed to use for random number generator; changing this gives different RV draws
  SEED1=1919
  
  # OPTIMIZATION PARAMETERS
  # Maximum number of iterations for the optimization routine.
  # The code will abort after ITERMAX iterations, even if convergence has
  # not been achieved. The default is 400.
  
  MAXITERS=1000
  
  # Convergence criterion based on the maximum change in parameters that is considered
  # to represent convergence. If all the parameters change by less than PARAMTOL 
  # from one iteration to the next, then the code considers convergence to have been
  # achieved. The default is 0.000001.
  
  PARAMTOL=1.0E-10
  
  # Convergence criterion based on change in the log-likelihood that is
  # considered to represent convergence. If the log-likelihood value changes
  # less than LLTOL from one iteration to the next, then the optimization routine
  # considers convergence to have been achieved. The default is 0.000001.
  
  LLTOL=1.0E-5
  
  
  
  
  
  
  #							PART A.3 DO IT
  
  # Script to use with nested logit, to call optimizer and print results
  
  cat('Start estimation',"\n")
  
  # makLik adjusts parameters to maximize the likelihood.  The function loglik can either return the NCS LL[n]'s or sum LL .
  # It returns par=optimal parameters, maximum=maximized value, code=success flag
  # hessian=matrix of second derivatives.
  
  
  results=maxLik(loglik,start=param,method="BFGS",iterlim=MAXITERS,tol=LLTOL,steptol=PARAMTOL)
  cat(' ',"\n")
  exitflag=results$code
  if (exitflag == 2)  cat('Convergence achieved.',"\n") else {#begin else 1
    if(exitflag == 0) cat('Convergence achieved.',"\n") else {#begin else 2
      if (exitflag == 1) cat('Reached iteration limit.',"\n") else {#begin else 3
        cat('Convergence not achieved.',"\n") 
      }#end else 3
    }#end else 2 
  }#end else 1
  cat('The current value of the parameters and hessian',"\n")
  cat('can be accesses as variables paramhat and hessian.',"\n")
  cat('Results are not printed because no convergence.',"\n")
  
  
  #							PART A.4  THE RESULTS OF ESTIMATING OMEGA AND C_PHI
  
  
  paramhat<-results$estimate
  
  
  cat('paramhat of loop:')
  cat(' ',"\n")
  print(paramhat)
  cat('loop number:')
  cat(' ',"\n")
  print(bootjunk)
  cat('start time:')
  cat(' ',"\n")
  print(start)
  
  
  chi21boot[bootjunk] = paramhat[1]
  chi22boot[bootjunk] = paramhat[2]
  chi23boot[bootjunk] = paramhat[3]
  chi24boot[bootjunk] = paramhat[4]
  
  beta11boot[bootjunk] = paramhat[5]
  beta12boot[bootjunk] = paramhat[6]
  beta13boot[bootjunk] = paramhat[7]
  beta14boot[bootjunk] = paramhat[8]
  
  
  alpha21boot[bootjunk] = paramhat[9]
  alpha22boot[bootjunk] = paramhat[10]
  alpha23boot[bootjunk] = paramhat[11]
  alpha24boot[bootjunk] = paramhat[12]
  alpha25boot[bootjunk] = paramhat[13]
  alpha26boot[bootjunk] = paramhat[14]
  alpha27boot[bootjunk] = paramhat[15]
  alpha28boot[bootjunk] = paramhat[16]
  alpha29boot[bootjunk] = paramhat[17]
  
  ksi1boot[bootjunk] = paramhat[18]
  ksi2boot[bootjunk] = paramhat[19]
  

  
} # end of bootstrapping loop


variables= rbind("Age", 
                 "household_size", 
                 "Preplan_shopping", 
                 "Sex",
                 
                 "Sex",
                 "household_size",
                 "Trait_impulsiveness", 
                 "Hedonic", 
                 "Ready_To_Use", 
                 "Sale_proneness", 
                 "Unhealthiness_Index", 
                 "Value_consciousness", 
                 "Variety_seeking", 
                 
                 "Sale", 
                 "Display", 
                 "New_Product", 
                 "price_difference_ratio",
                 
                 "ksi1:",
                 "ksi2:")

coefnames= rbind("beta11",
                 "beta12",
                 "beta13",
                 "beta14",
                 
                 "alpha21",
                 "alpha22",
                 "alpha23",
                 "alpha24",
                 "alpha25",
                 "alpha26",
                 "alpha27",
                 "alpha28",
                 "alpha29",
                 
                 "chi21",
                 "chi22",
                 "chi23",
                 "chi24",
                 
                 "ksi1",
                 "ksi2")


bootcoefmean = rbind(mean(beta11boot),
                     mean(beta12boot),
                     mean(beta13boot),
                     mean(beta14boot),
                     
                     mean(alpha21boot),
                     mean(alpha22boot),
                     mean(alpha23boot),
                     mean(alpha24boot),
                     mean(alpha25boot),
                     mean(alpha26boot),
                     mean(alpha27boot),
                     mean(alpha28boot),
                     mean(alpha29boot),
                     
                     mean(chi21boot),
                     mean(chi22boot),
                     mean(chi23boot),
                     mean(chi24boot),
                     
                     mean(ksi1boot),
                     mean(ksi2boot))




bootsd = rbind(sd(beta11boot),
               sd(beta12boot),
               sd(beta13boot),
               sd(beta14boot),
               
               sd(alpha21boot),
               sd(alpha22boot),
               sd(alpha23boot),
               sd(alpha24boot),
               sd(alpha25boot),
               sd(alpha26boot),
               sd(alpha27boot),
               sd(alpha28boot),
               sd(alpha29boot),
               
               sd(chi21boot),
               sd(chi22boot),
               sd(chi23boot),
               sd(chi24boot),
               
               sd(ksi1boot),
               sd(ksi2boot))

boottstat= bootcoefmean / bootsd

resultsc = cbind(variables, coefnames, bootcoefmean, bootsd, boottstat)

sink("MSL.txt")

finish=Sys.time()  #print stop time so we can see how long it took to simulate
duration = difftime(finish, start,  units = "mins")


cat('coefficient',"\t","\t", 'mean', "\t", "\t", 'sd', "\t", 't-stat', "\n")
print(resultsc)


sink()
file.show("MSL.txt")