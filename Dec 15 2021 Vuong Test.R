library(foreign)  #Load package that reads .csv files
library(nonnest2) #load package for vuong test

#100OSL
# d <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/Second Essay Data/November 2021 Data/OSL November 10 2021.csv", header=TRUE, sep=",", fill=TRUE)

#100MSL
#d <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/Second Essay Data/November 2021 Data/MSL November 10 2021.csv", header=TRUE, sep=",", fill=TRUE)

#20OSL_80MSL
# d <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/Second Essay Data/November 2021 Data/DATA_20OSL_80MSL.csv", header=TRUE, sep=",", fill=TRUE)

#50OSL_50MSL
# d <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/Second Essay Data/November 2021 Data/DATA_50OSL_50MSL.csv", header=TRUE, sep=",", fill=TRUE)

#80OSL_20MSL
 d <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/Second Essay Data/November 2021 Data/DATA_80OSL_20MSL.csv", header=TRUE, sep=",", fill=TRUE)

attach(d)
 
 start=Sys.time()  # start time
 
 # Number of alternatives in each choice situation
 
 NALT=3 
 
 # This is the number of rows of data in XMAT below.
 
 NROWS= nrow(d)
 
 # Number of choice situations in dataset.
 
 NCS= NROWS/3
 
 
XMAT=cbind(idcase.original,
           idalt, 
           choice,
           age, 
           impulsiveness,
           Sale,
           P1,
           P2,
           P3,
           OSL)    #Matrix including all variables in the dataset

bootsample = 5000                 #Bootstrapping sample size
NROWS2 = bootsample*NALT

one3=c(1, 1, 1)

x=XMAT[seq(1,nrow(XMAT),3),4]		  #a (bootsample*NALT)x column vector
q=XMAT[seq(1,nrow(XMAT),3),5]			#a (bootsample*NALT)x column vector
z=XMAT[seq(1,nrow(XMAT),3),6]			#a (bootsample*NALT)x column vector
y=XMAT[,3]		                  	    #a (bootsample*NALT)x column vector of 0 and 1

dim(y)=c(NALT,bootsample)	#restructure y  so there are NALT rows and bootsample cols

minus_x = x*-1		#vector of -x for each choice situation
minus_q = q*-1		#vector of -q for each choice situation
minus_z = z*-1		#vector of -z for each choice situation

one=rep(1,bootsample)	    		#a column vector of ones with bootsample rows
minus_one=rep(-1,bootsample)	#a column vector of -1 with bootsample rows


###########################################  OSL  ########################################

OSL_M2or3=cbind(one,minus_q,q,minus_z)    # Formula2or3: ksi23-q*alpha2+q*alpha3-z*chi2
OSL_M23or1=cbind(one,q,minus_x)           # Formula23or1(1):  ksi2+q*alpha2-x*beta1

#choice parameters:
#B=(chi2(1), beta1(1), alpha2(1), alpha3(1), ksi2, ksi3, ksi23) 
# chi2 = B[1]
# beta1 = B[2]
# alpha2 = B[3]
# alpha3 = B[4]
# ksi2 = B[5]
# ksi3 = B[6]
# ksi23 = B[7]
# theta = parama[8]
# kappa = parama[9] 

#XparamOSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/OSL code/OSL DEoptim/OSL on 100OSL DEoptim.csv", header=TRUE, sep=",", fill=TRUE)

#XparamOSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/OSL code/OSL DEoptim/OSL on 100MSL DEoptim.csv", header=TRUE, sep=",", fill=TRUE)

#XparamOSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/OSL code/OSL DEoptim/OSL on 20OSL_80MSL DEoptim.csv", header=TRUE, sep=",", fill=TRUE)

#XparamOSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/OSL code/OSL DEoptim/OSL on 50OSL_50MSL DEoptim.csv", header=TRUE, sep=",", fill=TRUE)

XparamOSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/OSL code/OSL DEoptim/OSL on 80OSL_20MSL DEoptim.csv", header=TRUE, sep=",", fill=TRUE)


paramosl = XparamOSL[,"mean"] 


OSL_coef2or3=c(paramosl[7],paramosl[3],paramosl[4],paramosl[1])     # Formula1or2: ksi23-q*alpha2+q*alpha3-z*chi2
OSL_coef231=c(paramosl[5],paramosl[3],paramosl[2])		            # Formula121:  ksi2+q*alpha2-x*beta1  
OSL_coef232=c(paramosl[6],paramosl[4],paramosl[2])                # Formula122:  ksi3+q*alpha3-x*beta1

OSL_V1=(OSL_M2or3%*%OSL_coef2or3)*paramosl[8]
OSL_V2= -OSL_V1
OSL_V1=exp(OSL_V1)
OSL_V2=exp(OSL_V2)

OSL_VV1=OSL_M23or1%*%OSL_coef231
OSL_VV2=OSL_M23or1%*%OSL_coef232
OSL_VV1=exp(OSL_VV1)
OSL_VV2=exp(OSL_VV2)

OSL_V3=(OSL_VV1+OSL_VV2)**(-paramosl[9])
OSL_V4=(OSL_VV1+OSL_VV2)**(paramosl[9])

#choice probabilities:

OSL_p1= one/(one+OSL_V4) 			            #probability of putting on shopping list, a a vector in which each element is p3 in each choice situation
OSL_p2=(one/(one+OSL_V1))*(one/(one+OSL_V3))	#probability of impulse purchase, a vector in which each element is p1 in each choice situation
OSL_p3=(one/(one+OSL_V2))*(one/(one+OSL_V3))	#probability of no buy, a vector in which each element is p2 in each choice situation

OSL_P<-rbind(t(OSL_p1), t(OSL_p2), t(OSL_p3))			#(NALT x NCS) matrix in which each column has p1, p2 and p3 for each choice situation

OSL_L1=y*OSL_P					#(NALT x NCS) matrix, elemenwise multiplication of prpbability of choice by dummy of chosen alternative

OSL_prob = one3%*%OSL_L1
add=t(rep(1e-16,bootsample))      # to prevent "zero"s
OSL_prob=OSL_prob+add

OSL_LL1=log(OSL_prob)				#one3%*%P gives a 1 x NCS vector of probability of choice for the chosen alternative in each choice situation and L is log of those probabilities
LLOSL= sum(OSL_LL1)




###########################################  MSL  ########################################

MSL_M1or23=cbind(one,x)         #ksi1+x*beta1
MSL_M2or3=cbind(one,q,z)        #ksi2+q*alpha2+z*chi2

#choice parameters:
#B=(chi2(1), beta1(1), alpha2(1), ksi1, ksi2) initial value of parameters (7 parameters)
# chi2 = B[1]
# beta1 = B[2]
# alpha2 = B[3]
# ksi1 = B[4]
# ksi2 = B[5]


# XparamMSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/MSL code/November 22 2021 MSL on 100OSL Results.csv", header=TRUE, sep=",", fill=TRUE)

# XparamMSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/MSL code/November 22 2021 MSL on 100MSL Results.csv", header=TRUE, sep=",", fill=TRUE)

# XparamMSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/MSL code/November 22 2021 MSL on 20OSL_80MSL Results.csv", header=TRUE, sep=",", fill=TRUE)

# XparamMSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/MSL code/November 22 2021 MSL on 50OSL_50MSL Results.csv", header=TRUE, sep=",", fill=TRUE)

 XparamMSL <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/R/Results November 2021/MSL code/November 22 2021 MSL on 80OSL_20MSL Results.csv", header=TRUE, sep=",", fill=TRUE)

parammsl = XparamMSL[,3] 

MSL_coef1or23=c(parammsl[4],parammsl[2])                              # Formula1or23: ksi1+x*beta1
MSL_coef2or3=c(parammsl[5],parammsl[3],parammsl[1])		                  # Formula2or3:  ksi2+q*alpha2+z*chi2  



MSL_V1=-(MSL_M1or23%*%MSL_coef1or23)
MSL_V2= -MSL_V1

MSL_V1e=exp(MSL_V1)
MSL_V2e=exp(MSL_V2)

MSL_V2or3= -(MSL_M2or3%*%MSL_coef2or3)
MSL_MV2or3 = -MSL_V2or3

MSL_V2or3e  = exp(MSL_V2or3)
MSL_MV2or3e = exp(MSL_MV2or3)

#choice probabilities:

MSL_p1=(one/(one+MSL_V1e))	                       #probability of plan
MSL_p2=(one/(one+MSL_V2or3e))*(one/(one+MSL_V2e))   	 #probability of impulse
MSL_p3=(one/(one+MSL_MV2or3e))*(one/(one+MSL_V2e))     #probability of No buy			              	

MSL_P=rbind(t(MSL_p1), t(MSL_p2), t(MSL_p3))			#(NALT x NCS) matrix in which each column has p1, p2 and p3 for each choice situation


MSL_L1=y*MSL_P					#(NALT x NCS) matrix, elemenwise multiplication of prpbability of choice by dummy of chosen alternative


MSL_prob = one3%*%MSL_L1
add=t(rep(1e-16,bootsample))
MSL_prob=MSL_prob+add


MSL_LL1=log(MSL_prob)				#one3%*%P gives a 1 x NCS vector of probability of choice for the chosen alternative in each choice situation and L is log of those probabilities
LLMSL=sum(MSL_LL1)


#################################      VUONG TEST      #################################

#m1 = MSL
#m2 = OSl

df_OSL = 9
df_MSL = 5

LR1 = log(MSL_prob/OSL_prob)
LR2 = sum(LR1)
LR1_w = sd(LR1)

L_dif = LLMSL-LLOSL

LR = L_dif-(((df_MSL-df_OSL)/2)*log(NCS))       #BIC
#LR = L_dif-(df_MSL-df_OSL)                       #AIC
#LR = L_dif                                      #Vuong

w2 = (1/NCS)*sum((log(MSL_prob/OSL_prob))^2)-((1/NCS)*sum(log(MSL_prob/OSL_prob)))^2
w= sqrt(w2)

Vtest = LR/(w*sqrt(NCS))                        

print(Vtest)
print(LR)
print(w)