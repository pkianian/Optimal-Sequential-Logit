library(foreign)  #Load package that reads .csv files
library(nonnest2) #load package for vuong test

 d <- read.csv("C:/Users/pkianian/Dropbox/Pari Dissertation/Dissertation/Essay 2/Second Essay Data/November 2021 Data/Real data final corrected Feb 26.csv", header=TRUE, sep=",", fill=TRUE)


attach(d)

start=Sys.time()  # start time

# Number of alternatives in each choice situation

NALT=3 

# This is the number of rows of data in XMAT below.

NROWS= nrow(d)

# Number of choice situations in dataset.

NCS= NROWS/3


XMAT=cbind(idcase, 
           idalt, 
           depvar, 
           
           Age, 
           household_size, 
           Preplan_shopping, 
           Sex,
           
           Sex,
           Trait_impulsiveness, 
           Hedonic, 
           Ready_To_Use, 
           Sale_proneness, 
           Unhealthiness_Index, 
           Value_consciousness, 
           Variety_seeking, 
           household_size,
           
           Sale, 
           Display, 
           New_Product, 
           price_difference_ratio)   #Matrix including all variables in the dataset



bootsample = 6187                 #Bootstrapping sample size
NROWS2 = bootsample*NALT

one3=c(1, 1, 1)


x=XMAT[seq(1,nrow(XMAT),3),4:7]		  #a (bootsample*NALT)x column vector
q=XMAT[seq(1,nrow(XMAT),3),8:16]			#a (bootsample*NALT)x column vector
z=XMAT[seq(1,nrow(XMAT),3),17:20]			#a (bootsample*NALT)x column vector
y=XMAT[,3]			    #a (bootsample*NALT)x column vector of 0 and 1

dim(y)=c(NALT,bootsample)	#restructure y  so there are NALT rows and bootsample cols

minus_x = x*-1		#vector of -x for each choice situation
minus_q = q*-1		#vector of -q for each choice situation
minus_z = z*-1		#vector of -z for each choice situation

one=rep(1,bootsample)	    		#a column vector of ones with bootsample rows
minus_one=rep(-1,bootsample)	#a column vector of -1 with bootsample rows



###########################################  OSL  ########################################

OSL_M2or3=cbind(one,minus_q,q,minus_z)    # Formula2or3: ksi23-q*alpha2+q*alpha3-z*chi2
OSL_M23or1=cbind(one,q,minus_x)           # Formula23or1(1):  ksi2+q*alpha2-x*beta1


paramosl = c(92.43,
             93.09,
             74.87,
             -23.21,
             
             
             1.06,
             0.27,
             3.10,
             -30.86,
             
             -108.51,
             3.52,
             134.37,
             50.18,
             -4.29,
             -0.89,
             6.44,
             -1.60,
             12.16,
             
             -0.01,
             35.40,
             -14.76,
             0.46,
             -2.42,
             1.06,
             2.23,
             
             -190.36,
             -52.59,
             449.97,
             
             0.008,
             0.007)

#choice parameters:
#parameters include: chi1, beta3, alpha1, alpha2, gamma1, gamma3, miu1, omega, c_phi
#B=(chi2(4), beta1(4), alpha2(9), alpha3(7), ksi2, ksi3, ksi23) initial value of parameters
# chi2 = B[1:4]
# beta1 = B[5:8]
# alpha2 = B[9:17]
# alpha3 = B[18:24]

# ksi2 = B[25]
# ksi3 = B[26]
# ksi23 = B[27]

#theta=B[28]
#kappa=B[29]

OSL_coef2or3=c(paramosl[27],paramosl[9:17],0,paramosl[18:24],0,paramosl[1:4])  	# Formula2or3: ksi23-q*alpha2+q*alpha3-z*chi2
    
OSL_coef231= c(paramosl[25],paramosl[9:17],paramosl[5:8])                     # Formula231:  ksi2+q*alpha2-x*beta1
	              
OSL_coef232= c(paramosl[26],0,paramosl[18:24],0,paramosl[5:8])                # Formula1232:  ksi3+q*alpha3-x*beta1
               

OSL_V1=(OSL_M2or3%*%OSL_coef2or3)*paramosl[28]
OSL_V2= -OSL_V1
OSL_V1=exp(OSL_V1)
OSL_V2=exp(OSL_V2)

OSL_VV1=OSL_M23or1%*%OSL_coef231
OSL_VV2=OSL_M23or1%*%OSL_coef232
OSL_VV1=exp(OSL_VV1)
OSL_VV2=exp(OSL_VV2)

OSL_V3=(OSL_VV1+OSL_VV2)**(-paramosl[29])
OSL_V4=(OSL_VV1+OSL_VV2)**(paramosl[29])

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
#B=(chi2(4), beta1(4), alpha2(9), ksi1, ksi2) initial value of parameters (7 parameters)
# chi2 = B[1:4]
# beta1 = B[5:8]
# alpha2 = B[9:17]
# ksi1 = B[18]
# ksi2 = B[19]


parammsl = c(0.33,
             0.14,
             0.10,
             -0.13,
             
             87.66,
             7.52,
             15.66,
             1.80,
             
             0.70,
             27.26,
             0.39,
             0.46,
             16.61,
             0.06,
             31.27,
             36.32,
             4.15,
             
             2.10,
             0.93) 

MSL_coef1or23=c(parammsl[18],parammsl[5:8])                              # Formula1or23: ksi1+x*beta1
MSL_coef2or3=c(parammsl[19],parammsl[9:17],parammsl[1:4])		             # Formula2or3:  ksi2+q*alpha2+z*chi2  



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

df_OSL = 29
df_MSL = 19
ncs = NCS

LR1 = log(MSL_prob/OSL_prob)
LR2 = sum(LR1)
L_dif = LLMSL-LLOSL

#LR = L_dif-(((df_MSL-df_OSL)/2)*log(NCS))       #BIC
#LR = 2*L_dif-2*(df_MSL-df_OSL)                  #AIC
#LR = L_dif-(((df_MSL-df_OSL)/2)*0)              #Vuong
#LR = L_dif                                      #Vuong

w2 = (1/ncs)*sum((log(MSL_prob/OSL_prob))^2)-((1/ncs)*sum(log(MSL_prob/OSL_prob)))^2
w= sqrt(w2)

Vtest = LR/(w*sqrt(NCS))

print(Vtest)
print(LR)
print(w)