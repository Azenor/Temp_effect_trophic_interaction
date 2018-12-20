##### Script for manuscript "Temperature modifies
##### consumer-resource interaction strength through
##### its effects on biological rates and body mass"

## This script:
## Gives the model parameter values
## It uses the functions from the scripts func_tempDEP and func_TSR
## P stands for primary producers
## H stands for herbivores
## C stands for carnivores

source('func_tempDEP.R')
source('func_TSR.R')

# Temperature #
temp_seq = seq(285,315,length.out=50)
topt = 298 # optimal temperature

# Body mass #

# dry mass in mg
bmPd = 1        # dry body mass of primary producers
bmHd = 1*10^2   # dry body mass of herbivores
bmCd = 1*10^4   # dry body mass of carnivores

# fresh mass in g
bmPg = bmPd/1000*6.5
bmHg = bmHd/1000*6.5
bmCg = bmCd/1000*6.5

## Parameters temperature dependent ##

# Attack rates #
par_aPH = c(aPH0 = 5*10^13, baPH = 0.05, topt = topt, E = 0.8, E2 = 1.5)
aPH_seq = rBAU(temp = temp_seq, m = bmHg, par_br = par_aPH)
#plot(temp_seq,aPH_seq,type="l")

par_aHC = c(aHC0 = 3*10^12, baHC = 0.05, topt = topt, E = 0.74, E2 = 1.15)
aHC_seq = rBAU(temp = temp_seq, m = bmCg, par_br = par_aHC)
#plot(temp_seq,aHC_seq,type="l")

# Mortality rates #
par_zP = c(zP0 = 5*10^7, bzP = -0.25, topt = topt, E = 0.55, E2 = 1.15)
zP_seq = rBAU(temp = temp_seq, m = bmPg, par_br = par_zP, uni = 0)
# plot(temp_seq,zP_seq,type="l")

par_zH = c(zH0 = 2*10^6, bzH = -0.25, topt = topt, E = 0.43, E2 = 1.15)
zH_seq = rBAU(temp = temp_seq, m = bmHg, par_br = par_zH, uni = 0)
# plot(temp_seq,zH_seq,type="l")

par_zC = c(zC0 = 3*10^11, bzC = -0.25, topt = topt, E = 0.72, E2 = 1.15)
zC_seq = rBAU(temp = temp_seq, m = bmCg, par_br = par_zC, uni = 0)
# plot(temp_seq,zC_seq,type="l")

# Growth rate #
par_mu_max = c(mu0 = 3*10^8, bmu = -0.25, topt = topt, E = 0.53, E2 = 1.15)
mu_seq = rBAU(temp = temp_seq, m = bmPg, par_br = par_mu_max)
# plot(temp_seq,mu_seq,type="l")

## Parameters temperature independent ##

D = 5     # dilution rate
Nin = 15  # nutrient input concentrations
q = 6     # C:N ratio
ks = 0.8  # half saturation constant
mP = 0.3  # mortality rate for P
mH = 0.3  # mortality rate for H
mC = 0.3  # mortality rate for C

# Conversion efficiencies
eHC = 0.85 # herbivore-carnivore
ePH = 0.45 # primary producer-herbivore

# List of parameters

param =  list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_seq, zP = zP_seq, zH = zH_seq, aPH = aPH_seq,
              zC = zC_seq, aHC = aHC_seq, mP = mP, mH = mH, mC = mC)


## Parameters at optimal temperature ##

# Attack rates
aPH_opt = rBAU(temp = topt, m = bmHg, par_br = par_aPH)
aHC_opt = rBAU(temp = topt, m = bmCg, par_br = par_aHC)

# Mortality rates
zP_opt = rBAU(temp = topt, m = bmPg, par_br = par_zP, uni = 0)
zH_opt = rBAU(temp = topt, m = bmHg, par_br = par_zH, uni = 0)
zC_opt = rBAU(temp = topt, m = bmCg, par_br = par_zC, uni = 0)

# Growth rate
mu_opt = rBAU(temp = topt, m = bmPg, par_br = par_mu_max)

## Temperature size rule ##

# Body mass #

mPtsr = sapply(temp_seq, TSRaqua, DM20 = bmPd)
mHtsr = sapply(temp_seq, TSRaqua, DM20 = bmHd)
mCtsr = sapply(temp_seq, TSRaqua, DM20 = bmCd)

mPtsrMax = sapply(temp_seq, TSRmax, DM20 = bmPd)
mHtsrMax = sapply(temp_seq, TSRmax, DM20 = bmHd)
mCtsrMax = sapply(temp_seq, TSRmax, DM20 = bmCd)

# Attack rates
aPH_optTSR = rBAU(temp = topt, m = mHtsr, par_br = par_aPH)
aHC_optTSR = rBAU(temp = topt, m = mCtsr, par_br = par_aHC)

# Mortality rates
zP_optTSR = rBAU(temp = topt, m = mPtsr, par_br = par_zP, uni = 0)
zH_optTSR = rBAU(temp = topt, m = mHtsr, par_br = par_zH, uni = 0)
zC_optTSR = rBAU(temp = topt, m = mCtsr, par_br = par_zC, uni = 0)

# Growth rate
mu_optTSR = rBAU(temp = topt, m = mPtsr, par_br = par_mu_max)

### Parameters with TSR

## Attack rates
aPH_seqTSR=rBAU(temp=temp_seq,mHtsr,par_br=par_aPH)
aHC_seqTSR=rBAU(temp=temp_seq,mCtsr,par_br=par_aHC)

## Mortality rates

# PP
zP_seqTSR=rBAU(temp=temp_seq,mPtsr,par_br=par_zP,uni=0)

# Herbivore
zH_seqTSR=rBAU(temp=temp_seq,mHtsr,par_br=par_zH,uni=0)

# Carnivore
zC_seqTSR=rBAU(temp=temp_seq,mCtsr,par_br=par_zC,uni=0)

## Growth rate
mu_seqTSR=rBAU(temp=temp_seq,mPtsr,par_br=par_mu_max)
