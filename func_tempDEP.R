##### Script for manuscript "Temperature modifies
##### consumer-resource interaction strength through
##### its effects on biological rates and body mass"

## This script contains:
## 2 functions to compute model parameters that are temperature dependent
## 1) rBAU: According to a BA-Johnson Lewin (BAJL) model
## 2) rGauss: According to a gaussian model

## Functions:
## rBAU:
## Input: 3 parameters:
##        temp, vector of temperatures
##        par_br, vector of parameters for the BAJL model
##        uni, by default = 1, stands for a unimodal function (i.e BAJL)
##             if specified = 0, stands for an exponential funtion (i.e BA)
## Output: A vector of biological rate values for a specified temperature gradient

rBAU = function(temp, m, par_br, uni = 1) {
  # m : body-mass
  ## par_br:
  r0 = par_br[[1]]  #scaling coefficient
  b = par_br[[2]]   #allometric scaling coefficient
  tpk = par_br[[3]] #optimal temperature
  E = par_br[[4]]   #activation energy
  Ed = par_br[[5]]  #deactivaiton energy
  k = 8.617*10^-5   #Boltzmann constant
  # uni : specify if unimodal or exponential BA model

  if (uni == 0) {
    l = 1
  } else {
    l = 1/(1 + exp( -1/(k*temp) * (Ed - (Ed/tpk + k*log(E/(Ed - E))) * temp))) #decline phase
  }
  return(BR = r0 * m^b * exp( -E/(k*temp)) * l)
}

## rGauss
## Input: 3 parameters:
##        temp, vector of temperatures
##        par_br, vector of parameters for the Gaussian model
##        humpS, by default =1, stands for a U-shaped Gaussian function
##              if specified !=1, stands for a hump-shaped Gaussian function
## Output: A vector of biological rate values for the specified temperature gradient
rGauss = function(temp, par_br, Ushape = 1){
  r0 = par[[1]] # minimal/maximal trait value
  m = par[[2]]  # body mass
  b = par[[3]]  # allometric scaling coefficient
  s = par[[4]]  # performance breath (width of function)
  tempR = par[[5]] # temperature at which trait is maximal
  if (Ushape == 1) {
    r = r0 * m^b * exp( -(temp - tempR)^2/(2 * s^2))
    return(r)
  }
  else {
    r = r0 * m^b * exp((temp - tempR)^2/(2 * s^2))
    return(r)
  }
}
