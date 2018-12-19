rm(list = ls())
setwd("~/GitHub/Article-projet-master/scripts")

##### Script for manuscript "Effect of temperature on
##### consumer-resource interaction strength"

## This script:
## Contains functions with the model with 2 and 3 trophic levels
## Simulate and plot models dynamics through time at different temperatures

source('parameters.R') ## call for model functions
source('func_equiBiom_logISpop_logISnet.R')

library("deSolve")
library("rootSolve")
#### Linear model

model3 = function(time, state, par) { # dynamical system nutrients-plants
  with(as.list(c(state, par)), {
    # state -> initial conditions (P plant,  N nutrient)
    # parameters -> D = dilution rate
    #               Nin = input nutrient concentration
    #               plant_gr = plant growth function
    #               par_pg = parameters for plant growth function
    dN = D * (Nin - N) - mu * N * P
    dP = q * mu * N * P - D * P - zP * P - aPH * P * H
    dH = ePH * aPH * P * H - D * H - zH * H - aHC * H * C
    dC = eHC * aHC * H * C - zC * C - D * C

    list(c(dN, dP, dH, dC))
  })
}

model2 = function(time, state, par) { # dynamical system nutrients-plants
  with(as.list(c(state, par)), {
    # state -> initial conditions (P plant,  N nutrient)
    # parameters -> D = dilution rate
    #               Nin = input nutrient concentration
    #               plant_gr = plant growth function
    #               par_pg = parameters for plant growth function
    dN = D * (Nin - N) - mu * N * P
    dP = q * mu * N * P - D * P - zP * P - aPH * P * H
    dH = ePH * aPH * P * H - D * H - zH * H

    list(c(dN, dP, dH))
  })
}


##Equilibrium densities to define starting values

equBiom = equiBiom(param)

Time = seq(0, 100, 0.1)


## Simulation ##

## Model 3

par(mfrow = c(2, 3))

for (i in 1:length(temp_seq)) {
  mu = mu_seq[i]
  aPH = aPH_seq[i]
  aHC = aHC_seq[i]
  zP = zP_seq[i]
  zH = zH_seq[i]
  zC = zC_seq[i]


  param3 = c(mu, ks, D, Nin, aPH, ePH, zH, aHC, eHC, zP, zC, q)

  state3 = c(N = equBiom$Nequ3[i] * 0.7, P = equBiom$Pequ3[i] * 0.7, H = equBiom$Hequ3[i] * 0.7, C = equBiom$Cequ3[i] * 0.7)

  sol3 = as.data.frame(ode(state3, Time, model3, param3))

  plot(Time, sol3$P, type = 'l', xlab = "Time", ylab = "Biomass", col = "green", ylim = c(0, max(c(sol3$N, sol3$P, sol3$H, sol3$C))),
       main = c(paste("t  =  ",  round(temp_seq[i], 1), "K",  sep = "")))
  lines(Time, sol3$N, type = 'l', col = "blue", lty = 2)
  lines(Time, sol3$H, type = 'l')
  lines(Time, sol3$C, type = 'l', col = "red")
}

# Model 2

par(mfrow = c(2, 3))


for (i in 1:length(temp_seq)) {
  mu = mu_seq[i]
  aPH = aPH_seq[i]
  zP = zP_seq[i]
  zH = zH_seq[i]


  param2 = c(mu, ks, D, Nin, aPH, ePH, zH, zP, q)

  state2 = c(N = equBiom$Nequ2[i] * 0.7, P = equBiom$Pequ2[i] * 0.7, H = equBiom$Hequ2[i] * 0.7)

  sol2 = as.data.frame(ode(state2, Time, model2, param2))

  plot(Time, sol2$P, type = 'l', xlab = "Time", ylab = "Biomass", col = "green", ylim = c(0, max(c(sol2$N, sol2$P, sol2$H, sol2$C))),
       main = c(paste("t  =  ",  round(temp_seq[i], 1), "K",  sep = "")))
  lines(Time, sol2$N, type = 'l', col = "blue", lty = 2)
  lines(Time, sol2$H, type = 'l')
}
