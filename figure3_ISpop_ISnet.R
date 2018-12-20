##### Script for manuscript "Temperature modifies
##### consumer-resource interaction strength through
##### its effects on biological rates and body mass"

# This script computes interaction strength per population and net
# when all parameters vary with temperature and when parameters are alternately fixed
# It then plots interaction strength measures according to temperature
# Figure 3 : effect of temperature on interaction strength measures
# for varying temperature dependence of parameters

##### FIGURE 3 || Per population and net interaction strength ##
###############################################################

## Packages
library(fields)

## Sourced scripts

source('parameters.R') # Model parameters
source('func_equiBiom_logISpop_logISnet.R') # Function to compute log(interaction strength)

#### ALL PARAMETERS VARY WITH TEMPERATURE ####
##############################################

## Biomass densities at equilibrium

resEquiBiom = equiBiom(param)

## Interaction strength per population

reslogISpop = logISpop(resEquiBiom, aPH_seq, aHC_seq)

logISpop_H2 = reslogISpop$ISpopH2
logISpop_H3 = reslogISpop$ISpopH3
logISpop_C3 = reslogISpop$ISpopC3

## Net interaction strength

reslogISnet = logISnet(resEquiBiom)

logISnet_HP = reslogISnet$ISnetHP
logISnet_CP = reslogISnet$ISnetCP
logISnet_CH = reslogISnet$ISnetCH

### Plot ###

### IS per population

par(mfrow = c(1, 3))
plot(temp_seq, logISpop_H2, type = 'l', xlab = '', ylab = 'Interaction strength per population', main = 'H2-P2')
plot(temp_seq, logISpop_H3, type = 'l', xlab = '', ylab = '', main = 'H3-P3')
plot(temp_seq, logISpop_C3, type = 'l', xlab = '', ylab = '', main = 'C-H')


# Net IS

par(mfrow = c(1, 3))
plot(temp_seq, logISnet_HP, type = 'l', xlab = 'Temperature', ylab = 'Net interaction strength', main = 'H-P')
plot(temp_seq, logISnet_CP, type = 'l', xlab = 'Temperature', ylab = '', main = 'C-P')
plot(temp_seq, logISnet_CH, type = 'l', xlab = 'Temperature', ylab = '', main = 'C-H')

#### PARAMETERS ARE ALTERNATELY FIXED ####
##########################################

##### PLOTS ####

## Function to compute interaction strengths and plot the results

plotISparF = function(paramF, legendFixParam) {

  resEquiBiomF = equiBiom(paramF) # equilibrium biomass densities

  ## Interaction strength per population

  reslogISpopF = logISpop(resEquiBiomF, paramF$aPH, paramF$aHC)

  ## Net interaction strength

  reslogISnetF = logISnet(resEquiBiomF)

  ## Check if interaction strength measure is temperature independent,
  ## if yes,  replicate the value to plot the result along the temperature gradient

  for(i in 1:length(reslogISpopF)){
    if(length(reslogISpopF[[i]])  ==  1){
      reslogISpopF[[i]] = rep(reslogISpopF[[i]], length(temp_seq))
    }
  }

  for(i in 1:length(reslogISnetF)){
    if(length(reslogISnetF[[i]])  ==  1){
      reslogISnetF[[i]] = rep(reslogISnetF[[i]], length(temp_seq))
    }
  }

  # Divide list for plots
  logISpop_H2_F = reslogISpopF$ISpopH2
  logISpop_H3_F = reslogISpopF$ISpopH3
  logISpop_C3_F = reslogISpopF$ISpopC3

  logISnet_HP_F = reslogISnetF$ISnetHP
  logISnet_CP_F = reslogISnetF$ISnetCP
  logISnet_CH_F = reslogISnetF$ISnetCH

  ### Plots

  ### IS per population

  par(mfrow = c(2, 3))
  plot(temp_seq, logISpop_H2_F, type = 'l', xlab = '', ylab = 'Interaction strength per population', main = 'H2-P2')
  plot(temp_seq, logISpop_H3_F, type = 'l', xlab = '', ylab = '', main = 'H3-P3')
  plot(temp_seq, logISpop_C3_F, type = 'l', xlab = '', ylab = '', main = 'C-H')

  # Net IS

  plot(temp_seq, logISnet_HP_F, type = 'l', xlab = 'Temperature', ylab = 'Net interaction strength', main = 'H-P')
  plot(temp_seq, logISnet_CP_F, type = 'l', xlab = 'Temperature', ylab = '', main = 'C-P')
  plot(temp_seq, logISnet_CH_F, type = 'l', xlab = 'Temperature', ylab = '', main = 'C-H')
  legend("bottomright", legend = substitute(paste('Fix:',  legendFixParam)))

  return(IS = append(reslogISpopF, reslogISnetF))

}

## Growth rate varies

param_mu = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_seq, zP = zP_opt, zH = zH_opt, aPH = aPH_opt, zC = zC_opt, aHC = aHC_opt)

ISmu = plotISparF(param_mu, GrowthRate)


## PP mortality rate varies

param_zPP = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
               mu = mu_opt, zP = zP_seq, zH = zH_opt, aPH = aPH_opt, zC = zC_opt, aHC = aHC_opt)

ISzP = plotISparF(param_zPP, PP_MortalityRate)

## H mortality rate varies

param_zH = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_opt, zP = zP_opt, zH = zH_seq, aPH = aPH_opt, zC = zC_opt, aHC = aHC_opt)

ISzH = plotISparF(param_zH, H_MortalityRate)

## C mortality rate varies

param_zC = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_opt, zP = zP_opt, zH = zH_opt, aPH = aPH_opt, zC = zC_seq, aHC = aHC_opt)

ISzC = plotISparF(param_zC, C_MortalityRate)

## H search rate varies

param_aPH = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
               mu = mu_opt, zP = zP_opt, zH = zH_opt, aPH = aPH_seq, zC = zC_opt, aHC = aHC_opt)

ISaPH = plotISparF(param_aPH, H_SearchRate)


## C search rate varies

param_aHC = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
               mu = mu_opt, zP = zP_opt, zH = zH_opt, aPH = aPH_opt, zC = zC_opt, aHC = aHC_seq)

ISaHC = plotISparF(param_aHC, C_SearchRate)


## Mortality rates (P,  H,  C) vary

param_z = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC
             , mu = mu_opt, zP = zP_seq, zH = zH_seq, aPH = aPH_opt, zC = zC_seq, aHC = aHC_opt)

ISz = plotISparF(param_z, MortalityRate)

## Search rates (H,  C) vary

param_a = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
             mu = mu_opt, zP = zP_opt, zH = zH_opt, aPH = aPH_seq, zC = zC_opt, aHC = aHC_seq)

ISa = plotISparF(param_a, AttackRate)

## Primary producer parameters vary

param_PP = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_seq, zP = zP_seq, zH = zH_opt, aPH = aPH_opt, zC = zC_opt, aHC = aHC_opt)

ISP = plotISparF(param_PP, PP)

## Herbivore parameters vary

param_H = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
             mu = mu_opt, zP = zP_opt, zH = zH_seq, aPH = aPH_seq, zC = zC_opt, aHC = aHC_opt)

ISH = plotISparF(param_H, H)

## Carnivore parameters vary

param_C = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
             mu = mu_opt, zP = zP_opt, zH = zH_opt, aPH = aPH_opt, zC = zC_seq, aHC = aHC_seq)

ISC = plotISparF(param_C, C)

## Herbivore and carnivore parameters vary

param_HC = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_opt, zP = zP_opt, zH = zH_seq, aPH = aPH_seq, zC = zC_seq, aHC = aHC_seq)

ISHC = plotISparF(param_HC, HC)

## Primary producers and herbivore parameters vary

param_PH = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_seq, zP = zP_seq, zH = zH_seq, aPH = aPH_seq, zC = zC_opt, aHC = aHC_opt)

ISPH = plotISparF(param_PH, PH)

## Primary producers and carnivore parameters vary

param_PC = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
              mu = mu_seq, zP = zP_seq, zH = zH_opt, aPH = aPH_opt, zC = zC_seq, aHC = aHC_seq)

ISPC = plotISparF(param_PC, PC)

## Shape of the thermal response ##
# 1 (blue)  =  hump-shaped,  2 (deepskyblue2)  =  U-shaped,
# 3 (darkolivegreen3)  =  linear decrease,  4 (grey)  =  no variation


matShape = matrix(c(1, 1, 1, 1, 1,
                  3, 3, 3, 3, 3,
                  3, 3, 3, 3, 3,
                  4, 3, 4, 3, 3,
                  3, 3, 3, 3, 3,
                  1, 1, 1, 2, 1,
                  4, 1, 4, 1, 1,
                  1, 1, 1, 1, 1,
                  1, 3, 3, 1, 3,
                  1, 1, 1, 2, 1,
                  4, 1, 4, 1, 1,
                  1, 1, 1, 1, 1,
                  1, 1, 1, 3, 1,
                  1, 1, 3, 1, 1,
                  1, 1, 1, 1, 1), ncol = 5, byrow = T)
matShape2 = t(apply(matShape, 2, rev))


##### COMPUTE SD #####
IStot = c(reslogISpop, reslogISnet) # when no parameters are fixed

sdIStot = sapply(IStot, sd)
sdISmu = sapply(ISmu, sd)
sdISzP = sapply(ISzP, sd)
sdISzH = sapply(ISzH, sd)
sdISzC = sapply(ISzC, sd)
sdISaPH = sapply(ISaPH, sd)
sdISaHC = sapply(ISaHC, sd)
sdISz = sapply(ISz, sd)
sdISa = sapply(ISa, sd)
sdISP = sapply(ISP, sd)
sdISH = sapply(ISH, sd)
sdISC = sapply(ISC, sd)
sdISHC = sapply(ISHC, sd)
sdISPH = sapply(ISPH, sd)
sdISPC = sapply(ISPC, sd)



dataSD = data.frame(sdISmu, sdISzP, sdISzH, sdISzC, sdISz, sdISaPH, sdISaHC, sdISa, sdISP,
                  sdISH, sdISC, sdISHC, sdISPH, sdISPC, sdIStot)
matdataSD = as.matrix(dataSD)

matdataSD = matdataSD[-2, ]
matdataSD2 = t(apply(matdataSD, 1, rev))

pdf("./figures/figure3.pdf", width = 7, height = 4)

par(mfrow = c(1, 2),  mar = c(2, 2, 4.1, 2.1),  oma = c(0, 2, 0, 2), mgp  =  c(1.5,  0.6,  0),  tck  =  -.015,  family  =  'sans')

image(matShape2, col = c("blue", "deepskyblue2", "darkgreen", "darkolivegreen3", "grey"), axes = F)
mtext(text = "Parameters temperature dependent", side = 2, line = 2, font = 1, cex = 1)
mtext(expression(bold((A))),  side  =  3,  at = 0.5, line  =  2.5,  outer  =  FALSE, cex = 1)


axis(3, at = seq(0, 1, length.out  =  5),  cex.axis  =  0.6,  labels  =  c("H-P", "C-H",
                                         "H-P", "C-H", "C-P"),  font  =  1)
axis(2, at = seq(0, 1, length.out  =  15),  cex.axis  =  0.6,  labels  =  c(expression(PHC), expression(PC), expression(PH), expression(HC), expression(C), expression(H), expression(P),
                                          expression(a), expression(a[HC]), expression(a[PH]), expression(z),
                                        expression(z[C]), expression(z[H]), expression(z[P]), expression(mu)), las = 2)

mtext(expression(IS[pop]),  side  =  3,  at  =  0.15,  line  =  1.6,  outer  =  FALSE,  cex  =  1)
mtext(expression(IS[net]),  side  =  3,  at  =  0.75,  line  =  1.6,  outer  =  FALSE,  cex  =  1)

abline(v  =  0.375,  lty  =  2,  lwd  =  2)

colPlot = rev(heat.colors(15))

image.plot(matdataSD2, axes  =  F, col  =  colPlot)
axis(3, at  =  seq(0, 1, length.out  =  5), cex.axis  =  0.6, labels  =  c("H-P", "C-H",
                                         "H-P", "C-H", "C-P"), font  =  1)
mtext(expression(bold((B))),  side  =  3,  at  =  0.5, line  =  2.5,  outer  =  FALSE, cex = 1)
mtext(expression(IS[pop]),  side  =  3,  at  =  0.15, line  =  1.6,  outer  =  FALSE, cex = 1)
mtext(expression(IS[net]),  side  =  3,  at  =  0.75, line  =  1.6,  outer  =  FALSE, cex = 1)
abline(v  =  0.375,  lty  =  2,  lwd  =  2)

dev.off()

### Figure for legend
par(mfrow = c(1, 1))
pdf("./figures/legend_figure3.pdf",  width = 3.3,  height = 3.3)

par(xaxs = 'i', yaxs = 'i', mar = c(2.5,  2,  1,  1))

ylim = c(min(logISpop_H2), max(logISpop_H2))

Tp  =  temp_seq[which(logISpop_H2  =  =  max(logISpop_H2))]

rylim = round(ylim, 3)

plot(temp_seq, logISpop_H2, lwd = 2, axes = F, ann = F, type = 'n', ylim = rylim)

mtext(expression(T["+"]),  side  =  1,  at  =  Tp-1,  line  =  1.5,  outer  =  FALSE,  cex  =  4)

abline(v = Tp, lwd = 4)
lines(temp_seq, logISpop_H2, lwd = 5)

dev.off()
