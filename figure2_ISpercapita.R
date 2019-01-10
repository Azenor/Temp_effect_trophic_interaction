##### Script for manuscript "Temperature modifies
##### consumer-resource interaction strength through
##### its effects on biological rates and body mass"

# This script computes attack rate
# It plots figure 2 :
# Effect of temperature on attack rate (IS per capita)

##### FIGURE 2 || Attack rate - Per capita interaction strength ##
##################################################################

## Sourced scripts

source('func_tempDEP.R')

# temperature
temp_seq = seq(280, 320, length.out = 50)
Topt  =  305

## Body mass ##

# dry mass in mg
bmPd = 1
bmHd = 1 * 10^2
bmCd = 1 * 10^4

# fresh mass in g
bmPg = bmPd/1000 * 6.5
bmHg = bmHd/1000 * 6.5
bmCg = bmCd/1000 * 6.5

## Parameters temperature dependent ##

# Attack rate
par_aPH = c(aPH0 = 3 * 10^8, baPH = 0.25, topt = Topt, E = 0.5, E2 = 1.5)
aPH_seq = rBAU(temp = temp_seq, m = bmHg, par_br = par_aPH)

# at optimal temperature
aPH_topt = rBAU(temp = Topt, m = bmHg, par_br = par_aPH)

### Plot ###
############

## Save as tiff

tiff("./figures/figure2.tiff",  width = 3.3,  height = 3.3,  units  =  'in',  res  =  300)
par(xaxs = 'i', yaxs = 'i', mar = c(2.5,  2,  1,  1))

ylim = c(min(aPH_seq), max(aPH_seq))
rylim = round(ylim, 3)

plot(temp_seq, aPH_seq, lwd = 2, axes = F, ann = F, type = 'n', ylim = rylim)

abline(v = min(temp_seq))
abline(h = min(aPH_seq))

mtext(text = substitute(paste(Search~rate, "  ", italic(a[ij](T)))), side = 2, line = 0.5, font = 1, cex = 1)
mtext(text = 'Temperature', side = 1, line = 1, font = 1, cex = 1)

mtext(expression(T[opt]),  side  =  1,  at  =  Topt, line  =  0.3,  outer  =  FALSE, cex = 0.8)

abline(v = Topt, col = 'red', lwd = 2)

text(Topt+2, 0.12, "PTR", font = 2, cex = 0.8)

lines(temp_seq, aPH_seq, lwd = 2)

dev.off()

## Save as pdf
pdf("./figures/figure2.pdf",  width = 3.3,  height = 3.3)
par(xaxs = 'i', yaxs = 'i', mar = c(2.5,  2,  1,  1))

ylim = c(min(aPH_seq), max(aPH_seq))
rylim = round(ylim, 3)

plot(temp_seq, aPH_seq, lwd = 2, axes = F, ann = F, type = 'n', ylim = rylim)

abline(v = min(temp_seq))
abline(h = min(aPH_seq))

mtext(text = substitute(paste(Search~rate, "  ", italic(a[ij](T)))), side = 2, line = 0.5, font = 1, cex = 1)
mtext(text = 'Temperature', side = 1, line = 1, font = 1, cex = 1)

mtext(expression(T[opt]),  side  =  1,  at  =  Topt, line  =  0.3,  outer  =  FALSE, cex = 0.8)

abline(v = Topt, col = 'red', lwd = 2)

text(Topt+2, 0.12, "PTR", font = 2, cex = 0.8)

lines(temp_seq, aPH_seq, lwd = 2)

dev.off()
