##### Script for manuscript "Temperature modifies
##### consumer-resource interaction strength through
##### its effects on biological rates and body mass"

# This script computes interaction strength per population and net
# when temperature independent mortality rates vary
# It plots figure 4 and figure S3
# - Figure 4 : interaction strength measures according to temperature
# for different values of temperature independent mortality rate
# - Figure S3 : equilibrium biomasses according to temperature
# for different values of temperature independent mortality rate

##### FIGURE 4 || Temperature independent mortality rates ##
############################################################

## Sourced scripts

source('parameters.R') # Model parameters
source('func_equiBiom_logISpop_logISnet.R') # Function to compute log(interaction strength)

## vector of temperature independent mortality rates
mseq = seq(0.2, 2.8, length.out = 5)

## initialize list for IS values
logISpop_H2 = list()
logISpop_H3 = list()
logISpop_C3 = list()

logISnet_HP = list()
logISnet_CP = list()
logISnet_CH = list()
resEquiBiom = list()

## Compute IS values

for (i in 1:length(mseq)) {

  param = list(d = D, Nin = Nin, q = q, effPH = ePH, effHC = eHC,
         mu = mu_seq, zP = zP_seq, zH = zH_seq, aPH = aPH_seq, zC = zC_seq, aHC = aHC_seq, mP = mseq[i],
         mH = mseq[i], mC = mseq[i])

  ## Equilibrium biomass
  resEquiBiom[[i]] = equiBiom(param)

  ## Per population interaction strength
  reslogISpop = logISpop(resEquiBiom[[i]], aPH_seq, aHC_seq)

  logISpop_H2[[i]] = reslogISpop$ISpopH2
  logISpop_H3[[i]] = reslogISpop$ISpopH3
  logISpop_C3[[i]] = reslogISpop$ISpopC3

  ## Net interaction strength

  reslogISnet = logISnet(resEquiBiom[[i]])

  logISnet_HP[[i]] = reslogISnet$ISnetHP
  logISnet_CP[[i]] = reslogISnet$ISnetCP
  logISnet_CH[[i]] = reslogISnet$ISnetCH

}

## PLOTS

# color gradient
colfunc = colorRampPalette(c("gray73", "black"))
colo = colfunc(length(mseq))

plot(temp_seq, logISpop_H2[[1]], type = "l", ylim = c(min(unlist(logISpop_H2)), max(unlist(logISpop_H2))))

for (i in 1:length(mseq)){
  lines(temp_seq, logISpop_H2[[i]], col = colo[i])
}

plot(temp_seq, logISpop_H3[[1]], type = "l", ylim = c(min(unlist(logISpop_H3)), max(unlist(logISpop_H3))))

for (i in 1:length(mseq)){
  lines(temp_seq, logISpop_H3[[i]], col = colo[i])
}

plot(temp_seq, logISpop_C3[[1]], type = "l", ylim = c(min(unlist(logISpop_C3)), max(unlist(logISpop_C3))))

for (i in 1:length(mseq)){
  lines(temp_seq, logISpop_C3[[i]], col = colo[i])
}

plot(temp_seq, logISnet_HP[[1]], type = "l", ylim = c(min(unlist(logISnet_HP)), max(unlist(logISnet_HP))))

for (i in 1:length(mseq)){
  lines(temp_seq, logISnet_HP[[i]], col = colo[i])
}

plot(temp_seq, logISnet_CP[[1]], type = "l", ylim = c(min(unlist(logISnet_CP)), max(unlist(logISnet_CP))))

for (i in 1:length(mseq)){
  lines(temp_seq, logISnet_CP[[i]], col = colo[i])
}

plot(temp_seq, logISnet_CH[[1]], type = "l", ylim = c(min(unlist(logISnet_CH)), max(unlist(logISnet_CH))))

for (i in 1:length(mseq)){
  lines(temp_seq, logISnet_CH[[i]], col = colo[i])
}


### Plot for manuscript

## save as tiff

tiff("./figures/figure4.tiff",  width = 7,  height = 4,  units  =  'in',  res  =  300)

#par(xaxs = 'i', yaxs = 'i', mar = c(2.5,  2,  1,  1))
layout(matrix(1:3, ncol = 3),  width  =  c(3, 3, 1), height  =  c(1, 1, 1))

### Plot IS pop
ylimC3pop = c(min(unlist(logISpop_C3)), max(unlist(logISpop_C3))+0.1)

rylimC3pop = round(ylimC3pop, 1)

plot(temp_seq, logISpop_C3[[1]], ylim = rylimC3pop,
     ann = FALSE, axes = F, type = 'n', xaxs = 'i', yaxs = 'i')

axis(2, tck  =  -0.015, at = rylimC3pop,
     labels = rylimC3pop, mgp = c(3, 0.5, 0), las = 2, cex.axis = 1.5)
axis(1, tck  =  -0.015, at = seq(min(temp_seq), max(temp_seq), 10), labels = seq(min(temp_seq), max(temp_seq), 10)
     , mgp = c(3, 0.8, 0), cex.axis = 1.5)

for (i in 1:length(mseq)){
  lines(temp_seq, logISpop_C3[[i]], col = colo[i], lwd = 2)
}

mtext(text = expression(log(IS[pop[C-H]])), side = 2, line = 1, font = 1, cex = 1)
mtext(text = 'Temperature', side = 1, line = 3, font = 1, cex = 1)
mtext(expression(bold((A))),  side  =  3,  at = 298, line  =  0,  outer  =  FALSE, cex = 1)

### Plot ISnet

ylimC3net = c(min(unlist(logISnet_CH)), max(unlist(logISnet_CH))+0.1)
rylimC3net = round(ylimC3net, 1)

plot(temp_seq, logISnet_CH[[1]], ylim = rylimC3net,
     ann = FALSE, axes = F, type = 'n', xaxs = 'i', yaxs = 'i')

axis(2, tck  =  -0.015, at = rylimC3net,
     labels = rylimC3net, mgp = c(3, 0.5, 0), las = 2, cex.axis = 1.5)
axis(1, tck  =  -0.015, at = seq(min(temp_seq), max(temp_seq), 10), labels = seq(min(temp_seq), max(temp_seq), 10)
     , mgp = c(3, 0.8, 0), cex.axis = 1.5)

for (i in 1:length(mseq)){
  lines(temp_seq, logISnet_CH[[i]], col = colo[i], lwd = 2)
}

mtext(text = expression(log(IS[net[C-H]])), side = 2, line = 1, font = 1, cex = 1)
mtext(text = 'Temperature', side = 1, line = 3, font = 1, cex = 1)
mtext(expression(bold((B))),  side  =  3,  at = 298, line  =  0,  outer  =  FALSE, cex = 1)

# dilution rate values
par(mar = c(2, 0.4, 2, 0))
legend_image <- as.raster(matrix(colfunc(5),  ncol = 1))
plot(c(0, 2), c(0, 1), type  =  'n',  axes  =  F, xlab  =  '',  ylab  =  '')

text(x = 0.9,  y  =  c(0.12, 0.89),  labels  = c(expression(0.2), expression(2.8)), cex = 1.5)

mtext(expression(b),  side  =  3,  at = 0.25, line  =  -2.5,  outer  =  FALSE, cex = 1.5)

rasterImage(rev(legend_image),  0,  0.1,  0.5, 0.9)

dev.off()

## save as pdf

pdf("./figures/figure4.pdf",  width = 7,  height = 4)

#par(xaxs = 'i', yaxs = 'i', mar = c(2.5,  2,  1,  1))
layout(matrix(1:3, ncol = 3),  width  =  c(3, 3, 1), height  =  c(1, 1, 1))

### Plot IS pop
ylimC3pop = c(min(unlist(logISpop_C3)), max(unlist(logISpop_C3))+0.1)

rylimC3pop = round(ylimC3pop, 1)

plot(temp_seq, logISpop_C3[[1]], ylim = rylimC3pop,
     ann = FALSE, axes = F, type = 'n', xaxs = 'i', yaxs = 'i')

axis(2, tck  =  -0.015, at = rylimC3pop,
     labels = rylimC3pop, mgp = c(3, 0.5, 0), las = 2, cex.axis = 1.5)
axis(1, tck  =  -0.015, at = seq(min(temp_seq), max(temp_seq), 10), labels = seq(min(temp_seq), max(temp_seq), 10)
     , mgp = c(3, 0.8, 0), cex.axis = 1.5)

for (i in 1:length(mseq)){
  lines(temp_seq, logISpop_C3[[i]], col = colo[i], lwd = 2)
}

mtext(text = expression(log(IS[pop[C-H]])), side = 2, line = 1, font = 1, cex = 1)
mtext(text = 'Temperature', side = 1, line = 3, font = 1, cex = 1)
mtext(expression(bold((A))),  side  =  3,  at = 298, line  =  0,  outer  =  FALSE, cex = 1)

### Plot ISnet

ylimC3net = c(min(unlist(logISnet_CH)), max(unlist(logISnet_CH))+0.1)
rylimC3net = round(ylimC3net, 1)

plot(temp_seq, logISnet_CH[[1]], ylim = rylimC3net,
     ann = FALSE, axes = F, type = 'n', xaxs = 'i', yaxs = 'i')

axis(2, tck  =  -0.015, at = rylimC3net,
     labels = rylimC3net, mgp = c(3, 0.5, 0), las = 2, cex.axis = 1.5)
axis(1, tck  =  -0.015, at = seq(min(temp_seq), max(temp_seq), 10), labels = seq(min(temp_seq), max(temp_seq), 10)
     , mgp = c(3, 0.8, 0), cex.axis = 1.5)

for (i in 1:length(mseq)){
  lines(temp_seq, logISnet_CH[[i]], col = colo[i], lwd = 2)
}

mtext(text = expression(log(IS[net[C-H]])), side = 2, line = 1, font = 1, cex = 1)
mtext(text = 'Temperature', side = 1, line = 3, font = 1, cex = 1)
mtext(expression(bold((B))),  side  =  3,  at = 298, line  =  0,  outer  =  FALSE, cex = 1)

# dilution rate values
par(mar = c(2, 0.4, 2, 0))
legend_image <- as.raster(matrix(colfunc(5),  ncol = 1))
plot(c(0, 2), c(0, 1), type  =  'n',  axes  =  F, xlab  =  '',  ylab  =  '')

text(x = 0.9,  y  =  c(0.12, 0.89),  labels  = c(expression(0.2), expression(2.8)), cex = 1.5)

mtext(expression(b),  side  =  3,  at = 0.25, line  =  -2.5,  outer  =  FALSE, cex = 1.5)

rasterImage(rev(legend_image),  0,  0.1,  0.5, 0.9)

dev.off()

###### FIGURE 3 for Supplementary materials ######
##################################################

#### Plot equilibrium biomasses ####

# Get the minimum and maximum for each plot

yLim  =  data.frame(name  =  names(resEquiBiom[[1]]),
  min  =  numeric(length(resEquiBiom[[1]])),
  max  =  numeric(length(resEquiBiom[[1]])))

for(j in 1:length(resEquiBiom[[1]])){
  prevMin  =  +Inf
  prevMax  =  -Inf
  for(i in 1:length(resEquiBiom))
  {
      currentMin  =  min(unlist(resEquiBiom[[i]][j]))
      currentMax  =  max(unlist(resEquiBiom[[i]][j]))
      if(currentMin < prevMin)
        prevMin  =  currentMin

      if(currentMax > prevMax)
        prevMax  =  currentMax
    }
    yLim[j,  "min"]  =  prevMin
    yLim[j,  "max"]  =  prevMax
}


## Fig S3 : Equilibrium biomasses according to temperature for different values of b ##

# save as tiff

equiBiom_names = c(expression(P[equ[1]]), expression(N[equ[2]]), expression(P[equ[2]]), expression(H[equ[2]]),
                   expression(N[equ[3]]), expression(P[equ[3]]), expression(H[equ[3]]), expression(C[equ[3]]))


tiff("./figures/figureS3.tiff",  width = 10,  height = 7,  units  =  'in',  res  =  300)

layout(matrix(c(1, 2, 3, 4, 5,
                6, 7, 8, 9, 9),  nrow  =  2,  ncol  =  5,  byrow  =  F),  width  =  c(1, 1, 1, 1, 0.3))

for(j in 1:length(resEquiBiom[[1]])) {
  plot(temp_seq,  unlist(resEquiBiom[[1]][j]),  type = "l",  ylim  =  c(yLim$min[j],  yLim$max[j]),
      ylab  =  equiBiom_names[j],  xlab  =  "Temperature", cex.lab = 1.2, mgp = c(1.8,0.5,0))

  for(i in 1:length(resEquiBiom)) {
    lines(temp_seq,  unlist(resEquiBiom[[i]][j]),  col  =  colo[i])
    }
}

par(mar = c(2, 0.4, 2, 0))
legend_image <- as.raster(matrix(colfunc(5),  ncol = 1))
plot(c(0, 2), c(0, 1), type  =  'n',  axes  =  F, xlab  =  '',  ylab  =  '')

text(x = 0.9,  y  =  c(0.12, 0.89),  labels  = c(expression(0.2), expression(2.8)), cex = 1.5)

mtext(expression(b),  side  =  3,  at = 0.25, line  =  -4,  outer  =  FALSE, cex = 1.5)

rasterImage(rev(legend_image),  0,  0.1,  0.5, 0.9)

dev.off()

## save as pdf

pdf("./figures/figureS3.pdf",  width = 10,  height = 6)

layout(matrix(c(1, 2, 3, 4, 5,
                6, 7, 8, 9, 9),  nrow  =  2,  ncol  =  5,  byrow  =  F),  width  =  c(1, 1, 1, 1, 0.3))

for(j in 1:length(resEquiBiom[[1]])) {
  plot(temp_seq,  unlist(resEquiBiom[[1]][j]),  type = "l",  ylim  =  c(yLim$min[j],  yLim$max[j]),
      ylab  =  equiBiom_names[j],  xlab  =  "Temperature", cex.lab = 1.2, mgp = c(1.8,0.5,0))

  for(i in 1:length(resEquiBiom)) {
    lines(temp_seq,  unlist(resEquiBiom[[i]][j]),  col  =  colo[i])
    }
}

par(mar = c(2, 0.4, 2, 0))
legend_image <- as.raster(matrix(colfunc(5),  ncol = 1))
plot(c(0, 2), c(0, 1), type  =  'n',  axes  =  F, xlab  =  '',  ylab  =  '')

text(x = 0.9,  y  =  c(0.12, 0.89),  labels  = c(expression(0.2), expression(2.8)), cex = 1.5)

mtext(expression(b),  side  =  3,  at = 0.25, line  =  -4,  outer  =  FALSE, cex = 1.5)

rasterImage(rev(legend_image),  0,  0.1,  0.5, 0.9)

dev.off()
