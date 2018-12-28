##### Script for manuscript "Temperature modifies
##### consumer-resource interaction strength through
##### its effects on biological rates and body mass"

# This script computes IS measures (per population and net) for different scenarios of TSR
# It plots :
# - figure 5 : Effect of temperature on per population and net IS with and without TSR
# - figure S1 : Effect of temperature on parameters with and without TSR
# - figure S2 : Effect of temperature on equilibrium biomasses with and without TSR


## Sourced scripts

source('func_equiBiom_logISpop_logISnet.R') # Function to compute log(interaction strength)
source('func_TSR.R') # Function for temperature-size rule
source('parameters.R') # Function for temperature-size rule

# Plot
# List of parameters for different TSR scenarios

param=list(d=D,Nin=Nin,q=q,effPH=ePH,effHC=eHC,
           mu=mu_seq,zP=zP_seq,zH=zH_seq,aPH=aPH_seq,zC=zC_seq,aHC=aHC_seq,
           mP=mP,mH=mH,mC=mC)


paramTSR=list(d=D,Nin=Nin,q=q,effPH=ePH,effHC=eHC,
           mu=mu_seqTSR,zP=zP_seqTSR,zH=zH_seqTSR,aPH=aPH_seqTSR,zC=zC_seqTSR,aHC=aHC_seqTSR,
           mP=mP,mH=mH,mC=mC)

paramTSR_H=list(d=D,Nin=Nin,q=q,effPH=ePH,effHC=eHC,
            mu=mu_seq,zP=zP_seq,zH=zH_seqTSR,aPH=aPH_seqTSR,zC=zC_seq,aHC=aHC_seq,
            mP=mP,mH=mH,mC=mC)

paramTSR_C=list(d=D,Nin=Nin,q=q,effPH=ePH,effHC=eHC,
           mu=mu_seq,zP=zP_seq,zH=zH_seq,aPH=aPH_seq,zC=zC_seqTSR,aHC=aHC_seqTSR,
           mP=mP,mH=mH,mC=mC)

paramTSR_HC=list(d=D,Nin=Nin,q=q,effPH=ePH,effHC=eHC,
           mu=mu_seq,zP=zP_seq,zH=zH_seqTSR,aPH=aPH_seqTSR,zC=zC_seqTSR,aHC=aHC_seqTSR,
           mP=mP,mH=mH,mC=mC)

## Equilibrium biomasses ##
equiBiom_names = c(expression(P[equ[1]]), expression(N[equ[2]]), expression(P[equ[2]]), expression(H[equ[2]]),
                   expression(N[equ[3]]), expression(P[equ[3]]), expression(H[equ[3]]), expression(C[equ[3]]))

# No TSR
resEquiBiom=equiBiom(param)

# TSR
resEquiBiomTSR=equiBiom(paramTSR)

# TSR H only
resEquiBiomTSR_H=equiBiom(paramTSR_H)

# TSR C only
resEquiBiomTSR_C=equiBiom(paramTSR_C)

# TSR H and C
resEquiBiomTSR_HC=equiBiom(paramTSR_HC)


## Interaction strength per population ##

# TSR
reslogISpopTSR=logISpop(resEquiBiomTSR,aPH_seqTSR,aHC_seqTSR)

logISpop_H2TSR=reslogISpopTSR$ISpopH2
logISpop_H3TSR=reslogISpopTSR$ISpopH3
logISpop_C3TSR=reslogISpopTSR$ISpopC3

# No TSR
reslogISpop=logISpop(resEquiBiom,aPH_seq,aHC_seq)

logISpop_H2=reslogISpop$ISpopH2
logISpop_H3=reslogISpop$ISpopH3
logISpop_C3=reslogISpop$ISpopC3

# TSR C only
reslogISpopTSR_C = logISpop(resEquiBiomTSR_C, aPH_seq, aHC_seqTSR)

logISpop_H2TSR_C=reslogISpopTSR_C$ISpopH2
logISpop_H3TSR_C=reslogISpopTSR_C$ISpopH3
logISpop_C3TSR_C=reslogISpopTSR_C$ISpopC3

# TSR H and C
reslogISpopTSR_HC = logISpop(resEquiBiomTSR_HC, aPH_seqTSR, aHC_seqTSR)

logISpop_H2TSR_HC=reslogISpopTSR_HC$ISpopH2
logISpop_H3TSR_HC=reslogISpopTSR_HC$ISpopH3
logISpop_C3TSR_HC=reslogISpopTSR_HC$ISpopC3

## Net interaction strength ##

# TSR
reslogISnetTSR=logISnet(resEquiBiomTSR)

logISnet_HPTSR=reslogISnetTSR$ISnetHP
logISnet_CPTSR=reslogISnetTSR$ISnetCP
logISnet_CHTSR=reslogISnetTSR$ISnetCH

# No TSR
reslogISnet=logISnet(resEquiBiom)

logISnet_HP=reslogISnet$ISnetHP
logISnet_CP=reslogISnet$ISnetCP
logISnet_CH=reslogISnet$ISnetCH

# TSR C only
reslogISnetTSR_C=logISnet(resEquiBiomTSR_C)

logISnet_HPTSR_C=reslogISnetTSR_C$ISnetHP
logISnet_CPTSR_C=reslogISnetTSR_C$ISnetCP
logISnet_CHTSR_C=reslogISnetTSR_C$ISnetCH

# TSR HC
reslogISnetTSR_HC=logISnet(resEquiBiomTSR_H)

logISnet_HPTSR_HC=reslogISnetTSR_HC$ISnetHP
logISnet_CPTSR_HC=reslogISnetTSR_HC$ISnetCP
logISnet_CHTSR_HC=reslogISnetTSR_HC$ISnetCH


#### PLOT MANUSCRIPT ####

# save as tiff
tiff("./figures/figure5.tiff", width=7, height=4, units = 'in', res = 300)

### Plot IS pop
layout(matrix(1:3,ncol=3), width = c(3,3,1),height = c(1,1,1))

ylimC3pop=c(min(logISpop_C3, logISpop_C3TSR, logISpop_C3TSR_C, logISpop_C3TSR_HC)
            ,max(logISpop_C3, logISpop_C3TSR, logISpop_C3TSR_C, logISpop_C3TSR_HC)+0.1)

rylimC3pop=round(ylimC3pop,2)

plot(temp_seq,logISpop_C3,ylim=rylimC3pop,
     ann=FALSE,axes=F,type='n',xaxs='i',yaxs='i')

axis(2,tck = -0.015,at=rylimC3pop,
     labels=rylimC3pop,mgp=c(3,0.5,0),las=2,cex.axis=1.5)
axis(1,tck = -0.015,at=seq(min(temp_seq),max(temp_seq),10),labels=seq(min(temp_seq),max(temp_seq),10)
     ,mgp=c(3,0.8,0),cex.axis=1.5)

lines(temp_seq,logISpop_C3,lwd=2)
lines(temp_seq,logISpop_C3TSR,lwd=2,lty=2)
lines(temp_seq,logISpop_C3TSR_C,lwd=2,lty=4)
lines(temp_seq,logISpop_C3TSR_HC,lwd=2,lty=5)


mtext(text=expression(log(IS[pop[C-H]])),side=2,line=1,font=1,cex=1)
mtext(text='Temperature',side=1,line=2.5,font=1,cex=1)
mtext(expression(bold((A))), side = 3, at=298,line = 0, outer = FALSE,cex=1)

### Plot ISnet

ylimC3net=c(min(logISnet_CH,logISnet_CHTSR, logISnet_CHTSR_C, logISnet_CHTSR_HC)
            ,max(logISnet_CH,logISnet_CHTSR, logISnet_CHTSR_C, logISnet_CHTSR_C)+0.1)
rylimC3net=round(ylimC3net,2)

plot(temp_seq,logISnet_CH,ylim=rylimC3net,
     ann=FALSE,axes=F,type='n',xaxs='i',yaxs='i')

axis(2,tck = -0.015,at=rylimC3net,
     labels=rylimC3net,mgp=c(3,0.5,0),las=2,cex.axis=1.5)
axis(1,tck = -0.015,at=seq(min(temp_seq),max(temp_seq),10),labels=seq(min(temp_seq),max(temp_seq),10)
     ,mgp=c(3,0.8,0),cex.axis=1.5)

lines(temp_seq,logISnet_CH,lwd=2)
lines(temp_seq,logISnet_CHTSR,lwd=2,lty=2)
lines(temp_seq,logISnet_CHTSR_C,lwd=2,lty=4)
lines(temp_seq,logISnet_CHTSR_HC,lwd=2,lty=5)


mtext(text=expression(log(IS[net[C-H]])),side=2,line=1,font=1,cex=1)
mtext(text='Temperature',side=1,line=2.5,font=1,cex=1)
mtext(expression(bold((B))), side = 3, at=298,line = 0, outer = FALSE,cex=1)

oldMar <- par(mar = c(0,0,0,0))
plot.new()
legend("center",legend=c("None", "C","HC", "PHC"),inset=0,lty=c(1,4,5,2),bty="n",cex=1.4)
par(oldMar)


dev.off()


pdf("./figures/figure5.pdf", width=7, height=4)

### Plot IS pop
layout(matrix(1:3,ncol=3), width = c(3,3,1),height = c(1,1,1))

ylimC3pop=c(min(logISpop_C3, logISpop_C3TSR, logISpop_C3TSR_C, logISpop_C3TSR_HC)
            ,max(logISpop_C3, logISpop_C3TSR, logISpop_C3TSR_C, logISpop_C3TSR_HC)+0.1)

rylimC3pop=round(ylimC3pop,2)

plot(temp_seq,logISpop_C3,ylim=rylimC3pop,
     ann=FALSE,axes=F,type='n',xaxs='i',yaxs='i')

axis(2,tck = -0.015,at=rylimC3pop,
     labels=rylimC3pop,mgp=c(3,0.5,0),las=2,cex.axis=1.5)
axis(1,tck = -0.015,at=seq(min(temp_seq),max(temp_seq),10),labels=seq(min(temp_seq),max(temp_seq),10)
     ,mgp=c(3,0.8,0),cex.axis=1.5)

lines(temp_seq,logISpop_C3,lwd=2)
lines(temp_seq,logISpop_C3TSR,lwd=2,lty=2)
lines(temp_seq,logISpop_C3TSR_C,lwd=2,lty=4)
lines(temp_seq,logISpop_C3TSR_HC,lwd=2,lty=5)


mtext(text=expression(log(IS[pop[C-H]])),side=2,line=1,font=1,cex=1)
mtext(text='Temperature',side=1,line=2.5,font=1,cex=1)
mtext(expression(bold((A))), side = 3, at=298,line = 0, outer = FALSE,cex=1)

### Plot ISnet

ylimC3net=c(min(logISnet_CH,logISnet_CHTSR, logISnet_CHTSR_C, logISnet_CHTSR_HC)
            ,max(logISnet_CH,logISnet_CHTSR, logISnet_CHTSR_C, logISnet_CHTSR_C)+0.1)
rylimC3net=round(ylimC3net,2)

plot(temp_seq,logISnet_CH,ylim=rylimC3net,
     ann=FALSE,axes=F,type='n',xaxs='i',yaxs='i')

axis(2,tck = -0.015,at=rylimC3net,
     labels=rylimC3net,mgp=c(3,0.5,0),las=2,cex.axis=1.5)
axis(1,tck = -0.015,at=seq(min(temp_seq),max(temp_seq),10),labels=seq(min(temp_seq),max(temp_seq),10)
     ,mgp=c(3,0.8,0),cex.axis=1.5)

lines(temp_seq,logISnet_CH,lwd=2)
lines(temp_seq,logISnet_CHTSR,lwd=2,lty=2)
lines(temp_seq,logISnet_CHTSR_C,lwd=2,lty=4)
lines(temp_seq,logISnet_CHTSR_HC,lwd=2,lty=5)


mtext(text=expression(log(IS[net[C-H]])),side=2,line=1,font=1,cex=1)
mtext(text='Temperature',side=1,line=2.5,font=1,cex=1)
mtext(expression(bold((B))), side = 3, at=298,line = 0, outer = FALSE,cex=1)

oldMar <- par(mar = c(0,0,0,0))
plot.new()
legend("center",legend=c("None", "C","HC", "PHC"),inset=0,lty=c(1,4,5,2),bty="n",cex=1.4)
par(oldMar)

dev.off()

###### FIGURE Supplementary Materials ######

## Figure S1 : parameters according to temperature with and without TSR ##

## save as tiff

tiff("./figures/figureS1.tiff", width=10, height=8, units = 'in', res = 300)
par(mfrow=c(2,3))

plot(temp_seq ,mu_seq, ylim = c(min(mu_seq,mu_seqTSR), max(mu_seq,mu_seqTSR)), type = "l",
        ylab = expression(mu), xlab = "", cex.lab = 1.2)
lines(temp_seq,mu_seqTSR,col = "red", lty = 2)

plot(temp_seq, aPH_seq, ylim = c(min(aPH_seq, aPH_seqTSR),max(aPH_seq, aPH_seqTSR)), type = "l",
        ylab = expression(a[PH]), xlab = "", cex.lab = 1.2)
lines(temp_seq, aPH_seqTSR, col = "red", lty = 2)

plot(temp_seq, aHC_seq, ylim = c(min(aHC_seq, aHC_seqTSR), max(aHC_seq, aHC_seqTSR)), type = "l",
        ylab = expression(a[HC]), xlab = "", cex.lab = 1.2)
lines(temp_seq, aHC_seqTSR, col = "red", lty = 2)

plot(temp_seq, zP_seq, ylim = c(min(zP_seq, zP_seqTSR), max(zP_seq, zP_seqTSR)), type = "l",
        ylab = expression(z[P]), xlab = "Temperature", cex.lab = 1.2)
lines(temp_seq, zP_seqTSR, col = "red", lty = 2)

plot(temp_seq, zH_seq, ylim = c(min(zH_seq,zH_seqTSR), max(zH_seq,zH_seqTSR)), type = "l",
        ylab = expression(z[H]), xlab = "Temperature", cex.lab = 1.2)
lines(temp_seq, zH_seqTSR, col = "red", lty = 2)

plot(temp_seq, zC_seq, ylim = c(min(zC_seq, zC_seqTSR), max(zC_seq, zC_seqTSR)), type = "l",
        ylab = expression(z[C]), xlab = "Temperature", cex.lab = 1.2)
lines(temp_seq, zC_seqTSR, col = "red", lty = 2)
legend("topleft", legend = c("No TSR", "TSR"), col = c("black", "red"), lty = c(1,2), box.lty = 0)
dev.off()

## save as pdf
pdf("./figures/figureS1.pdf", width = 10, height = 8)
par(mfrow=c(2,3))

plot(temp_seq ,mu_seq, ylim = c(min(mu_seq,mu_seqTSR), max(mu_seq,mu_seqTSR)), type = "l",
        ylab = expression(mu), xlab = "", cex.lab = 1.2)
lines(temp_seq,mu_seqTSR,col = "red", lty = 2)

plot(temp_seq, aPH_seq, ylim = c(min(aPH_seq, aPH_seqTSR),max(aPH_seq, aPH_seqTSR)), type = "l",
        ylab = expression(a[PH]), xlab = "", cex.lab = 1.2)
lines(temp_seq, aPH_seqTSR, col = "red", lty = 2)

plot(temp_seq, aHC_seq, ylim = c(min(aHC_seq, aHC_seqTSR), max(aHC_seq, aHC_seqTSR)), type = "l",
        ylab = expression(a[HC]), xlab = "", cex.lab = 1.2)
lines(temp_seq, aHC_seqTSR, col = "red", lty = 2)

plot(temp_seq, zP_seq, ylim = c(min(zP_seq, zP_seqTSR), max(zP_seq, zP_seqTSR)), type = "l",
        ylab = expression(z[P]), xlab = "Temperature", cex.lab = 1.2)
lines(temp_seq, zP_seqTSR, col = "red", lty = 2)

plot(temp_seq, zH_seq, ylim = c(min(zH_seq,zH_seqTSR), max(zH_seq,zH_seqTSR)), type = "l",
        ylab = expression(z[H]), xlab = "Temperature", cex.lab = 1.2)
lines(temp_seq, zH_seqTSR, col = "red", lty = 2)

plot(temp_seq, zC_seq, ylim = c(min(zC_seq, zC_seqTSR), max(zC_seq, zC_seqTSR)), type = "l",
        ylab = expression(z[C]), xlab = "Temperature", cex.lab = 1)
lines(temp_seq, zC_seqTSR, col = "red", lty = 2)
legend("topleft", legend = c("No TSR", "TSR"), col = c("black", "red"), lty = c(1,2), box.lty = 0)
dev.off()

## Figure S2 : biomass densities at equilibrium ##

## save as tiff

tiff("./figures/figureS2.tiff", width=10, height=7, units = 'in', res = 300)
par(mfrow=c(2,4))

for(i in 1:length(resEquiBiom)){
        ylim=c(min(resEquiBiomTSR[[i]],resEquiBiom[[i]]), max(resEquiBiomTSR[[i]],resEquiBiom[[i]]))

        plot(temp_seq, resEquiBiom[[i]], type = "l", xlab = "Temperature (K)",
        ylab = equiBiom_names[i], ylim = ylim, cex.lab = 1.2, mgp = c(1.8,0.5,0))
        lines(temp_seq, resEquiBiomTSR[[i]], col = "red", lty = 2)
}
legend("topright", legend = c("No TSR", "TSR"), col = c("black", "red"), lty = c(1,2), box.lty = 0)

dev.off()

## save as pdf

pdf("./figures/figureS2.pdf", width=10, height=7)
par(mfrow=c(2,4))

for(i in 1:length(resEquiBiom)){

        ylim=c(min(resEquiBiomTSR[[i]],resEquiBiom[[i]]), max(resEquiBiomTSR[[i]],resEquiBiom[[i]]))

        plot(temp_seq, resEquiBiom[[i]], type = "l", xlab = "Temperature (K)",
        ylab = equiBiom_names[i], ylim = ylim, cex.lab = 1.2, mgp = c(1.8,0.5,0))
        lines(temp_seq, resEquiBiomTSR[[i]], col = "red", lty = 2)

}
legend("topright", legend = c("No TSR", "TSR"), col = c("black", "red"), lty = c(1,2), box.lty = 0)

dev.off()
