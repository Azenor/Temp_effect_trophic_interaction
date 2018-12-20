##### Script for manuscript "Temperature modifies
##### consumer-resource interaction strength through
##### its effects on biological rates and body mass"

###### Temperature-size responses, following Forster et al 2012 ######
### Functions to compute body masses after TS responses
## TSRaqua : mean response for aquatic organisms
## TSR max : maximum response

### TSR for aqua organisms

TSRaqua = function(Temp, DM20){
  TempCel = Temp-273.15
  c = 6.5
  PCM = - 3.90 - 0.53 * log10(DM20)        # Percentage change in body-mass per degrés C
  S = log(PCM/100+1)                     # Sign and magnitude of TS response
  M = c * DM20 * exp(S * (TempCel - 20))/1000  # Dry mass converted to fresh mass in g
  return(M)
}

### TSR max

TSRmax = function(Temp, DM20){
  TempCel = Temp - 273.15
  c = 6.5
  PCM = - 8
  S = log(PCM/100+1)
  M = c * DM20 * exp(S * (TempCel - 20))/1000
  return(M)
}
