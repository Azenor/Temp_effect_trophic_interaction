##### Script for manuscript "Effect of temperature on
##### consumer-resource interaction strength"

## This script contains 3 functions:
## A function to compute biomass densities at equilibrium
## A function to compute interaction strengths per population
## A function to compute net interaction strengths

### Function equiBiom: Compute equilibrium biomass densities
## Input: A vector of model parameters:
##        D, dilution rate
##        Nin, nutrient input concentrations
##        e, conversion efficieny
##        A, attack success probability
##        q, C:N ratio
##        ks, half saturation constant
##        mu, growth rate
##        z, mortality rate temperature dependent
##        m, mortality rate temperature independent
##        a, search rate
## Output: A list of equilibrium biomass densities:
##         Pequ1, primary producers (1 trophic level)
##         Nequ2, nurient concentration (2 trophic levels)
##         Hequ2, herbivore (2 trophic levels)
##         Hequ3, herbivore (3 trophic levels)
##         Nequ3, nutrient (3 trophic levels)
##         Pequ3, primary producers (3 trophic levels)
##         Cequ3, carnivore (3 trophic levels)

equiBiom = function(para)  {
  with(as.list(para), {

    ## Equilibrial biomasses
    ## NP
    Pequ1 = ((q * d * Nin)/(zP + mP)) - (d/mu)

    ## NPH
    Pequ2 = (zH + mH)/(effPH * aPH)
    Nequ2 = (d * Nin)/(d + mu * Pequ2)
    Hequ2 = (q * mu * Nequ2 - zP - mP)/(aPH)

    ## NPHC
    Hequ3 = (zC + mC)/(effHC * aHC)
    Nequ3 = (zP + mP + (aPH * Hequ3))/(q * mu)
    Pequ3 = (d * Nin - d * Nequ3)/(mu * Nequ3)
    Cequ3 = ((effPH * aPH * Pequ3) - zH - mH)/aHC

    return(list(Pequ1 = Pequ1, Nequ2 = Nequ2, Pequ2 = Pequ2, Hequ2 = Hequ2,
                Nequ3 = Nequ3, Pequ3 = Pequ3, Hequ3 = Hequ3, Cequ3 = Cequ3))

  })
}

## Function logISpop: compute interaction strength per population
## Input: A list of equilibrium biomass densities
## Output: A list of log(interaction strength per population):
##         ISpopH2, effect of herbivore on primary producer (2 trophic levels)
##         ISpopH3, effect of herbivore on primary producer (3 trophic levels)
##         ISpopC3, effect of carnivore on herbivore (3 trophic levels)

logISpop = function(listEquiBiom, aPH_seq, aHC_seq)  {
  with(as.list(listEquiBiom),{

    # Interaction strength
    ISpopH2 = log(listEquiBiom$Hequ2 * aPH_seq)
    ISpopH3 = log(listEquiBiom$Hequ3 * aPH_seq)
    ISpopC3 = log(listEquiBiom$Cequ3 * aHC_seq)

    return(list(ISpopH2 = ISpopH2, ISpopH3 = ISpopH3, ISpopC3 = ISpopC3))

  })
}

## Function logISnet: compute net interaction strength
## Input: A vector of model parameters:
##        D, dilution rate
##        Nin, nutrient input concentrations
##        e, conversion efficieny
##        A, attack success probability
##        q, C:N ratio
##        ks, half saturation constant
##        mu, growth rate
##        z, mortality rate
##        a, search rate
## Output: A list of log(net interaction strength):
##         ISnetH2, effect of herbivore on primary producer (2 trophic levels)
##         ISnetH3, effect of herbivore on primary producer (3 trophic levels)
##         ISnetC3, effect of carnivore on herbivore (3 trophic levels)

logISnet = function(listEquiBiom)  {
  with(as.list(listEquiBiom),{

    ## Strength of interaction
    ISnetHP = log(Pequ1/Pequ2)
    ISnetCP = log(Pequ2/Pequ3)
    ISnetCH = log(Hequ2/Hequ3)

    return(list(ISnetHP = ISnetHP, ISnetCP = ISnetCP, ISnetCH = ISnetCH))

  })
}
