# 
# if(0 > minimum.insecticide.resistance.hertitability |minimum.insecticide.resistance.hertitability > 1){stop("minimum.insecticide.resistance.hertitability must be between 0 and 1")}
# if(0 > maximum.insecticide.resistance.hertitability |maximum.insecticide.resistance.hertitability > 1){stop("maximum.insecticide.resistance.hertitability must be between 0 and 1")}
# if(minimum.insecticide.resistance.hertitability > maximum.insecticide.resistance.hertitability){stop("minimum.insecticide.resistance.hertitability is greater than maximum.insecticide.resistance.hertitability")}
# 
# if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
# if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
# if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}
# 
# if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
# if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
# if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}
# 
# if(0 > min.intervention.coverage | min.intervention.coverage > 1){stop("min.intervention.coverage must be between 0 and 1")}
# if(0 > max.intervention.coverage | max.intervention.coverage > 1){stop("max.intervention.coverage must be between 0 and 1")}
# if(min.intervention.coverage > max.intervention.coverage){stop("min.intervention.coverage is greater than max.intervention.coverage")}
# 
# if(0 > min.dispersal.rate | min.dispersal.rate > 1){stop("min.dispersal.rate must be between 0 and 1")}
# if(0 > max.dispersal.rate | max.dispersal.rate > 1){stop("max.dispersal.rate must be between 0 and 1")}
# if(min.dispersal.rate > max.dispersal.rate){stop("min.dispersal.rate is greater than max.dispersal.rate")}
# 
# if(0 > withdrawal.threshold.value | withdrawal.threshold.value > 1){stop("withdrawal.threshold.value must be between 0 and 1")}
# if(0 > return.threshold.value | return.threshold.value > 1){stop("return.threshold.value must be between 0 and 1")}
# if(return.threshold.value > withdrawal.threshold.value){warning("return.threshold.value is greater than withdrawal.threshold.value. Withdrawn insecticides will be unable to be returned to the arsenal.")}
# 
# if(michaelis.menten.slope != 1){stop("michaelis.menten.slope must equal 1")}
# if(maximum.bioassay.survival.proportion != 1){stop("maximum.bioassay.survival.proportion must equal 1.")}
# if(bioassay.survival > 1 | bioassay.survival < 0){stop("Bioassay survival must be between 0 and 1.")}
# if(sd.population.resistance < 0){stop("sd.population.resistance must be greater than or equal to 0.")}
# 
# #Warning messages
# if(minimum.resistance.value > 0.1){warning("High input for minimum.resistance.value, bioassay survival could be out of range.")}
# if(maximum.resistance.value < 5000){warning("Low input for maximum.bioassay.survival.proportion, bioassay survival could be out of range.")}
# if(half.population.bioassay.survival.resistance < minimum.resistance.value |
#    half.population.bioassay.survival.resistance > maximum.resistance.value){warning("half.population.survival.resistance outside resistance value range")}
