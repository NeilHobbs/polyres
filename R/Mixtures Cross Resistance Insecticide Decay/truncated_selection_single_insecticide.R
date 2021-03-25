
truncated_selection_single_insecticide = function(population.sd,
                                                  initial.resistance.intensity){
  
  
  
  
  surviving.proportion = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                         mean.population.resistance = initial.resistance.intensity,
                                                         michaelis.menten.slope = 1, 
                                                         half.population.bioassay.survival.resistance = 900,
                                                         sd.population.resistance = 0, 
                                                         nsim = 1)
  
  selection.differential = dnorm(((0 - initial.resistance.intensity)/ population.sd)) * (population.sd/surviving.proportion)
  
  
    return(selection.differential)
  
}

truncated_selection_single_insecticide(population.sd = 10,
                                       initial.resistance.intensity = 200)




selection_intensity = function(initial.resistance.intensity,
                               insecticide.efficacy,
                               population.sd){
  
  surviving.proportion = convert_bioassay_survival_to_field(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                mean.population.resistance = initial.resistance.intensity,
                                                                                                                michaelis.menten.slope = 1, 
                                                                                                                half.population.bioassay.survival.resistance = 900,
                                                                                                                sd.population.resistance = 0, 
                                                                                                                nsim = 1),
                                                            intercept = 0.15,
                                                            conversion.factor = 0.48) ^ insecticide.efficacy
  
  
  
  
  #Truncation selection for selection intensity
  selection.intensity = dnorm(qnorm(1-surviving.proportion))/surviving.proportion
  
  selection.differential = selection.intensity * population.sd #this is the selection of those mosquitoes exposed in the intervention site

 #Can therefore find the mean resistance intensity survival of the survivors:
  resistance.intensity.survivors = selection.differential + initial.resistance.intensity
  
return(list(surviving.proportion, resistance.intensity.survivors))
  
}

selection_intensity(initial.resistance.intensity = 0,
                    insecticide.efficacy = 1,
                    population.sd = 10)

selection_intensity(initial.resistance.intensity = (15*0.15),
                    insecticide.efficacy = 1,
                    population.sd = 10)


respoinse_to_selection_new = function(){
  
  exposure = (female.exposure + (male.exposure*female.exposure))/2
  
  
  
  
  
}





