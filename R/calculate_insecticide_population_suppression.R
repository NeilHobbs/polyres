#'@title The impact of insecticides on the relative population size of female mosquitoes the intervention site, who are now ready to lay eggs.
#'
#'@param female.population.size.after.selection = The relative population size of all the female mosquitoes in the intervention site after insecticide selection.
#'@param total.female.population.size = The total relative population size of female mosquitoes in the intervention site before insecticide selection.


calculate_insecticide_population_suppression = function( minimum.female.insecticide.exposure,
                                                         maximum.female.insecticide.exposure,
                                                         nsim,
                                                         intercept,
                                                         conversion.factor,
                                                         current.insecticide.efficacy,
                                                         currently.deployed.insecticide,
                                                         sim.array,
                                                         current.generation,
                                                         half.population.bioassay.survival.resistance){
  
  female.insecticide.exposure = ifelse(minimum.female.insecticide.exposure == maximum.female.insecticide.exposure,
                                       yes = minimum.female.insecticide.exposure,
                                       no = runif(n = nsim,
                                                  min = minimum.female.insecticide.exposure,
                                                  max = maximum.female.insecticide.exposure))
  
  
  intervention.site.bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                        mean.population.resistance = sim.array['treatment', currently.deployed.insecticide, current.generation],
                                                                        michaelis.menten.slope = 1, 
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                        sd.population.resistance = 0, 
                                                                        nsim = 1)
  
  intervention.site.field.survival = convert_bioassay_survival_to_field_insecticide_decay(conversion.factor,
                                                                        bioassay.survival = intervention.site.bioassay.survival,
                                                                        intercept = intercept,
                                                                        current.insecticide.efficacy)
  
  
    insecticide.population.suppression = (1 - female.insecticide.exposure) + female.insecticide.exposure*intervention.site.field.survival
  
  
  return(insecticide.population.suppression)
}