#'@title The impact of insecticides on the relative population size of female mosquitoes the intervention site, who are now ready to lay eggs.
#'
#'@param minimum.female.insecticide.exposure = the minimim proportion of female mosquitoes exposed to the insecticide
#'@param maximum.female.insecticide.exposure = the maximum proportion of female mosquitoes exposed to the insecticide
#'@param nsim = number of interations of runif
#'@param intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param conversion.factor = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The insecticide efficacy of insecticide i at time since deployment τ defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.
#'@param currently.deployed.insecticide = The currently deployed insecticide in the intervention(treatment) site
#'@param sim.array = the array that holds the simulation data
#'@param current.generation = the current generation the model is tracking
#'@param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.


calculate_insecticide_population_suppression = function(minimum.female.insecticide.exposure,
                                                        maximum.female.insecticide.exposure,
                                                        nsim,
                                                        intercept,
                                                        conversion.factor,
                                                        current.insecticide.efficacy,
                                                        currently.deployed.insecticide,
                                                        sim.array,
                                                        current.generation,
                                                        half.population.bioassay.survival.resistance){
  
  #Get the female insecticide exposure. 
  female.insecticide.exposure = ifelse(minimum.female.insecticide.exposure == maximum.female.insecticide.exposure,
                                       yes = minimum.female.insecticide.exposure,
                                       no = mean(runif(n = nsim,
                                                  min = minimum.female.insecticide.exposure,
                                                  max = maximum.female.insecticide.exposure)))
  
  #Get the bioassay survival of the intervention site to the currently deployed insecticide. Is current.generation-1.
  intervention.site.bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                        mean.population.resistance = sim.array['treatment', currently.deployed.insecticide, current.generation-1],
                                                                        michaelis.menten.slope = 1, 
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                        sd.population.resistance = 0, 
                                                                        nsim = 1)
  
  #Convert to field survival
  intervention.site.field.survival = convert_bioassay_survival_to_field_insecticide_decay(conversion.factor,
                                                                                          bioassay.survival = intervention.site.bioassay.survival,
                                                                                          intercept = intercept,
                                                                                          current.insecticide.efficacy)
  
  #Then do equation 10(i)::
  insecticide.population.suppression = (1 - female.insecticide.exposure) + female.insecticide.exposure*intervention.site.field.survival
  
  #return the proportion of the population that remains.
  return(insecticide.population.suppression)
}