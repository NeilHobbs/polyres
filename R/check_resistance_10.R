#' To see if resistance is gives lower than 10% survival.


check_resistance_10 = function(current.resistance.intensity){
  
  if(current.resistance.intensity > bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                                    michaelis.menten.slope = 1, 
                                                                    half.population.bioassay.survival.resistance = 900, 
                                                                    bioassay.survival = 0.1, 
                                                                    estimate.precision = 0.01, 
                                                                    sd.population.resistance = 0,
                                                                    nsim = 1000,
                                                                    minimum.resistance.value = 0, 
                                                                    maximum.resistance.value = 25000)){FALSE} else(TRUE)
  
}