#' Function to convert mean population resistance intensity to bioassay survival
#' 
#' @param maximum.bioassay.survival.proportion Should be set as 1.
#' @param mean.population.resistance 
#' @param michaelis.menton.slope Should be set as 1
#' @param half.population.bioassay.survival.resistance This is calculated using the calculate_half_population_resistance function
#' @param sd.population.resistance How much variation in the population resistance. 
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' 
#' @return mean(bioassay.survival.proportion) This is the mean bioassay survival for the population resistance intensity.


resistance_to_bioassay_survival = function(maximum.bioassay.survival.proportion,
                                           mean.population.resistance,
                                           michaelis.menton.slope, 
                                           half.population.bioassay.survival.resistance,
                                           sd.population.resistance, 
                                           nsim){ 
  
  #Generate a Normal distribution around the population mean of insecticide resistance values
  resistance_values = rnorm(nsim, 
                            mean = mean.population.resistance, 
                            sd = sd.population.resistance) 
  
  
  #Prevent Insecticide Resistance being less than 0, as this would give survival less than 0.
  resistance_values = ifelse(resistance_values < 0, 0, resistance_values) 
  
  ##Calculate Bioassay Survival (Equation 6)
  bioassay.survival.proportion = (maximum.bioassay.survival.proportion * 
                                    (resistance_values^michaelis.menton.slope)) / 
    (half.population.bioassay.survival.resistance + 
       (resistance_values ^michaelis.menton.slope))  
  
   #Prevent survival being less than zero, as this is impossible!
    bioassay.survival.proportion = ifelse(bioassay.survival.proportion < 0, 0, bioassay.survival.proportion) 
  return(mean(bioassay.survival.proportion))
}




