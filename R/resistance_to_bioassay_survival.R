#' @title Convert insecticide resistance intensity to bioassay survival proportion
#' 
#' @param maximum.bioassay.survival.proportion Should be set as 1.
#' @param mean.population.resistance The mean resistance intensity of the population.
#' @param michaelis.menten.slope Should be set as 1
#' @param half.population.bioassay.survival.resistance This is calculated using the calculate_half_population_resistance function
#' @param sd.population.resistance How much variation in the population resistance. 
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' 
#' @return mean(bioassay.survival.proportion) This is the mean bioassay survival for the population resistance intensity.


resistance_to_bioassay_survival = function(maximum.bioassay.survival.proportion = 1,
                                           mean.population.resistance = 900,
                                           michaelis.menten.slope = 1, 
                                           half.population.bioassay.survival.resistance = 900,
                                           sd.population.resistance = 10, 
                                           nsim = 1000){ 
  
  #Error Messages
  if(michaelis.menten.slope != 1){stop("michaelis.menten.slope must equal 1")}
  if(maximum.bioassay.survival.proportion != 1){stop("maximum.bioassay.survival.proportion must equal 1.")}
  if(sd.population.resistance < 0){stop("sd.population.resistance must be greater than or equal to 0.")}

  
  #Generate a Normal distribution around the population mean of insecticide resistance values
  resistance.values = rnorm(n = nsim, 
                            mean = mean.population.resistance, 
                            sd = sd.population.resistance)
  
  
  #Prevent Insecticide Resistance being less than 0, as this would give survival less than 0.
  resistance.values = ifelse(resistance.values < 0, 0, resistance.values) 
  
  ##Calculate Bioassay Survival (Equation 6)
  bioassay.survival.proportion = (maximum.bioassay.survival.proportion * 
                                    (resistance.values^michaelis.menten.slope)) / 
    (half.population.bioassay.survival.resistance + 
       (resistance.values ^michaelis.menten.slope))  
  
   #Prevent survival being less than zero, as this is impossible!
    bioassay.survival.proportion = ifelse(bioassay.survival.proportion < 0, 0, bioassay.survival.proportion) 
  return(mean(bioassay.survival.proportion))
}




