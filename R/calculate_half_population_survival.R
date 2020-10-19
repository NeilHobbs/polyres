#' @title Calculate the 50% survival for a given resistance intensity
#' 
#' @description
#' A function that enables calculation of a  50% survival threshold, based on desired 
#' resistance value and its desired survival value. For example if you want to base the scale on 
#'  a 20% survival having a resistance intensity of 1000. 
#' 
#' 
#' @param desired.resistance The value you want a survival value to correspond to
#' @param desired.survival.proportion The survival proportion you want your desired.resistance to have. Values must be between 0 and 1
#' @param maximum.bioassay.survival.proportion Must be set at 1
#' @param michaelis.menten.slope Must be set at 1
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param sd.population.resistance How much variation in the population resistance. Recommend setting as zero for scale setting
#' @param nsim Number of replications of the rnorm function. Recommended value = 1000
#' @param minimum.resistance.value Recommend setting to 0. Must be lower than half survival resistance.
#' @param maximum.resistance.value Depends on your scale. Recommend setting arbitrarily high (10000). Must be higher than half resistance survival
#' 
#' @return a value for the half.population.survival.value which is the half.population.bioassay.survival.resistance for the scale.

calculate_half_population_survival = function(desired.resistance,
                                              desired.survival.proportion,
                                              maximum.bioassay.survival.proportion = 1,
                                              michaelis.menten.slope = 1, 
                                              estimate.precision = 0.01, 
                                              sd.population.resistance,
                                              nsim = 1000,
                                              minimum.resistance.value = 0, 
                                              maximum.resistance.value = 25000){
  
  #Error Messages
  if(michaelis.menten.slope != 1){stop("michaelis.menten.slope must equal 1")}
  if(maximum.bioassay.survival.proportion != 1){stop("maximum.bioassay.survival.proportion must equal 1.")}
  if(bioassay.survival > 1 | bioassay.survival < 0){stop("Bioassay survival must be between 0 and 1.")}
  if(sd.population.resistance < 0){stop("sd.population.resistance must be greater than or equal to 0.")}
  
  #Warning messages
  if(minimum.resistance.value > 10){warning("High input for minimum.resistance.value, bioassay survival could be out of range.")}
  if(maximum.resistance.value < 1000){warning("Low input for maximum.bioassay.survival.proportion, bioassay survival could be out of range.")}
  if(half.population.bioassay.survival.resistance < minimum.resistance.value |
     half.population.bioassay.survival.resistance > maximum.resistance.value){warning("half.population.survival.resistance outside resistance value range")}
  
  while((half.population.survival.value = ((minimum.resistance.value + maximum.resistance.value)/2))){
    if((maximum.resistance.value - minimum.resistance.value) < estimate.precision)
    {return(half.population.survival.value)} #When precision level reached return population resistance
    else(
      if(resistance_to_bioassay_survival(
        maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
        mean.population.resistance = desired.resistance, 
        michaelis.menten.slope = michaelis.menten.slope, 
        half.population.bioassay.survival.resistance = half.population.survival.value, 
        sd.population.resistance = sd.population.resistance, 
        nsim = nsim) > desired.survival.proportion) #check if survival 
      {
        minimum.resistance.value = half.population.survival.value} 
      else(maximum.resistance.value = half.population.survival.value))
  }
}