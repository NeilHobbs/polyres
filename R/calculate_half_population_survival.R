#' A function that enables calculation of a  50% survival threshold
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


calculate_half_population_survival = function(desired.resistance,
                                              desired.survival.proportion,
                                              maximum.bioassay.survival.proportion,
                                              michaelis.menten.slope, 
                                              estimate.precision, 
                                              sd.population.resistance,
                                              nsim,
                                              minimum.resistance.value, 
                                              maximum.resistance.value){
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