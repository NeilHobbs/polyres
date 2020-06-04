#' Function to convert bioassay survival to mean population insecticide resistance intensity
#' 
#' @param maximum.bioassay.survival.proportion Should be set as 1.
#' @param michaelis.menten.slope Should be set as 1
#' @param half.population.bioassay.survival.resistance This is calculated using the calculate_half_population_resistance function
#' @param bioassay.survival The survival that occured in the bioassay, as a proportion(values must be between 0 and 1). Where 1=all survived, 0=all died
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param sd.population.resistance How much variation in the population resistance. 
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' @param minimum.resistance.value Recommend setting as 0.
#' @param maximum.resistance.value This will depend on the half survival scale, but 10000 would be a good start. 
#' 
#' @return test.population.resistance This is the mean insecticide resistance intensity of the population.

bioassay_survival_to_resistance = function(maximum.bioassay.survival.proportion = 1,
                                           michaelis.menten.slope = 1, #must be set to 1 to work properly
                                           half.population.bioassay.survival.resistance = 900, 
                                           bioassay.survival = 0.1, 
                                           estimate.precision = 0.01, 
                                           sd.population.resistance = 10,
                                           nsim = 1000,
                                           minimum.resistance.value = 0, 
                                           maximum.resistance.value = 25000){
  
  while((test.population.resistance = ((minimum.resistance.value + maximum.resistance.value)/2))){
    
    if((maximum.resistance.value - minimum.resistance.value) < estimate.precision)
    {return(test.population.resistance)} #When precision level reached return population resistance
    else(
      if(resistance_to_bioassay_survival(
        maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
        mean.population.resistance = test.population.resistance, 
        michaelis.menten.slope = michaelis.menten.slope, 
        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
        sd.population.resistance = sd.population.resistance, 
        nsim = nsim) < bioassay.survival) #check if survival less than bioassay survival
      {
        minimum.resistance.value = test.population.resistance} #TRUE update min value
      else(maximum.resistance.value = test.population.resistance))#FALSE update max value
  }
}#end while loop