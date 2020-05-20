#' Function to generate a table of resistant values based on bioassay survival and standard deviation
#'
#' @import magrittr dplyr tidyr
#' @importFrom magrittr %>%
#' @name %>%
#' 
#' @param half.population.bioassay.survival.resistance As calculated using calculate_half_population_survival
#' @param maximum.bioassay.survival.proportion Should be set as 1
#' @param michaelis.menten.slope Should be set as 1
#' @param bioassay.survival.values A vector containing survival values with repeats. Same length as sd.population.values.
#' @param sd.population.values  A vector containing repeated standard deviation values. Same length as bioassay.survival values
#' @param estimate.precision ow precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' @param minimum.resistance.value Should be set as 0.
#' @param maximum.resistance.value Value required would depend on resistance intensity scale. Recommend starting with 10000

table_resistance_from_survival_and_sd = function(half.population.bioassay.survival.resistance, 
                                                 maximum.bioassay.survival.proportion, 
                                                 michaelis.menten.slope, 
                                                 bioassay.survival.values, 
                                                 sd.population.values, 
                                                 estimate.precision, 
                                                 nsim, 
                                                 minimum.resistance.value, 
                                                 maximum.resistance.value){
  
  df = data.frame(bioassay.survival.values, sd.population.values)%>%
    dplyr::rowwise()%>%
    dplyr::mutate(resistance.values = bioassay_survival_to_resistance(
      maximum.bioassay.survival.proportion=maximum.bioassay.survival.proportion, 
      michaelis.menten.slope=michaelis.menten.slope, 
      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
      bioassay.survival = bioassay.survival.values, 
      estimate.precision = estimate.precision, 
      sd.population.resistance = sd.population.values, 
      nsim = nsim,
      minimum.resistance.value = minimum.resistance.value, 
      maximum.resistance.value = maximum.resistance.value))%>%
    tidyr::spread(key = bioassay.survival.values, value = resistance.values)
  
  return(knitr::kable(df))
  
}


