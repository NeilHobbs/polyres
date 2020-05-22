#' Create a table of the impact of michaelis.menten.slope values
#'
#' @import magrittr dplyr knitr
#' @importFrom magrittr %>%
#' @name %>%
#' 
#' @param michaelis.menten.slope.values A vector of values, including the value 1. Recommend a sequence from 0 to 2.
#' @param sd.population.resistance.values How much variation in the population resistance. 
#' @param maximum.bioassay.survival.proportion must be set as 1
#' @param half.population.bioassay.survival.resistance As calculated with calculate_half_population_survival
#' @param mean.population.resistance The resistance intensity of the population
#' @param nsim Number of repitions of the rnorm function. Recommend 1000.

table_michaelis_menten_slope_survival = function(michaelis.menten.slope.values, 
                                                 sd.population.resistance.values,
                                                 maximum.bioassay.survival.proportion = 1,
                                                 half.population.bioassay.survival.resistance = 900, 
                                                 mean.population.resistance = 900,
                                                 nsim = 1000){
  
  df = data.frame(michaelis.menten.slope.values, sd.population.resistance.values)%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                      mean.population.resistance = mean.population.resistance, 
                                                      michaelis.menten.slope = michaelis.menten.slope.values, 
                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
                                                      sd.population.resistance = sd.population.resistance.values, 
                                                      nsim = nsim))%>%
    dplyr::mutate(sd.population.resistance.values = as.factor(sd.population.resistance.values))
  
  return(knitr::kable(df))
}




