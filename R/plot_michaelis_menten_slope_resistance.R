#' This function demonstrates the impact (by plotting) of what happens when you change michaelis.menten.slope in the bioassay_survival_to_resistance function,
#' and demonstrates how the function only performs correctly when the michaelis.menten.slope = 1.
#' 
#' @import magrittr dplyr ggplot2
#' @importFrom magrittr %>%
#' @name %>%
#' 
#' @param maximum.bioassay.survival.proportion Must be set to 1. 
#' @param michaelis.menton.slope.values A vector of values, including the value 1. Recommend a sequence from 0 to 2.
#' @param half.population.bioassay.survival.resistance As calculated with calculate_half_population_survival,
#' @param bioassay.survival The survival that occured in the bioassay, as a proportion(values must be between 0 and 1). Where 1=all survived, 0=all died 
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param sd.population.resistance.values How much variation in the population resistance. 
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' @param minimum.resistance.value Recommend setting as 0. Must be lower than the resistance being calculate.
#' @param maximum.resistance.value This will depend on the half survival scale, but 10000 would be a good start. Must be higher than the resistance value being calculated.
#' 
#' @example plot_michaelis_menten_slope_resistance(maximum.bioassay.survival.proportion = 1,
#'                                     michaelis.menton.slope.values = michaelis.menton.slope.values,
#'                                     half.population.bioassay.survival.resistance = 900,
#'                                     bioassay.survival = 0.5, 
#'                                     estimate.precision = 0.01, 
#'                                     sd.population.resistance.values = sd.population.resistance.values, 
#'                                     nsim = 1000, 
#'                                     minimum.resistance.value = 0,
#'                                     maximum.resistance.value = 10000)

plot_michaelis_menten_slope_resistance = function(maximum.bioassay.survival.proportion,
                                                  michaelis.menten.slope.values,
                                                  half.population.bioassay.survival.resistance,
                                                  bioassay.survival, 
                                                  estimate.precision, 
                                                  sd.population.resistance.values, 
                                                  nsim, 
                                                  minimum.resistance.value,
                                                  maximum.resistance.value){
  
  df = data.frame(michaelis.menten.slope.values, sd.population.resistance.values)%>%
    dplyr::rowwise()%>%
    dplyr::mutate(resistance.values = bioassay_survival_to_resistance(
      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion, 
      michaelis.menten.slope = michaelis.menten.slope.values, 
      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
      bioassay.survival = bioassay.survival,
      estimate.precision = estimate.precision,
      sd.population.resistance = sd.population.resistance.values, 
      nsim = nsim, 
      minimum.resistance.value = minimum.resistance.value, 
      maximum.resistance.value = maximum.resistance.value))%>%
    dplyr::mutate(sd.population.resistance.values = as.factor(sd.population.resistance.values))
  
  ggplot2::ggplot(df, aes(x=michaelis.menten.slope.values, y=resistance.values)) +
    geom_point(aes(color = sd.population.resistance.values)) +
    theme_classic()+
    ylab("Insecticide Resistance Intensity") +
    xlab("Michaelis-Menten Equation Slope") +
    ggtitle(paste("Bioassay Survival=",bioassay.survival,
                  "& Resistance for 50% Survival=",half.population.bioassay.survival.resistance))
}




