#' Plot the impact of changing the michaelis.menten.slope parameter in the resistance_to_bioassay_survival function.
#' Demonstrates the equations only work correctly when michaelis.menten.slope = 1.
#' 
#' @import magrittr dplyr ggplot2
#' @importFrom magrittr %>%
#' @name %>%
#' 
#' @param michaelis.menten.slope.values A vector of values, including the value 1. Recommend a sequence from 0 to 2.
#' @param sd.population.resistance.values How much variation in the population resistance. 
#' @param maximum.bioassay.survival Must be set at 1.
#' @param half.population.bioassay.survival.resistance As calculated with calculate_half_population_survival
#' @param mean.population.resistance The resistance intensity of the population
#' @param nsim Number of repitions of the rnorm function. Recommend 1000.
#' 
#' @example plot_michaelis_menten_slope_survival(
#'                                   michaelis.menten.slope.values = michaelis.menten.slope.values, 
#'                                   sd.population.resistance.values = sd.population.resistance.values, 
#'                                   maximum.bioassay.survival = 1,
#'                                   half.population.bioassay.survival.resistance = 900, 
#'                                   mean.population.resistance = 900,
#'                                   nsim = 1000)

plot_michaelis_menten_slope_survival = function(michaelis.menten.slope.values, 
                                                sd.population.resistance.values, 
                                                maximum.bioassay.survival,
                                                half.population.bioassay.survival.resistance, 
                                                mean.population.resistance,
                                                nsim){
  df = data.frame(michaelis.menten.slope.values, sd.population.resistance.values)%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival.proportion = resistance_to_bioassay_survival(
      maximum.bioassay.survival = maximum.bioassay.survival, 
      mean.population.resistance = mean.population.resistance, 
      michaelis.menten.slope = michaelis.menten.slope.values, 
      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
      sd.population.resistance = sd.population.resistance.values,
      nsim = nsim))%>%
    dplyr::mutate(sd.population.resistance.values = as.factor(sd.population.resistance.values))
  
  ggplot2::ggplot(df, aes(x=michaelis.menten.slope.values, y=bioassay.survival.proportion)) +
    geom_point(aes(color = sd.population.resistance.values)) +
    theme_classic()+
    ylab("Bioassay Survival Proportion") +
    xlab("Michaelis-Menten Equation Slope") +
    ggtitle(paste("Mean population resistance=",mean.population.resistance,
                  "& resistance for 50% survival=",half.population.bioassay.survival.resistance))
}



