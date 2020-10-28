#' Plot the relationship between survival and resistance calculated from bioassay survival
#' @import magrittr dplyr ggplot2
#' @importFrom magrittr %>%
#' @name %>%
#' 
#' @export
#'
#' @param maximum.bioassay.survival.proportion Should be set as 1.
#' @param michaelis.menten.slope Should be set as 1
#' @param half.population.bioassay.survival.resistance This is calculated using the calculate_half_population_resistance function
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param sd.population.resistance How much variation in the population resistance. 
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' @param minimum.resistance.value Recommend setting as 0.
#' @param maximum.resistance.value This will depend on the half survival scale, but 25000 would be a good start. 
#' @param minimum.bioassay.survival As a proportion, should be set as 0
#' @param maximum.bioassay.survival As a proportion, should be set as 1
#' @param divisions The resolution of the final plotted graph
#' 
#' 
#' @return A ggplot of the relationship between resistance (x axis) and survival (y axis).
#' 
#' @example 
#' plot_bioassay_survival_to_resistance(
#' maximum.bioassay.survival.proportion = 1,
#' michaelis.menten.slope = 1, 
#' half.population.bioassay.survival.resistance = 900, 
#' estimate.precision = 0.01, 
#' sd.population.resistance = 10, 
#' nsim = 1000, 
#' minimum.resistance.value = 0, 
#' maximum.resistance.value = 25000,
#' minimum.bioassay.survival = 0, 
#' maximum.bioassay.survival = 1, 
#' divisions = 0.01)

plot_bioassay_survival_to_resistance = function(maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1, 
                                                half.population.bioassay.survival.resistance = 900, 
                                                estimate.precision = 0.01, 
                                                sd.population.resistance = 10, 
                                                nsim = 1000, 
                                                minimum.resistance.value = 0, 
                                                maximum.resistance.value = 25000,
                                                minimum.bioassay.survival = 0, 
                                                maximum.bioassay.survival = 1, 
                                                divisions = 0.01){
  
    df=data.frame(bioassay.survival.values=seq(minimum.bioassay.survival, maximum.bioassay.survival,
                                             by = divisions))%>%
    
    dplyr::rowwise()%>%
      dplyr::mutate(resistance.values = bioassay_survival_to_resistance(
      maximum.bioassay.survival.proportion=maximum.bioassay.survival.proportion,
      michaelis.menten.slope=michaelis.menten.slope, 
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance, 
      bioassay.survival=bioassay.survival.values, 
      estimate.precision=estimate.precision, 
      sd.population.resistance=sd.population.resistance,
      nsim=nsim,
      minimum.resistance.value=minimum.resistance.value, 
      maximum.resistance.value=maximum.resistance.value)) ##plotting with sigma as 0.1 or 25 made no difference to plots
  
  ggplot2::ggplot(df, aes(x=resistance.values, y = bioassay.survival.values)) +
    geom_line(colour = "red") +
    xlab("Insecticide Resistance Intensity") +
    ylab("Bioassay Survival Proportion") +
    theme_classic()
}


#Need to figure out a way to get rid of the weird up-tick
#Also:
  #Dotted line for 10% Survival
  #Dotted line for 50% Survival