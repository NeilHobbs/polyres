#' Plot the relationship between resistance and survival calculated from insecticide resistance
#' @import magrittr dplyr ggplot2
#' @importFrom magrittr %>%
#' @name %>%
#' 
#' @export
#'
#' @param maximum.bioassay.survival.proportion Should be set as 1.
#' @param michaelis.menten.slope Should be set as 1
#' @param half.population.bioassay.survival.resistance This is calculated using the calculate_half_population_resistance function
#' @param bioassay.survival The survival that occured in the bioassay, as a proportion(values must be between 0 and 1). Where 1=all survived, 0=all died
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param sd.population.resistance How much variation in the population resistance. 
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' @param minimum.resistance Recommend setting as 0.
#' @param maximum.resistance This will depend on the half survival scale, but 10000 would be a good start. 
#' 
#' @return A ggplot of the relationship between resistance (x axis) and survival (y axis).
#' 
#' @example plot_resistance_to_bioassay_survival(
#' maximum.bioassay.survival.proportion = 1, 
#' michaelis.menten.slope = 1, 
#' half.population.bioassay.survival.resistance = 900, 
#' nsim = 1000, 
#' minimum.resistance = 0, 
#' maximum.resistance = 10000, 
#' sd.population.resistance = 10
#' )



plot_resistance_to_bioassay_survival = function(maximum.bioassay.survival.proportion = 1, 
                                                michaelis.menten.slope = 1, 
                                                half.population.bioassay.survival.resistance = 900, 
                                                nsim = 1000, 
                                                minimum.resistance = 0, 
                                                maximum.resistance = 25000, 
                                                sd.population.resistance = 10){
  
    df=data.frame(resistance.values=seq(minimum.resistance, maximum.resistance, by = 1))%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival.proportion = resistance_to_bioassay_survival(
      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion, 
      mean.population.resistance = resistance.values, 
      michaelis.menten.slope=michaelis.menten.slope, 
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance, 
      sd.population.resistance = sd.population.resistance,
      nsim=nsim)) ##plotting with sigma as 0.1 or 25 made no difference to plots
  
    ggplot(df, aes(x=resistance.values, y = bioassay.survival.proportion)) + ##logging to increase ease of readibility
    geom_point(colour = "blue") +
    xlab("Insecticide Resistance Intensity") + ##applying log(z) makes it more readable at the lower z levels.
    ylab("Bioassay Survival Proportion")+ #note: this label may need to be changed depending on what we calibrate against
    theme_classic()
}

#Note: This is being really slow at plotting.
