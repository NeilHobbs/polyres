#'
#' @import magrittr dplyr ggplot2
#' @importFrom magrittr %>%
#' @name %>%
#' 
#' @export
#'


plot_resistance_to_bioassay_survival = function(maximum.bioassay.survival.proportion, 
                                                michaelis.menton.slope, 
                                                half.population.bioassay.survival.resistance, 
                                                nsim, 
                                                minimum.resistance, 
                                                maximum.resistance, 
                                                sd.population.resistance){
  
    df=data.frame(resistance.values=seq(minimum.resistance, maximum.resistance, by = 1))%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival.proportion = resistance_to_bioassay_survival(
      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion, 
      mean.population.resistance = resistance.values, 
      michaelis.menton.slope=michaelis.menton.slope, 
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance, 
      sd.population.resistance = sd.population.resistance,
      nsim=nsim)) ##plotting with sigma as 0.1 or 25 made no difference to plots
  
    ggplot(df, aes(x=resistance.values, y = bioassay.survival.proportion)) + ##logging to increase ease of readibility
    geom_point(colour = "blue") +
    xlab("Insecticide Resistance") + ##applying log(z) makes it more readable at the lower z levels.
    ylab("Bioassay Survival Proportion")+ #note: this label may need to be changed depending on what we calibrate against
    theme_classic()
}

