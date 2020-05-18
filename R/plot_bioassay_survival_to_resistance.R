
plot_bioassay_survival_to_resistance = function(maximum.bioassay.survival.proportion,
                                                michaelis.menton.slope, 
                                                half.population.bioassay.survival.resistance, 
                                                estimate.precision, 
                                                sd.population.resistance, 
                                                nsim, 
                                                minimum.resistance.value, 
                                                maximum.resistance.value,
                                                minimum.bioassay.survival, 
                                                maximum.bioassay.survival, 
                                                divisions){
  
  df=data.frame(bioassay.survival.values=seq(minimum.bioassay.survival, maximum.bioassay.survival,
                                             by = divisions))%>%
    rowwise%>%
    mutate(resistance.values = bioassay_survival_to_resistance(
      maximum.bioassay.survival.proportion=maximum.bioassay.survival.proportion,
      michaelis.menton.slope=michaelis.menton.slope, 
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance, 
      bioassay.survival=bioassay.survival.values, 
      estimate.precision=estimate.precision, 
      sd.population.resistance=sd.population.resistance,
      nsim=nsim,
      minimum.resistance.value=minimum.resistance.value, 
      maximum.resistance.value=maximum.resistance.value)) ##plotting with sigma as 0.1 or 25 made no difference to plots
  
  ggplot(df, aes(x=resistance.values, y = bioassay.survival.values)) +
    geom_point(colour = "red") +
    xlab("Insecticide Resistance Intensity") +
    ylab("Bioassay Survival Proportion") +
    theme_classic()
}