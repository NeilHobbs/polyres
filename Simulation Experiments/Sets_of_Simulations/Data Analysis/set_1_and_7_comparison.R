Seq5 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "sequence", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.05,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)

Seq9 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "sequence", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.09,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)

Rot5 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "rotation", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.05,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)

Rot9 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "rotation", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.09,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)



plot_simulation_temp = function(simulation.dataframe, #created from the get_simulation_dataframe() function
                           half.population.bioassay.survival.resistance, #must be same as used in the simulation
                           withdrawal.threshold, #must be the same as used in the simulation
                           return.threshold,
                           plot.title){ #must be the same as used in the simulation
  
  
  #Convert the insecticide resistance intensity into bioassay survival(%).
  #This is more intuitive and operationally relevant to visualise.
  simulation.dataframe = simulation.dataframe%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                      mean.population.resistance = resistance.intensity,
                                                                      michaelis.menten.slope = 1, 
                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                      sd.population.resistance = 0, 
                                                                      nsim = 1) * 100)#As a percentage)
  #Make as separate dataframes: 1 for treatment, 1 for refugia
  temp.df.treatment = simulation.dataframe%>%
    dplyr::filter(site == "treatment")
  

  #Create a plot for the treatment site: this will have the deployment sequence and the threshold lines.
  treatment.plot = ggplot(data = temp.df.treatment, aes(x=time.in.generations, 
                                                        y = bioassay.survival, 
                                                        colour = insecticide.tracked,))+
    geom_point(aes(x=time.in.generations, y=(withdrawal.threshold*100), #Make a line of the deployed insecticide at %
                   colour=insecticide.deployed, #colours should match the plots
                   alpha = 0.3)) +
    geom_point(aes(x=time.in.generations, #Line indicating the return threshold.
                   y=(return.threshold * 100)), 
               colour="grey", 
               alpha = 0.3) +  #keep it fairly faint.
    geom_line(data =temp.df.treatment, aes(x=time.in.generations, 
                                           y=bioassay.survival, #aleady in %
                                           colour=insecticide.tracked))+ #matches deployed colour
    scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$bioassay.survival) < (withdrawal.threshold*100),
                                            yes = (withdrawal.threshold*100), no = max(simulation.dataframe$bioassay.survival) + 1)),
                       breaks = c(0, (return.threshold*100), (withdrawal.threshold*100)))+ #make sure there are labels at the important bits
    xlim(0, 500)+
    ylab("Survival in Bioassay (%)") +
    xlab("Time in Generations") +
    ggtitle(plot.title)+
    theme_classic()+
    theme(legend.position = "none") #Colour label is irrelevant; insecticide number can be inferred from the order of deployment
  

  
  return(treatment.plot)
}

seq5.plot = plot_simulation_temp(simulation.dataframe = Seq5,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05,
                plot.title = "A")

seq9.plot = plot_simulation_temp(simulation.dataframe = Seq9,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.09,
                plot.title = "B")

rot5.plot = plot_simulation_temp(simulation.dataframe = Rot5,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05,
                plot.title = "C")

rot9.plot = plot_simulation_temp(simulation.dataframe = Rot9,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.09,
                plot.title = "D")


gridExtra::grid.arrange(seq5.plot, seq9.plot,
                        rot5.plot, rot9.plot)
