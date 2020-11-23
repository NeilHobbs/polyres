

@imports

library(gridExtra)


temp.df = get_simulation_dataframe(run_simulation_intervention_test(number.of.insecticides = 2,
                                                               exposure.scaling.factor = 10,
                                                               nsim = 1,
                                                               minimum.insecticide.resistance.heritability = 0.30,
                                                               maximum.insecticide.resistance.heritability = 0.30,
                                                               minimum.male.insecticide.exposure = 0.9,
                                                               maximum.male.insecticide.exposure = 0.9,
                                                               minimum.female.insecticide.exposure = 0.9,
                                                               maximum.female.insecticide.exposure = 0.9,
                                                               resistance.cost = 0.1,
                                                               starting.treatment.site.intensity = 0,
                                                               starting.refugia.intensity = 0,
                                                               min.intervention.coverage = 0.9,
                                                               max.intervention.coverage = 0.9,
                                                               min.dispersal.rate = 0.5,
                                                               max.dispersal.rate = 0.5,
                                                               maximum.generations = 500,
                                                               irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                               half.population.bioassay.survival.resistance = 900,
                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case

),
                         number.of.insecticides = 2,
                         maximum.generations = 500)





plot_simulation = function(simulation.dataframe,
                           half.population.bioassay.survival.resistance,
                           withdrawal.threshold,
                           return.threshold){

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

temp.df.refugia = simulation.dataframe%>%
  dplyr::filter(site == "refugia")

#Create a plot for the treatment site: this will have the deployment sequence and the threshold lines.
treatment.plot = ggplot(temp.df.treatment, aes(x=time.in.generations, y = bioassay.survival, colour = insecticide.tracked,))+
  geom_point(aes(x=time.in.generations, y=(withdrawal.threshold*100), colour=insecticide.deployed, alpha = 0.3)) + #This needs to be an input: calculated from bioassay_survival_to_resistance()
  geom_point(aes(x=time.in.generations, y=(return.threshold * 100)), colour="grey", alpha = 0.3) + #This needs to be an input: calculated from bioassay_survival_to_resistance()
  geom_line(data =temp.df.treatment, aes(x=time.in.generations, y=bioassay.survival, colour=insecticide.tracked))+
  scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$bioassay.survival) < (withdrawal.threshold*100),
                                          yes = (withdrawal.threshold*100), no = max(simulation.dataframe$bioassay.survival) + 1)),
                     breaks = c(0, (return.threshold*100), (withdrawal.threshold*100)))+
  ylab("Survival in Bioassay (%)") +
  xlab("Time in Generations") +
  ggtitle("Intervention Site")+
  theme_classic()+
  theme(legend.position = "none")

#Create a plot for the refugia. Does not have deployment/threshold lines.
refugia.plot = ggplot(temp.df.refugia, aes(x=time.in.generations, y = bioassay.survival))+
  geom_line(data = temp.df.refugia, mapping = aes(x=time.in.generations, y=bioassay.survival, colour = insecticide.tracked)) +
  scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$bioassay.survival) < (withdrawal.threshold*100),
                                          yes = (withdrawal.threshold*100), no = max(simulation.dataframe$bioassay.survival) + 1)),
                     breaks = c(0, (return.threshold*100), (withdrawal.threshold*100)))+
  ylab(" ") +
  xlab("Time in Generations") +
  ggtitle("Refugia")+
  theme_classic()+
  theme(legend.position = "none")


return(gridExtra::grid.arrange(treatment.plot, refugia.plot, ncol=2))
}


plot_simulation(simulation.dataframe = temp.df,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05)
