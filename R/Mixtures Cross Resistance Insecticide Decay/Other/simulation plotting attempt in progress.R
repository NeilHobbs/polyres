#'@title Plot the Insecticide Resistance Management Simulation
#'
#'@description
#'
#'@import magrittr dplyr ggplot2
#'@importFrom magrittr %>%
#'@name %>%
#'
#'@param  simulation.dataframe The dataframe of the simulation created using the get_simualation_dataframe function
#'@param half.population.bioassay.survival.resistance The resistance intensity that gives 50% bioassay survival. This must be identical to the value used in the simulation.
#'@param withdrawal.threshold The bioassay survival proportion that leads to the withdrawal of the insecticide. This must be identitical to the value used in the simulation.
#'@param return.threshold The bioassay survival proportion that allows the return of the insecticide to the arsenal. This must be identical to the value used in the simulation.
#'
#'@return A two panel ggplot of the bioassay survival in the Intervention Site and Refugia. 
#'

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
