#start out with some resistance present already
  #use same parameter space as previously
sim.output.df =read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/set_18_mixtures.csv")

regression.parameter.space = sim.output.df%>%
  dplyr::select("regresssion.intercept", "regression.coefficient")


temp.list.mixtures = list()
for(v in 1: nrow(regression.parameter.space)){
  
  temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures(number.of.insecticides = 2,
                                                                                      exposure.scaling.factor = 10,
                                                                                      nsim = 1,
                                                                                      minimum.insecticide.resistance.heritability = 0.3,
                                                                                      maximum.insecticide.resistance.heritability = 0.3,
                                                                                      minimum.male.insecticide.exposure = 1,
                                                                                      maximum.male.insecticide.exposure = 1,
                                                                                      minimum.female.insecticide.exposure = 1,
                                                                                      maximum.female.insecticide.exposure = 1,
                                                                                      resistance.cost = 0,
                                                                                      starting.treatment.site.intensity = 50,
                                                                                      starting.refugia.intensity = 50,
                                                                                      min.intervention.coverage = 1,
                                                                                      max.intervention.coverage = 1,
                                                                                      min.dispersal.rate = 0,
                                                                                      max.dispersal.rate = 0,
                                                                                      maximum.generations = 500,
                                                                                      irm.deployment.strategy = "mixtures", #single, mixtures
                                                                                      mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
                                                                                      irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                      deployment.frequency = 2, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                      maximum.resistance.value = 25000,
                                                                                      conversion.factor = regression.parameter.space$regression.coefficient[v],
                                                                                      intercept = regression.parameter.space$regresssion.intercept[v]), 
                                            maximum.generations = 500, number.of.insecticides = 2)                                              
  
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  average.resistance.intensity.1 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  average.resistance.intensity.2 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2,  
                      peak.resistance)
  
  temp.list.mixtures[[v]] = temp_2
}

mixture.df.regression = do.call(rbind, temp.list.mixtures)
mixture.df = cbind(mixture.df.regression, regression.parameter.space)

write.csv(mixture.df, ".//mixtures.field.bioassay.relationship.resistance.present.csv")
