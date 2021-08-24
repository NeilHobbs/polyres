#Set 5: Mixtures, Cross Resistance

#read in previously used random parameters
parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[1:5000,2:7] #only need the first 5000 rows

cross.resistance = c(rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000), rep(0, 5000),
                     rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000)) #35,000

parameter.space.df = rbind(parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space,
                           parameter.space)

parameter.space.df= cbind(parameter.space.df, cross.resistance)


#Do the rotation Runs
temp.list.mixtures.set.5 = list()
for(v in 1:nrow(parameter.space.df)){
  
  temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
                                                                                                      exposure.scaling.factor = 10,
                                                                                                      nsim = 1,
                                                                                                      minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                                      maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                                      minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                      maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                      minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                      maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                      resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                                                      starting.treatment.site.intensity = 50,
                                                                                                      starting.refugia.intensity = 50,
                                                                                                      min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                      max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                      min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                      max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                      maximum.generations = 500,
                                                                                                      irm.deployment.strategy = "mixtures", #single, mixtures
                                                                                                      mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
                                                                                                      irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
                                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                                      deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                                      maximum.resistance.value = 25000,
                                                                                                      conversion.factor = 0.48,
                                                                                                      intercept = 0.15,
                                                                                                      min.cross.selection = parameter.space.df$cross.resistance[v],
                                                                                                      max.cross.selection = parameter.space.df$cross.resistance[v]), 
                                            maximum.generations = 500, number.of.insecticides = 2)                                              
  
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  #note, as in mixture will be the same for mixture part 1 and mixture part 2 - only count based on part1.
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = c(2)
  
  dep.freq = c(10)
  
  cross.selection = parameter.space.df$cross.resistance[v]
  
  average.resistance.intensity.1 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  average.resistance.intensity.2 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  strategy = "mixture"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq, cross.selection)
  
svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space.df))  
  
  temp.list.mixtures.set.5[[v]] = temp_2
}

mixture.df.set.5 = do.call(rbind, temp.list.mixtures.set.5)
mixture.df.set.5.1 = cbind(mixture.df.set.5, parameter.space.df)

write.csv(mixture.df.set.5.1, ".//mixtures.set.5.csv")
