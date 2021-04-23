#Set 5: sequence, Cross Resistance, 50 starting resistance.

#read in previously used random parameters
parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[1:5000,2:9] #only need the first 5000 rows

cross.resistance = c(rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000), rep(0, 5000),
                     rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000)) #35,000


parameter.space.df = rbind(parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space,
                           parameter.space)

parameter.space.df= cbind(parameter.space.df, cross.resistance)


#Do the sequence Runs
temp.list.sequence.cross.set.5 = list()
for(v in 1: nrow(parameter.space.df)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
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
                                                                               maximum.generations = 500, #appoximately 50 years
                                                                               irm.strategy = "sequence", 
                                                                               half.population.bioassay.survival.resistance = 900, 
                                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                               maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                               min.cross.selection = parameter.space.df$cross.resistance[v],
                                                                               max.cross.selection = parameter.space.df$cross.resistance[v]), 
                                   maximum.generations = 500, number.of.insecticides = 2)                                              
  
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = c(2)
  
  dep.freq = c(10)
  
  cross.resistance = parameter.space.df$cross.resistance[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "sequence"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
                      cross.resistance)
  
svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                 max.value = nrow(parameter.space.df))  
  
  temp.list.sequence.cross.set.5[[v]] = temp_2
}

sequence.df.cross = do.call(rbind, temp.list.sequence.cross.set.5)
sequence.df.set.5 = cbind(sequence.df.cross, parameter.space.df)

write.csv(sequence.df.set.5, ".//sequence.set.5.csv")

