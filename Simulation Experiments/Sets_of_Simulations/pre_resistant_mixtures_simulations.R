#Create the input parameters for the simulations::::
library(devtools)
load_all()
#Insecticide Efficacy
#Start Resistance
#Insecticide Decay Rates
basal.decay.rate.novel = rep(c(rep(0, 5000),
                           rep(0.015, 5000),
                           rep(0.025, 5000),
                           rep(0.025, 5000),
                           rep(0.025, 5000),
                           rep(0.005, 5000),
                           rep(0.005, 5000),
                           rep(0.005, 5000),
                           rep(0.015, 5000),
                           rep(0.015, 5000)), 8)

rapid.decay.rate.novel = rep(c(rep(0, 5000), rep(0.08, 45000)), 8)

threshold.generation.novel = rep(c(rep(15, 5000), 
                               rep(15, 5000),
                               rep(15, 5000),
                               rep(20, 5000),
                               rep(10, 5000),
                               rep(15, 5000),
                               rep(20, 5000),
                               rep(10, 5000),
                               rep(10, 5000),
                               rep(20, 5000)), 8)

basal.decay.rate.old = rep(c(rep(0, 5000), rep(0.015, 45000)), 8)
rapid.decay.rate.old = rep(c(rep(0, 5000), rep(0.08, 45000)), 8)
threshold.generation.old = rep(rep(15, 50000), 8)

start.resistance.old = c(rep(0, 50000),
                         rep(18, 50000),
                         rep(47, 50000),
                         rep(100, 50000),
                         rep(225, 50000),
                         rep(900, 50000),
                         rep(3600, 50000),
                         rep(8100, 50000))

parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[1:5000,2:7]

parameter.space.df = dplyr::bind_rows(replicate(80, parameter.space, simplify = FALSE))

#ID1	Novel AI full dose alone
temp.ID1= c()
for(v in 1:nrow(parameter.space.df)){
  
  temp.ID1[v] = get_simulation_dataframe(run_simulation_intervention_special_cases(number.of.insecticides = 1,
                                                                             exposure.scaling.factor = 10,
                                                                             nsim = 1,
                                                                             minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                             maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                             minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                             maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                             minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                             maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                             resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                             starting.treatment.site.intensity = 0,
                                                                             starting.refugia.intensity = 0,
                                                                             min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                             max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                             min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                             max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                             maximum.generations = 500, #appoximately 50 years
                                                                             irm.strategy = "sequence", 
                                                                             half.population.bioassay.survival.resistance = 900, 
                                                                             withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                             return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                             deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                             maximum.resistance.value = 80000, #have arbitrarily high just in case
                                                                             min.cross.selection = 0,
                                                                             max.cross.selection = 0,
                                                                             applied.insecticide.dose = 1, #deployment is a ratio.
                                                                             recommended.insecticide.dose = 1,
                                                                             threshold.generation = threshold.generation.novel[v],
                                                                             base.efficacy.decay.rate = basal.decay.rate.novel[v], #default estimated from Toe et al
                                                                             rapid.decay.rate = rapid.decay.rate.novel[v], #default estimated from Toe et al
                                                                             intercept = 0.15,
                                                                             conversion.factor = 0.48,
                                                                             insecticide.suppression = FALSE
  ) , 500, 1)%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::summarise(max(time.in.generations))
  
  if(v %% 5 == 0){print(v)}
  
}

ID1.Duration = c(unlist(temp.ID1))

parameter.space.df$ID1.Duration = ID1.Duration


write.csv(parameter.space.df, ".//ID1.csv")


#ID2	Mixture full dose Novel AI with full dose “old” AI
temp.ID2= c()
for(v in 1:nrow(parameter.space.df)){
  
  temp.ID2[v] = get_simulation_dataframe_mixtures(run_simulation_intervention_mixtures_special_cases(number.of.insecticides = 2,
                                                                                   exposure.scaling.factor = 10,
                                                                                   nsim = 1,
                                                                                   insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                   minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                   maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                   minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                   maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                   resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                                   starting.treatment.site.intensity = c(0, start.resistance.old[v]),
                                                                                   starting.refugia.intensity = c(0, start.resistance.old[v]),
                                                                                   min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                   max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                   min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                   max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                   maximum.generations = 500, #appoximately 50 years
                                                                                   irm.deployment.strategy = "mixtures", 
                                                                                   mixture.strategy = "mix.sequential.discrete", 
                                                                                   irm.switch.strategy = "insecticide.1", 
                                                                                   half.population.bioassay.survival.resistance = 900, 
                                                                                   withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                   return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                   deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                   maximum.resistance.value = 80000, #have arbitrarily high just in case
                                                                                   min.cross.selection = 0,
                                                                                   max.cross.selection = 0,
                                                                                   applied.insecticide.dose = 1, #deployment is a ratio.
                                                                                   recommended.insecticide.dose = 1,
                                                                                   threshold.generation = c(threshold.generation.novel[v], threshold.generation.old[v]),
                                                                                   base.efficacy.decay.rate = c(basal.decay.rate.novel[v], basal.decay.rate.old[v]), #default estimated from Toe et al
                                                                                   rapid.decay.rate = c(rapid.decay.rate.novel[v], rapid.decay.rate.old[v]), #default estimated from Toe et al
                                                                                   intercept = 0.15,
                                                                                   conversion.factor = 0.48,
                                                                                   insecticide.suppression = FALSE
  ) , 500, 2)%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::summarise(max(time.in.generations))
  
  if(v %% 5 == 0){print(v)}
  
}

ID2.Duration = c(unlist(temp.ID2))

parameter.space.df$ID2.Duration = ID2.Duration


write.csv(parameter.space.df, ".//ID2.csv")

#ID3	Mixture half dose Novel AI with full dose “old” AI

temp.ID3= c()
for(v in 1:nrow(parameter.space.df)){
  
  temp = get_simulation_dataframe_mixtures(run_simulation_intervention_mixtures_special_cases(number.of.insecticides = 2,
                                                                                                     exposure.scaling.factor = 10,
                                                                                                     nsim = 1,
                                                                                                     insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                                     minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                     maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                     minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                     maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                     resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                                                     starting.treatment.site.intensity = c(0, start.resistance.old[v]),
                                                                                                     starting.refugia.intensity = c(0, start.resistance.old[v]),
                                                                                                     min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                     max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                     min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                     max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                     maximum.generations = 500, #appoximately 50 years
                                                                                                     irm.deployment.strategy = "mixtures", 
                                                                                                     mixture.strategy = "mix.sequential.discrete", 
                                                                                                     irm.switch.strategy = "insecticide.1", 
                                                                                                     half.population.bioassay.survival.resistance = 900, 
                                                                                                     withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                                     return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                                     deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                                     maximum.resistance.value = 80000, #have arbitrarily high just in case
                                                                                                     min.cross.selection = 0,
                                                                                                     max.cross.selection = 0,
                                                                                                     applied.insecticide.dose = c(0.5, 1), #deployment is a ratio.
                                                                                                     recommended.insecticide.dose = 1,
                                                                                                     threshold.generation = c(threshold.generation.novel[v], threshold.generation.old[v]),
                                                                                                     base.efficacy.decay.rate = c(basal.decay.rate.novel[v], basal.decay.rate.old[v]), #default estimated from Toe et al
                                                                                                     rapid.decay.rate = c(rapid.decay.rate.novel[v], rapid.decay.rate.old[v]), #default estimated from Toe et al
                                                                                                     intercept = 0.15,
                                                                                                     conversion.factor = 0.48,
                                                                                                     insecticide.suppression = FALSE
  ) , 500, 2)
  
  
  temp.ID3[v] = max(temp$time.in.generations)
  
  if(v %% 5 == 0){print(v)}
  
}

ID3.Duration = c(unlist(temp.ID3))

parameter.space.df$ID3.Duration = ID3.Duration


write.csv(parameter.space.df, ".//ID3.csv")


#ID4	Mixture half dose Novel with half dose “old” AI



#ID5	Mixture full dose Novel AI with half dose “old” AI



#ID6	Novel AI half dose alone.






