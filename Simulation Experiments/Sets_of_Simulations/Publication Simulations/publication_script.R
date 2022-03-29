#Document for the running of simulations and data analysis used for publication:

#install required packages if necessary

#load required packages : packages can be installed: install.packages("packagename")
library(magrittr)
library(ggplot2)
library(ggridges)
library(dplyr)
library(patchwork)
library(dplyr)
library(RColorBrewer)
library(ggpubr)
library(epiR)
library(mgcv)
library(MASS)
library(devtools) #for polyres package [will need to figure out how to allow install from github]
load_all()

############################################
## 1. Setting Up and Running Simulations  ##
############################################


# ##Step 1: Create a data frame for the parameter space: ##hashed out to prevent re-sampling
# 
# parameter.space.df = data.frame(lhs::randomLHS(5000, 6)) #20,000 random samples of the 6 input parameters.
# 
# 
# #Rename and sample from the uniform distributions
# parameter.space.df = parameter.space.df%>%
#   dplyr::rename(Heritability = X1)%>%
#   dplyr::rename(`Male Insecticide Exposure` = X2)%>%
#   dplyr::rename(`Female Insecticide Exposure` = X3)%>%
#   dplyr::rename(`Fitness Cost` = X4)%>%
#   dplyr::rename(`Intervention Coverage` = X5)%>%
#   dplyr::rename(Dispersal = X6)%>%
#   dplyr::mutate(Heritability = qunif(Heritability, 0.05, 0.3))%>%
#   dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))%>% #
#   dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))%>% #Defaults from Ian
#   dplyr::mutate(`Fitness Cost` = qunif(`Fitness Cost`, 0.01, 0.2))%>%
#   dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.1, 0.9))%>%
#   dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))
# 
# #use cross selection values of -0.5 to 0.5 (by 0.1)
# cross.selection.values = seq(-0.5, 0.5, 0.1)
# cross.selection.values = rep(cross.selection.values, 5000)
# cross.selection.values = sort(cross.selection.values, decreasing = TRUE)
# start.resistance.values = c(rep(0, 55000), rep(50, 55000))
# cross.selection.values = c(cross.selection.values, cross.selection.values)
# 
# parameter.space.df = do.call("rbind", replicate(22, parameter.space.df, simplify = FALSE))
# parameter.space.df$cross.selection.values = cross.selection.values
# parameter.space.df$start.resistance.values = start.resistance.values
# 
# 
# write.csv(parameter.space.df, "paramater.space.df.publication.csv")
# 
# #read in the dataset
#parameter.space.df = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/paramater.space.df.publication.csv")
# 
# 
# #When running simulations we extract the:
#     # -Primary Outcome = Simulation Duration
#     # -Secondary Outcome = Mean resisance to deployed insecticide
#     # -Secondary Outcome = Control failure generations (deployed and not deployed)
#     # -Secondary Outcome = Peak Resistance.
# 
# 
# #Do the sequence runs: run in chunks of 40,000
# 
# 
# temp.list.sequence = list()
# 
# for(v in 1:nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "sequence",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   strategy = "sequence"
# 
#   dep.freq = c(10)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
# 
#   strategy = "sequence"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
#                       cross.selection, start.resistance)
# 
#   temp.list.sequence[[v]] = temp_2
# }
# 
# sequence.df = do.call(rbind, temp.list.sequence)
# sequence.df.publication = cbind(sequence.df, parameter.space.df)
# 
# write.csv(sequence.df.publication, ".//sequence.set.publication.csv")
# 
# 
# #Do the Rotation Runs
# temp.list.rotation = list()
# for(v in 1: nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "rotation",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   dep.freq = c(10)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
# 
#   strategy = "rotation"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
#                       cross.selection, start.resistance)
# 
#   svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
#                    max.value = nrow(parameter.space.df))
# 
#   temp.list.rotation[[v]] = temp_2
# }
# 
# rotation.df = do.call(rbind, temp.list.rotation)
# rotation.df.publication = cbind(rotation.df, parameter.space.df)
# 
# write.csv(rotation.df.publication, ".//rotation.set.publication.csv")
# 
# 
# #Then do mixtures::
# temp.list.mixtures = list()
# for(v in 1:nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
#                                                                                                       exposure.scaling.factor = 10,
#                                                                                                       nsim = 1,
#                                                                                                       minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                                       maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                                       minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                                       maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                                       minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                                       maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                                       resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                                       starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                                       starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                                       min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                                       max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                                       min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                                       max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                                       maximum.generations = 500,
#                                                                                                       irm.deployment.strategy = "mixtures", #single, mixtures
#                                                                                                       mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
#                                                                                                       irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
#                                                                                                       half.population.bioassay.survival.resistance = 900,
#                                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                                       return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                                       deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                                       maximum.resistance.value = 25000,
#                                                                                                       conversion.factor = 0.48, #point estimates from linear regression
#                                                                                                       intercept = 0.15, #point estimates from linear regression
#                                                                                                       min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                                       max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                             maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   #note, as in mixture will be the same for mixture part 1 and mixture part 2 - only count based on part1.
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   dep.freq = c(10)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity.1 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   average.resistance.intensity.2 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   strategy = "mixture"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq, cross.selection, start.resistance)
# 
#   svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
#                    max.value = nrow(parameter.space.df))
# 
#   temp.list.mixtures[[v]] = temp_2
# }
# 
# mixture.df = do.call(rbind, temp.list.mixtures)
# mixture.df.publication = cbind(mixture.df, parameter.space.df)
# 
# write.csv(mixture.df.publication, ".//mixture.df.publication.csv")


    ##The simulation running has been been deliberately hashed out. To run highlight lines 23 to 303.
      #then press crtl+shft+c and this should unhash all those lines. 
      #The simulations can take a long time time to run. We therefore letting each 
      #strategy run separately after each has finished. Depending on your computer,
      #it may be worth running each for loop in chunks of say 50000. 
      

##RUN WITH A 30 GENERATION DEPLOYMENT INTERVAL - For LLINs

# temp.list.sequence.30 = list()
# 
# for(v in 1:nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "sequence",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   strategy = "sequence"
# 
#   dep.freq = c(30)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
# 
#   strategy = "sequence"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
#                       cross.selection, start.resistance)
# 
#   temp.list.sequence.30[[v]] = temp_2
# }
# 
# sequence.df.30 = do.call(rbind, temp.list.sequence.30)
# sequence.df.30.publication = cbind(sequence.df.30, parameter.space.df)
# 
# write.csv(sequence.df.30.publication, ".//sequence.set.publication.30.csv")


#Do the Rotation Runs
# temp.list.rotation.30 = list()
# for(v in 1: nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "rotation",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   dep.freq = c(30)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
# 
#   strategy = "rotation"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
#                       cross.selection, start.resistance)
# 
#   svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
#                    max.value = nrow(parameter.space.df))
# 
#   temp.list.rotation.30[[v]] = temp_2
# }
# 
# rotation.df.30 = do.call(rbind, temp.list.rotation.30)
# rotation.df.publication.30 = cbind(rotation.df.30, parameter.space.df)
# 
# write.csv(rotation.df.publication.30, ".//rotation.set.publication.30.csv")
# 

#Then do mixtures::
# temp.list.mixtures.30 = list()
# for(v in 1:nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
#                                                                                                       exposure.scaling.factor = 10,
#                                                                                                       nsim = 1,
#                                                                                                       minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                                       maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                                       minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                                       maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                                       minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                                       maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                                       resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                                       starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                                       starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                                       min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                                       max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                                       min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                                       max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                                       maximum.generations = 500,
#                                                                                                       irm.deployment.strategy = "mixtures", #single, mixtures
#                                                                                                       mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
#                                                                                                       irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
#                                                                                                       half.population.bioassay.survival.resistance = 900,
#                                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                                       return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                                       deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                                       maximum.resistance.value = 25000,
#                                                                                                       conversion.factor = 0.48, #point estimates from linear regression
#                                                                                                       intercept = 0.15, #point estimates from linear regression
#                                                                                                       min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                                       max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                             maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   #note, as in mixture will be the same for mixture part 1 and mixture part 2 - only count based on part1.
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   dep.freq = c(30)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity.1 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   average.resistance.intensity.2 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   strategy = "mixture"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq, cross.selection, start.resistance)
# 
#   svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
#                    max.value = nrow(parameter.space.df))
# 
#   temp.list.mixtures.30[[v]] = temp_2
# }
# 
# mixture.df.30 = do.call(rbind, temp.list.mixtures.30)
# mixture.df.30.publication = cbind(mixture.df.30, parameter.space.df)
# 
# write.csv(mixture.df.30.publication, ".//mixture.df.publication.30.csv")

##Run with unique insecticide properties (heritability and fitness):
#This increases the amount of noise in the system (more field realistic)

parameter.space.df = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/paramater.space.df.publication.csv")


# parameter.space.2.df = data.frame(lhs::randomLHS(50000, 11)) #50,000 random samples of the 6 input parameters.
# 
# 
# #Rename and sample from the uniform distributions
# parameter.space.2.df = parameter.space.2.df%>%
#   dplyr::rename(Heritability.1 = X1)%>%
#   dplyr::rename(Heritability.2 = X2)%>%
#   dplyr::mutate(Heritability.1 = qunif(Heritability.1, 0.05, 0.3))%>%
#   dplyr::mutate(Heritability.2 = qunif(Heritability.2, 0.05, 0.3))%>%
#   dplyr::rename(Fitness.1 = X3)%>%
#   dplyr::rename(Fitness.2 = X4)%>%
#   dplyr::mutate(Fitness.1 = qunif(Fitness.1, 0.01, 0.2))%>%
#   dplyr::mutate(Fitness.2 = qunif(Fitness.2, 0.01, 0.2))%>%
#   dplyr::rename(Start.1 = X5)%>%
#   dplyr::rename(Start.2 = X6)%>%
#   dplyr::mutate(Start.1 = qunif(Start.1, 0, 80))%>%
#   dplyr::mutate(Start.2 = qunif(Start.2, 0, 80))%>%
#   dplyr::rename(Cross.Selection = X7)%>%
#   dplyr::mutate(Cross.Selection = qunif(Cross.Selection, -0.5, 0.5))%>%
#   dplyr::rename(Dispersal = X8)%>%
#   dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))%>%
#   dplyr::rename(Coverage = X9)%>%
#   dplyr::mutate(Coverage = qunif(Coverage, 0.1, 0.9))%>%
#   dplyr::rename(Female.Exposure = X10)%>%
#   dplyr::mutate(Female.Exposure = qunif(Female.Exposure, 0.4, 0.9))%>%
#   dplyr::rename(Male.Exposure = X11)%>%
#   dplyr::mutate(Male.Exposure = qunif(Male.Exposure, 0, 1))
# 
# write.csv(parameter.space.2.df, ".//unique.insecticide.properties.parameter.space.publication.csv")

parameter.space.2.df = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/unique.insecticide.properties.parameter.space.publication.csv")

#group the values into lists of vectors
heritability.list = list()
fitness.list = list()
start.resistance.list = list()

for(i in 1:50000){
  
  heritability.list[[i]] = c(parameter.space.2.df$Heritability.1[i], parameter.space.2.df$Heritability.2[i])
  start.resistance.list[[i]] = c(parameter.space.2.df$Start.1[i], parameter.space.2.df$Start.2[i])
  fitness.list[[i]] = c(parameter.space.2.df$Fitness.1[i], parameter.space.2.df$Fitness.2[i])
}


# #Perfect Sequences
# 
# temp.list.sequence.unique = list()
# 
# for(v in 1:50000){
# 
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = c(heritability.list[[v]]),
#                                                                                maximum.insecticide.resistance.heritability = c(heritability.list[[v]]),
#                                                                                minimum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                resistance.cost = c(fitness.list[[v]]),
#                                                                                starting.treatment.site.intensity = c(start.resistance.list[[v]]),
#                                                                                starting.refugia.intensity = c(start.resistance.list[[v]]),
#                                                                                min.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "sequence",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.2.df$Cross.Selection[v],
#                                                                                max.cross.selection = parameter.space.2.df$Cross.Selection[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   strategy = "sequence"
# 
#   dep.freq = c(10)
# 
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
# 
#   strategy = "sequence"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, dep.freq)
# 
#   temp.list.sequence.unique[[v]] = temp_2
# }
# 
# sequence.df.unique = do.call(rbind, temp.list.sequence.unique)
# sequence.df.unique.ps = cbind(sequence.df.unique, parameter.space.2.df)
# 
# write.csv(sequence.df.unique.ps, ".//unique.insecticides.sequences.csv")
# 
# 
# #Standard Rotations
# temp.list.rotation.unique = list()
# 
# for(v in 1:50000){
#   
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = c(heritability.list[[v]]),
#                                                                                maximum.insecticide.resistance.heritability = c(heritability.list[[v]]),
#                                                                                minimum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                resistance.cost = c(fitness.list[[v]]),
#                                                                                starting.treatment.site.intensity = c(start.resistance.list[[v]]),
#                                                                                starting.refugia.intensity = c(start.resistance.list[[v]]),
#                                                                                min.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "rotation",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.2.df$Cross.Selection[v],
#                                                                                max.cross.selection = parameter.space.2.df$Cross.Selection[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
#   
#   
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
#   
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
#   
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
#   
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
#   
#   peak.resistance = max(temp_treatment$resistance.intensity)
#   
#   strategy = "rotation"
#   
#   dep.freq = c(10)
#   
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
#   
#    temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, dep.freq)
#   
#    temp.list.rotation.unique[[v]] = temp_2
# }
# 
# rotation.df.unique = do.call(rbind, temp.list.rotation.unique)
# rotation.df.unique.ps = cbind(rotation.df.unique, parameter.space.2.df)
# 
# write.csv(rotation.df.unique.ps, ".//unique.insecticides.rotation.csv")
# 
# #Adapative Rotations (become sequences when necessary)
# temp.list.adapative.rotation.unique = list()
# 
# for(v in 1:50000){
#   
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = c(heritability.list[[v]]),
#                                                                                maximum.insecticide.resistance.heritability = c(heritability.list[[v]]),
#                                                                                minimum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                resistance.cost = c(fitness.list[[v]]),
#                                                                                starting.treatment.site.intensity = c(start.resistance.list[[v]]),
#                                                                                starting.refugia.intensity = c(start.resistance.list[[v]]),
#                                                                                min.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "adaptive.rotation",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.2.df$Cross.Selection[v],
#                                                                                max.cross.selection = parameter.space.2.df$Cross.Selection[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
#   
#   
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
#   
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
#   
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
#   
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
#   
#   peak.resistance = max(temp_treatment$resistance.intensity)
#   
#   strategy = "adaptive.rotation"
#   
#   dep.freq = c(10)
#   
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
#   
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, dep.freq)
#   
#   temp.list.adapative.rotation.unique[[v]] = temp_2
# }
# 
# adaptive.rotation.df.unique = do.call(rbind, temp.list.adapative.rotation.unique)
# adaptive.rotation.df.unique.ps = cbind(adaptive.rotation.df.unique, parameter.space.2.df)
# 
# write.csv(adaptive.rotation.df.unique.ps, ".//unique.insecticides.adaptive.rotation.csv")

#Mixtures




# do mixtures::
# temp.list.mixtures.unique = list()
# for(v in 1:50000){
# 
#   temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
#                                                                                                       exposure.scaling.factor = 10,
#                                                                                                       nsim = 1,
#                                                                                                       minimum.insecticide.resistance.heritability = heritability.list[[v]],
#                                                                                                       maximum.insecticide.resistance.heritability = heritability.list[[v]],
#                                                                                                       minimum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                                       maximum.male.insecticide.exposure = parameter.space.2.df$Male.Exposure[v],
#                                                                                                       minimum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                                       maximum.female.insecticide.exposure = parameter.space.2.df$Female.Exposure[v],
#                                                                                                       resistance.cost = fitness.list[[v]],
#                                                                                                       starting.treatment.site.intensity = start.resistance.list[[v]],
#                                                                                                       starting.refugia.intensity = start.resistance.list[[v]],
#                                                                                                       min.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                                       max.intervention.coverage = parameter.space.2.df$Coverage[v],
#                                                                                                       min.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                                       max.dispersal.rate = parameter.space.2.df$Dispersal[v],
#                                                                                                       maximum.generations = 500,
#                                                                                                       irm.deployment.strategy = "mixtures", #single, mixtures
#                                                                                                       mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
#                                                                                                       irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
#                                                                                                       half.population.bioassay.survival.resistance = 900,
#                                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                                       return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                                       deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                                       maximum.resistance.value = 25000,
#                                                                                                       conversion.factor = 0.48, #point estimates from linear regression
#                                                                                                       intercept = 0.15, #point estimates from linear regression
#                                                                                                       min.cross.selection = parameter.space.2.df$Cross.Selection[v],
#                                                                                                       max.cross.selection = parameter.space.2.df$Cross.Selection[v]),
#                                             maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   #note, as in mixture will be the same for mixture part 1 and mixture part 2 - only count based on part1.
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   average.resistance.intensity.1 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   average.resistance.intensity.2 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   strategy = "mixture"
# 
#   dep.freq = c(10)
#   
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)
# 
#   svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
#                    max.value = 50000)
# 
#   temp.list.mixtures.unique[[v]] = temp_2
# }
# 
# mixture.df.unique = do.call(rbind, temp.list.mixtures.unique)
# mixture.df.unique.publication = cbind(mixture.df.unique, parameter.space.2.df)
# 
# write.csv(mixture.df.unique.publication, ".//mixture.df.unique.publication.csv")

#Adaptive mixtures becomes single insecticide sequence when necessary



########################################
## 2. DATA VISUALISATION              ##
########################################

#Make plot of the polygenic resistance score (and its conversion to field survival)

polygenic.resistance.values = seq(0, 900, by = 0.1)
bioassay.values = c()
for(i in 1:length(polygenic.resistance.values)){
bioassay.values[i] = resistance_to_bioassay_survival(mean.population.resistance = polygenic.resistance.values[i],
                                                     nsim = 1,
                                                     sd.population.resistance = 0, #measure without error,
                                                     michaelis.menten.slope = 1,
                                                     maximum.bioassay.survival.proportion = 1)
}

field.survival = c()
for(i in 1:length(bioassay.values)){
  
  field.survival[i] = convert_bioassay_survival_to_field(bioassay.survival = bioassay.values[i],
                                                         conversion.factor = 0.48,
                                                         intercept = 0.15)
  
}



#Figure 1
example.df = data.frame(polygenic.resistance.values, bioassay.values, field.survival)


ggplot(example.df, aes(x=polygenic.resistance.values,
                       y = bioassay.values))+
  geom_line(aes(x=polygenic.resistance.values,
                 y=bioassay.values*100),
             colour = "black",
            size = 3,
            alpha = 0.7)+
  geom_line(aes(x=polygenic.resistance.values,
                 y=field.survival*100),
             colour = "green",
            size = 3,
            alpha = 0.7)+
  geom_segment(aes(x=0, y=10, xend=100, yend=10),
               linetype = "dashed",
               size = 1.5,
               colour = "blue")+
  geom_segment(aes(x=100, y=0, xend=100, yend=10),
               linetype = "dashed",
               size = 1.5,
               colour = "blue")+
  geom_segment(aes(x=0, y=50, xend=900, yend=50),
               linetype = "dashed",
               size = 1.5,
               colour = "orange")+
  geom_segment(aes(x=900, y=0, xend=900, yend=50),
               linetype = "dashed",
               size = 1.5,
               colour = "orange")+
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700, 900))+
  xlab("Mean Population Polygenic Resistance Score")+
  ylab("Survival (%)")+
  theme_classic()+
  theme(text = element_text(size=20))


## Read in the data sets
##Read in the datasets::
#10 generation deployments
sequence.df.10 = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/sequence.set.publication.csv")
rotation.df.10 = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/rotation.set.publication.csv")
mixture.df.10 = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/mixture.df.publication.csv")

#30 generation deployments 
sequence.df.30 = read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/sequence.set.publication.30.csv")
rotation.df.30 = read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/rotation.set.publication.30.csv")
mixture.df.30 = read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/mixture.df.publication.30.csv")

#unique insecticides
sequence.df.unique = read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/unique.insecticides.sequences.csv")
rotation.df.unique = read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/unique.insecticides.rotation.csv")
adaptive.rotation.df.unique = read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/unique.insecticides.adaptive.rotation.csv")
mixture.df.unique = read.csv(("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/mixture.df.unique.publication.csv"))

#parameter spaces
parameter.space.10.30 = read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/paramater.space.df.publication.csv")
parameter.space.unique =read.csv("C:/Users/neilp/OneDrive - LSTM/polyres/Simulation Experiments/Sets_of_Simulations/Publication Simulations/unique.insecticide.properties.parameter.space.publication.csv")


#Analysis Part 1: Descriptive analysis
mean(c(sequence.df.10$simulation.duration, sequence.df.30$simulation.duration))
mean(c(rotation.df.10$simulation.duration, rotation.df.30$simulation.duration))
mean(c(mixture.df.10$simulation.duration, mixture.df.30$simulation.duration))


min(sequence.df.10$simulation.duration, sequence.df.30$simulation.duration)
min(rotation.df.10$simulation.duration, rotation.df.30$simulation.duration)
min(mixture.df.10$simulation.duration, mixture.df.30$simulation.duration)

#Unique insecticides descriptive analysis:

mean(sequence.df.unique$simulation.duration)
mean(rotation.df.unique$simulation.duration)
mean(mixture.df.unique$simulation.duration)
mean(adaptive.rotation.df.unique$simulation.duration)

min(sequence.df.unique$simulation.duration)
min(rotation.df.unique$simulation.duration)
min(mixture.df.unique$simulation.duration)
min(adaptive.rotation.df.unique$simulation.duration)



###Analysis Part 1: Sequences vs Rotations:

#When insecticides are only available as single insecticide formulations is it better to deploy in sequence or as rotations.

seq.rot.10.difference.raw = sequence.df.10$simulation.duration - rotation.df.10$simulation.duration
seq.rot.10.difference.proportion =  1 - (sequence.df.10$simulation.duration / rotation.df.10$simulation.duration)

seq.rot.30.difference.raw = sequence.df.30$simulation.duration - rotation.df.10$simulation.duration
seq.rot.30.difference.proportion = 1 - (sequence.df.30$simulation.duration / rotation.df.30$simulation.duration)

rot.seq.10.outcome = c()
for(i in 1:110000){
  if(rotation.df.10$simulation.duration[i] > sequence.df.10$simulation.duration[i]){rot.seq.10.outcome[i] = "rotation.win"}
  if(sequence.df.10$simulation.duration[i] > rotation.df.10$simulation.duration[i]){rot.seq.10.outcome[i] = "sequence.win"}
  if(rotation.df.10$simulation.duration[i] == sequence.df.10$simulation.duration[i]){rot.seq.10.outcome[i] = "draw"}
}

table(rot.seq.10.outcome)

rot.seq.10.operational.outcome = c()
for(i in 1:110000){
  if(seq.rot.10.difference.proportion[i] >= 0.1){rot.seq.10.operational.outcome[i]  = "rotation.operational.win"}
  if(seq.rot.10.difference.proportion[i] <= -0.1){rot.seq.10.operational.outcome[i]  = "sequence.operational.win"}
  if(seq.rot.10.difference.proportion[i] < 0.1 &
     seq.rot.10.difference.proportion[i] > -0.1){rot.seq.10.operational.outcome[i] = "no.operational.win"}
}

table(rot.seq.10.operational.outcome)


rot.seq.30.outcome = c()
for(i in 1:110000){
  if(rotation.df.30$simulation.duration[i] > sequence.df.30$simulation.duration[i]){rot.seq.30.outcome[i] = "rotation.win"}
  if(sequence.df.30$simulation.duration[i] > rotation.df.30$simulation.duration[i]){rot.seq.30.outcome[i] = "sequence.win"}
  if(rotation.df.30$simulation.duration[i] == sequence.df.30$simulation.duration[i]){rot.seq.30.outcome[i] = "draw"}
}

table(rot.seq.30.outcome)

rot.seq.30.operational.outcome = c()
for(i in 1:110000){
  if(seq.rot.30.difference.proportion[i] >= 0.1){rot.seq.30.operational.outcome[i]  = "rotation.operational.win"}
  if(seq.rot.30.difference.proportion[i] <= -0.1){rot.seq.30.operational.outcome[i]  = "sequence.operational.win"}
  if(seq.rot.30.difference.proportion[i] < 0.1 &
     seq.rot.30.difference.proportion[i] > -0.1){rot.seq.30.operational.outcome[i] = "no.operational.win"}
}

table(rot.seq.30.operational.outcome)

table(rot.seq.10.outcome)+ table(rot.seq.30.outcome)

#Table 2
(table(rot.seq.10.outcome)+ table(rot.seq.30.outcome))/220000*100


table(rot.seq.10.operational.outcome)+ table(rot.seq.30.operational.outcome)

(table(rot.seq.10.operational.outcome)+ table(rot.seq.30.operational.outcome))/220000*100

##Compare Sequences and Rotations
proportion.difference = c(seq.rot.10.difference.proportion, seq.rot.30.difference.proportion)
cross.selection = c(sequence.df.10$cross.selection, sequence.df.30$cross.selection)
start.resistance = c(sequence.df.10$start.resistance, sequence.df.30$start.resistance)
deployment.frequency = c(sequence.df.10$dep.freq, sequence.df.30$dep.freq)

rot.seq.df.all = data.frame(proportion.difference, deployment.frequency, cross.selection, start.resistance)



rot.seq.df.all.no.draws = rot.seq.df.all%>%
  dplyr::filter(proportion.difference != 0)

seq.rot.count.labels = data.frame(table(rot.seq.df.all.no.draws$cross.selection,
      rot.seq.df.all.no.draws$deployment.frequency,
      rot.seq.df.all.no.draws$start.resistance))

seq.rot.count.labels = seq.rot.count.labels%>%
  dplyr::rename("cross.selection" = Var1)%>%
  dplyr::rename("deployment.frequency" = Var2)%>%
  dplyr::rename("start.resistance" = Var3)

seq.rot.count.labels$cross.selection.category = ifelse(as.numeric(as.character(seq.rot.count.labels$cross.selection)) > 0,
                                                       yes = "Positive",
                                                       no = ifelse(as.numeric(as.character(seq.rot.count.labels$cross.selection)) < 0,
                                                                   yes = "Negative",
                                                                   no = "None"))



plot_seq_rot_primary_outcome = function(){
  
  rot.seq.df.all.no.draws$operational.outcome = ifelse(rot.seq.df.all.no.draws$proportion.difference > 0,
                                                       yes = "Favours Rotations",
                                                       no = ifelse(rot.seq.df.all.no.draws$proportion.difference < 0,
                                                                   yes = "Favours Sequences",
                                                                   no = "Favours Neither"))
  
  
  
  rot.seq.df.all.no.draws$cross.selection.category = ifelse(rot.seq.df.all.no.draws$cross.selection > 0,
                                                            yes = "Positive",
                                                            no = ifelse(rot.seq.df.all.no.draws$cross.selection < 0,
                                                                        yes = "Negative",
                                                                        no = "None"))
  
  
 
  seq.rot.plot.all = ggplot(rot.seq.df.all.no.draws, aes(x=proportion.difference*100))+
    geom_histogram(aes(fill = operational.outcome),
                   bins = 50,
                   colour = "black",
                   alpha = 0.4)+
    scale_fill_manual(values = c("red", "blue"))+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey",
               size = 2)+
    #xlim(-50, 50)+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+
    ggtitle("Overall - 61663 Simulations")+
    guides(fill=guide_legend(title="Outcome"))+
    theme_classic()+ #draws = 158337
    theme(legend.position = "bottom")
  
  
  seq.rot.no.draws.10.0 = rot.seq.df.all.no.draws%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.rot.count.labels.10.0 = seq.rot.count.labels%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.rot.no.draws.30.0 = rot.seq.df.all.no.draws%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.rot.count.labels.30.0 = seq.rot.count.labels%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.rot.no.draws.10.50 = rot.seq.df.all.no.draws%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.rot.count.labels.10.50 = seq.rot.count.labels%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.rot.no.draws.30.50 = rot.seq.df.all.no.draws%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.rot.count.labels.30.50 = seq.rot.count.labels%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.rot.plot.10.0  = ggplot(seq.rot.no.draws.10.0, aes(x=proportion.difference*100,
                                                         y = cross.selection.category))+
    geom_density_ridges(aes(fill = operational.outcome),alpha = 0.4, 
                        #stat="binline",
                        colour = "black"#,
                        #bins = 50
    )+
    scale_fill_manual(values = c("red", "blue"))+
    geom_label(data = seq.rot.count.labels.10.0,
               aes(x=50,
                   y=cross.selection.category,
                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    xlim(-100, 50)+
    ggtitle("Novel - Deployment Interval: 10 Generations")+
    theme_classic()+
    theme(legend.position = "none")
  
  seq.rot.plot.30.0  = ggplot(seq.rot.no.draws.30.0, aes(x=proportion.difference*100,
                                                         y = cross.selection.category))+
    geom_density_ridges(aes(fill = operational.outcome),alpha = 0.4, 
                        #stat="binline",
                        colour = "black"#,
                        #bins = 50
    )+
    scale_fill_manual(values = c("red", "blue"))+
    geom_label(data = seq.rot.count.labels.30.0,
               aes(x=50,
                   y=cross.selection.category,
                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    xlim(-100, 50)+
    ggtitle("Novel - Deployment Interval: 30 Generations")+
    theme_classic()+
    theme(legend.position = "none")
  
  seq.rot.plot.10.50  = ggplot(seq.rot.no.draws.10.50, aes(x=proportion.difference*100,
                                                           y = cross.selection.category))+
    geom_density_ridges(aes(fill = operational.outcome),alpha = 0.4, 
                        #stat="binline",
                        colour = "black"#,
                        #bins = 50
    )+
    scale_fill_manual(values = c("red", "blue"))+
    geom_label(data = seq.rot.count.labels.10.50,
               aes(x=50,
                   y=cross.selection.category,
                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    xlim(-100, 50)+
    ggtitle("Pre-Used - Deployment Interval: 10 Generations")+
    theme_classic()+
    theme(legend.position = "none")
  
  seq.rot.plot.30.50  = ggplot(seq.rot.no.draws.30.50, aes(x=proportion.difference*100,
                                                           y = cross.selection.category))+
    geom_density_ridges(aes(fill = operational.outcome),alpha = 0.4, 
                        #stat="binline",
                        colour = "black"#,
                        #bins = 50
    )+
    scale_fill_manual(values = c("red", "blue"))+
    geom_label(data = seq.rot.count.labels.30.50,
               aes(x=50,
                   y=cross.selection.category,
                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+    
    xlim(-100, 50)+
    ggtitle("Pre-Used - Deployment Interval: 30 Generations")+
    theme_classic()+
    theme(legend.position = "none")
  
  #Combine the plots into a single plot
  top.rot.seq = seq.rot.plot.10.0 + seq.rot.plot.10.50
  bottom.rot.set = seq.rot.plot.30.0 + seq.rot.plot.30.50
  side.panel.rot.seq = top.rot.seq / bottom.rot.set
  final.figure.rot.seq = seq.rot.plot.all + side.panel.rot.seq + plot_layout(widths = c(1, 2)) +
    plot_annotation(title = "Sequences vs Rotations")
  return(final.figure.rot.seq)
  
}
#Figure 4
plot_seq_rot_primary_outcome()
#Sequences and Rotations: The Draws

rot.seq.df.all$difference.peak.resistance = c(sequence.df.10$peak.resistance - rotation.df.10$peak.resistance, sequence.df.30$peak.resistance - rotation.df.30$peak.resistance)
rot.seq.df.all$difference.mean.resistance = c(sequence.df.10$mean.resistance.intensity. - rotation.df.10$mean.resistance.intensity., sequence.df.30$mean.resistance.intensity. - rotation.df.30$mean.resistance.intensity.)
rot.seq.df.all$difference.control.failure.gens = c(sequence.df.10$exceedance.generations.deployed - rotation.df.10$exceedance.generations.deployed, sequence.df.30$exceedance.generations.deployed - rotation.df.30$exceedance.generations.deployed)

rot.seq.df.draws = rot.seq.df.all%>%
  dplyr::filter(proportion.difference == 0)


difference.peak.resistance.bioassay = c()
difference.mean.resistance.bioassay = c()


for(i in 1:nrow(rot.seq.df.draws)){
  difference.peak.resistance.bioassay[i] = resistance_to_bioassay_survival(mean.population.resistance = rot.seq.df.draws$difference.peak.resistance[i],
                                                                           sd.population.resistance = 0, #measured without error
                                                                           michaelis.menten.slope = ,
                                                                           nsim = 1, #is measured without error
                                                                           maximum.bioassay.survival.proportion = 1,
                                                                           half.population.bioassay.survival.resistance = 900
  )
  difference.mean.resistance.bioassay[i] = resistance_to_bioassay_survival(mean.population.resistance = rot.seq.df.draws$difference.mean.resistance[i],
                                                                           sd.population.resistance = 0, #measured without error
                                                                           michaelis.menten.slope = ,
                                                                           nsim = 1, #is measured without error
                                                                           maximum.bioassay.survival.proportion = 1,
                                                                           half.population.bioassay.survival.resistance = 900
  )
}

rot.seq.df.draws$difference.peak.resistance.bioassay = difference.peak.resistance.bioassay
rot.seq.df.draws$difference.mean.resistance.bioassay = difference.mean.resistance.bioassay
rot.seq.df.draws$start.resistance.status = ifelse(rot.seq.df.draws$start.resistance == 0,
                                                  yes = "novel",
                                                  no = "pre-used")


rot.seq.draws.label = data.frame(table(rot.seq.df.draws$cross.selection,
                                       rot.seq.df.draws$deployment.frequency,
                                       rot.seq.df.draws$start.resistance))

rot.seq.draws.label = rot.seq.draws.label%>%
  dplyr::rename("cross.selection" = Var1)%>%
  dplyr::rename("deployment.frequency" = Var2)%>%
  dplyr::rename("start.resistance" = Var3)


plot_seq_rot_secondary_outcome = function(){
  
  secondary.peak.plot = ggplot(rot.seq.df.draws, aes(x=difference.peak.resistance.bioassay*100))+
    geom_histogram(aes(fill = as.character(cross.selection)),
                   colour = "black",
                   binwidth = 0.25)+
    scale_fill_manual(values =rev(c("#67001f",
                                    "#b2182b",
                                    "#d6604d",
                                    "#f4a582",
                                    "#fddbc7",
                                    "#4d9221",
                                    "#d1e5f0",
                                    "#92c5de",
                                    "#4393c3",
                                    "#2166ac",
                                    "#053061")))+
    theme_bw()+
    xlim(-2, 10)+
    xlab("Difference in Peak Bioassay Survival")+
    guides(fill=guide_legend(title="Cross Selection"))+
    theme(legend.position = "none")
  
  secondary.mean.plot = ggplot(rot.seq.df.draws, aes(x=difference.mean.resistance.bioassay*100))+
    geom_histogram(aes(fill = as.character(cross.selection)),
                   colour = "black",
                   binwidth = 0.25)+
    scale_fill_manual(values =rev(c("#67001f",
                                    "#b2182b",
                                    "#d6604d",
                                    "#f4a582",
                                    "#fddbc7",
                                    "#4d9221",
                                    "#d1e5f0",
                                    "#92c5de",
                                    "#4393c3",
                                    "#2166ac",
                                    "#053061")))+
    xlim(-2, 10)+
    theme_bw()+
    xlab("Difference in Mean Bioassay Survival")+
    guides(fill=guide_legend(title="Cross Selection"))+
    theme(legend.position = "none")
  
  get.legend =ggplot(rot.seq.df.draws, aes(x=difference.mean.resistance.bioassay*100))+
    geom_histogram(aes(fill = as.character(cross.selection)),
                   colour = "black",
                   binwidth = 0.25)+
    scale_fill_manual(values =rev(c("#67001f",
                                    "#b2182b",
                                    "#d6604d",
                                    "#f4a582",
                                    "#fddbc7",
                                    "#4d9221",
                                    "#d1e5f0",
                                    "#92c5de",
                                    "#4393c3",
                                    "#2166ac",
                                    "#053061")))+
    theme_bw()+
    xlab("Difference in Mean Bioassay Survival")+
    guides(fill=guide_legend(title="Cross Selection"),
    )+
    theme(legend.direction="horizontal")
  
  
  B = cowplot::get_legend(get.legend)
  
  layout <- "
AAABBB
AAABBB
##CC##
"
  final.plot = secondary.peak.plot + secondary.mean.plot + B + 
    plot_layout(design = layout)
  
  return(final.plot)
}

plot_seq_rot_secondary_outcome()


##Sequences & Rotations vs Mixtures

rot.duration = c(rotation.df.10$simulation.duration, rotation.df.30$simulation.duration)
seq.duration = c(sequence.df.10$simulation.duration, sequence.df.30$simulation.duration)
mix.duration = c(mixture.df.10$simulation.duration, mixture.df.30$simulation.duration)

seq.rot.mix.outcome = c()
for(i in 1:length(rot.duration)){
  if(rot.duration[i] > seq.duration[i] &
     rot.duration[i] > mix.duration[i]){seq.rot.mix.outcome[i] = "rotation.win"}
  if(seq.duration[i] > rot.duration[i] & 
     seq.duration[i] > mix.duration[i]){seq.rot.mix.outcome[i] = "sequence.win"}
  if(mix.duration[i] > seq.duration[i] &
     mix.duration[i] > rot.duration[i]){seq.rot.mix.outcome[i] = "mixture.win"}
  if(rot.duration[i] == seq.duration[i] & 
     rot.duration[i] == mix.duration[i]){seq.rot.mix.outcome[i] = "draw"}
  if(rot.duration[i] > seq.duration[i] & 
     rot.duration[i] == mix.duration[i]){seq.rot.mix.outcome[i] = "sequence.loss"}
  if(seq.duration[i] > rot.duration[i] & 
     seq.duration[i] == mix.duration[i]){seq.rot.mix.outcome[i] = "rotation.loss"}
  
}

sum(is.na(seq.rot.mix.outcome))#all accounted for

table(seq.rot.mix.outcome)
round(table(seq.rot.mix.outcome)/220000*100)

prop.diff.seq.mix = 1 - (seq.duration/mix.duration)
prop.diff.seq.rot = 1 - (seq.duration/rot.duration)
prop.diff.rot.mix = 1 - (rot.duration/mix.duration)

rot.seq.mix.operational.outcome = c()
for(i in 1:length(seq.duration)){
  
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){rot.seq.mix.operational.outcome[i] = "sequence loss"}
  if(prop.diff.rot.mix[i] >= 0.1 &
     prop.diff.seq.mix[i] < 0.1 ){rot.seq.mix.operational.outcome[i] = "rotation loss"}
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] >= 0.1 ){rot.seq.mix.operational.outcome[i] = "mixture win"}
  if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] > 0.1 ){rot.seq.mix.operational.outcome[i] = "rotation win"}
  if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] < -0.1 ){rot.seq.mix.operational.outcome[i] = "sequence win"}
  if(prop.diff.seq.mix[i] < 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){rot.seq.mix.operational.outcome[i] = "no operational win"}
}

sum(is.na(rot.seq.mix.operational.outcome))#all accounted for

table(rot.seq.mix.operational.outcome)
round(table(rot.seq.mix.operational.outcome)/220000*100)

sum(table(rot.seq.mix.operational.outcome)/220000*100)

##Sequences vs Mixtures:::
rot.seq.df.all$prop.diff.seq.mix = prop.diff.seq.mix

seq.mix.df.no.draws = rot.seq.df.all%>%
  dplyr::filter(prop.diff.seq.mix != 0)

seq.mix.df.labels = data.frame(table(seq.mix.df.no.draws$deployment.frequency,
                          seq.mix.df.no.draws$cross.selection,
                          seq.mix.df.no.draws$start.resistance))

seq.mix.df.labels = seq.mix.df.labels%>%
  dplyr::rename("deployment.frequency" = Var1)%>%
  dplyr::rename("cross.selection" = Var2)%>%
  dplyr::rename("start.resistance" = Var3)


plot_seq_mix_primary_outcome = function(){
  
  seq.mix.df.no.draws$operational.outcome = ifelse(seq.mix.df.no.draws$prop.diff.seq.mix > 0,
                                                   yes = "Favours Mixtures",
                                                   no = ifelse(seq.mix.df.no.draws$prop.diff.seq.mix < 0,
                                                               yes = "Favours Sequences",
                                                               no = "Favours Neither"))
  seq.mix.df.no.draws$cross.selection.category = ifelse(seq.mix.df.no.draws$cross.selection > 0,
                                                        yes = "Positive",
                                                        no = ifelse(seq.mix.df.no.draws$cross.selection < 0,
                                                                    yes = "Negative",
                                                                    no = "None"))
  
  seq.mix.df.labels = data.frame(table(seq.mix.df.no.draws$deployment.frequency,
                                       seq.mix.df.no.draws$cross.selection.category,
                                       seq.mix.df.no.draws$start.resistance))
  
  seq.mix.df.labels = seq.mix.df.labels%>%
    dplyr::rename("deployment.frequency" = Var1)%>%
    dplyr::rename("cross.selection.category" = Var2)%>%
    dplyr::rename("start.resistance" = Var3)
  
  label.seq.mix= data.frame(x.coord = c(-10, 25),
                            y.coord = c(6000, 6000),
                            label.text = c(paste("Favours \nSequences"), paste("Favours \nMixtures")))
  
  seq.mix.plot.all = ggplot(seq.mix.df.no.draws, aes(x=prop.diff.seq.mix*100))+
    geom_histogram(fill = "#005a32",
                   colour = "#a1d99b")+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey",
               size = 2)+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+
    ggtitle("Overall - 79845 Simulations")+
    xlim(-15, 80)+
    theme_classic() 
  
  seq.mix.no.draws.10.0 = seq.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.mix.df.labels.10.0 = seq.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.mix.no.draws.30.0 = seq.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.mix.df.labels.30.0 = seq.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 0)
  
  seq.mix.no.draws.10.50 = seq.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.mix.df.labels.10.50 = seq.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.mix.no.draws.30.50 = seq.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.mix.df.labels.30.50 = seq.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 50)
  
  seq.mix.plot.10.0  = ggplot(seq.mix.no.draws.10.0, aes(x=prop.diff.seq.mix*100,
                                                         y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = seq.mix.df.labels.10.0, aes(x=80,
                                                  y=cross.selection.category,
                                                  label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    ggtitle("Novel - Deployment Interval: 10 Generations")+
    xlim(0, 80)+
    theme_classic()
  
  seq.mix.plot.30.0  = ggplot(seq.mix.no.draws.30.0, aes(x=prop.diff.seq.mix*100,
                                                         y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = seq.mix.df.labels.30.0, aes(x=80,
                                                  y=cross.selection.category,
                                                  label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    ggtitle("Novel - Deployment Interval: 30 Generations")+
    xlim(0, 80)+
    theme_classic()
  
  seq.mix.plot.10.50  = ggplot(seq.mix.no.draws.10.50, aes(x=prop.diff.seq.mix*100,
                                                           y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = seq.mix.df.labels.10.50, aes(x=80,
                                                   y=cross.selection.category,
                                                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    ggtitle("Pre-Used - Deployment Interval: 10 Generations")+
    xlim(0, 80)+
    theme_classic()
  
  
  seq.mix.plot.30.50  = ggplot(seq.mix.no.draws.30.50, aes(x=prop.diff.seq.mix*100,
                                                           y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = seq.mix.df.labels.30.50, aes(x=80,
                                                   y=cross.selection.category,
                                                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    xlim(0, 80)+
    ggtitle("Pre-Used - Deployment Interval: 30 Generations")+
    theme_classic()
  
  
  #Combine the plots into a single plot
  top.seq.mix = seq.mix.plot.10.0 + seq.mix.plot.10.50
  bottom.seq.mix = seq.mix.plot.30.0 + seq.mix.plot.30.50
  side.panel.seq.mix = top.seq.mix / bottom.seq.mix
  final.figure.seq.mix = seq.mix.plot.all + side.panel.seq.mix + plot_layout(widths = c(1, 2)) +
    plot_annotation(title = "Sequences vs Mixtures")
  final.figure.seq.mix
  
  return(final.figure.seq.mix)
}
#Figure 6
plot_seq_mix_primary_outcome()


#Rotations vs Mixtures::::
rot.seq.df.all$prop.diff.rot.mix = prop.diff.rot.mix

rot.mix.df.no.draws = rot.seq.df.all%>%
  dplyr::filter(prop.diff.rot.mix != 0)

rot.mix.df.labels = data.frame(table(rot.mix.df.no.draws$cross.selection,
                                     rot.mix.df.no.draws$deployment.frequency,
                                     rot.mix.df.no.draws$start.resistance))

rot.mix.df.labels = rot.mix.df.labels%>%
  dplyr::rename("cross.selection" = Var1)%>%
  dplyr::rename("deployment.frequency" = Var2)%>%
  dplyr::rename("start.resistance" = Var3)

plot_rot_mix_primary_outcome = function(){
  
  rot.mix.df.no.draws$operational.outcome = ifelse(rot.mix.df.no.draws$prop.diff.rot.mix > 0,
                                                   yes = "Favours Mixtures",
                                                   no = ifelse(rot.mix.df.no.draws$prop.diff.rot.mix < 0,
                                                               yes = "Favours Sequences",
                                                               no = "Favours Neither"))
  rot.mix.df.no.draws$cross.selection.category = ifelse(rot.mix.df.no.draws$cross.selection > 0,
                                                        yes = "Positive",
                                                        no = ifelse(rot.mix.df.no.draws$cross.selection < 0,
                                                                    yes = "Negative",
                                                                    no = "None"))
  
  rot.mix.df.labels = data.frame(table(rot.mix.df.no.draws$deployment.frequency,
                                       rot.mix.df.no.draws$cross.selection.category,
                                       rot.mix.df.no.draws$start.resistance))
  
  rot.mix.df.labels = rot.mix.df.labels%>%
    dplyr::rename("deployment.frequency" = Var1)%>%
    dplyr::rename("cross.selection.category" = Var2)%>%
    dplyr::rename("start.resistance" = Var3)
  
  rot.mix.plot.all = ggplot(rot.mix.df.no.draws, aes(x=prop.diff.rot.mix*100))+
    geom_histogram(fill = "#005a32",
                   colour = "#a1d99b")+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey",
               size = 2)+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+
    ggtitle("Overall - 75885 Simulations")+
    xlim(-15, 80)+
    theme_classic() 
  
  rot.mix.no.draws.10.0 = rot.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 0)
  
  rot.mix.df.labels.10.0 = rot.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 0)
  
  rot.mix.no.draws.30.0 = rot.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 0)
  
  rot.mix.df.labels.30.0 = rot.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 0)
  
  rot.mix.no.draws.10.50 = rot.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 50)
  
  rot.mix.df.labels.10.50 = rot.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 10)%>%
    dplyr::filter(start.resistance == 50)
  
  rot.mix.no.draws.30.50 = rot.mix.df.no.draws%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 50)
  
  rot.mix.df.labels.30.50 = rot.mix.df.labels%>%
    dplyr::filter(deployment.frequency == 30)%>%
    dplyr::filter(start.resistance == 50)
  
  rot.mix.plot.10.0  = ggplot(rot.mix.no.draws.10.0, aes(x=prop.diff.rot.mix*100,
                                                         y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = rot.mix.df.labels.10.0, aes(x=80,
                                                  y=cross.selection.category,
                                                  label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    ggtitle("Novel - Deployment Interval: 10 Generations")+
    xlim(0, 80)+
    theme_classic()
  
  rot.mix.plot.30.0  = ggplot(rot.mix.no.draws.30.0, aes(x=prop.diff.rot.mix*100,
                                                         y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = rot.mix.df.labels.30.0, aes(x=80,
                                                  y=cross.selection.category,
                                                  label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    ggtitle("Novel - Deployment Interval: 30 Generations")+
    xlim(0, 80)+
    theme_classic()
  
  rot.mix.plot.10.50  = ggplot(rot.mix.no.draws.10.50, aes(x=prop.diff.rot.mix*100,
                                                           y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = rot.mix.df.labels.10.50, aes(x=80,
                                                   y=cross.selection.category,
                                                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    ggtitle("Pre-Used - Deployment Interval: 10 Generations")+
    xlim(0, 80)+
    theme_classic()
  
  
  rot.mix.plot.30.50  = ggplot(rot.mix.no.draws.30.50, aes(x=prop.diff.rot.mix*100,
                                                           y = cross.selection.category))+
    geom_density_ridges(alpha = 0.7, 
                        fill = "#005a32",
                        colour = "#a1d99b"
    )+
    geom_label(data = rot.mix.df.labels.30.50, aes(x=80,
                                                   y=cross.selection.category,
                                                   label = Freq),
               size = 3)+
    geom_vline(xintercept = 0, linetype = "dashed", colour ="grey")+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Cross Selection Between Insecticides")+
    xlim(0, 80)+
    ggtitle("Pre-Used - Deployment Interval: 30 Generations")+
    theme_classic()
  
  
  #Combine the plots into a single plot
  top.rot.mix = rot.mix.plot.10.0 + rot.mix.plot.10.50
  bottom.rot.mix = rot.mix.plot.30.0 + rot.mix.plot.30.50
  side.panel.rot.mix = top.rot.mix / bottom.rot.mix
  final.figure.rot.mix = rot.mix.plot.all + side.panel.rot.mix + plot_layout(widths = c(1, 2)) +
    plot_annotation(title = "Rotations vs Mixtures")
  
  return(final.figure.rot.mix)
}
plot_rot_mix_primary_outcome()



##Unique Insecticides:
sequence.unique.duration = sequence.df.unique$simulation.duration
rotation.unique.duration = rotation.df.unique$simulation.duration
adaptive.rotation.unique.duration = adaptive.rotation.df.unique$simulation.duration
mixture.unique.duration = mixture.df.unique$simulation.duration

difference.sr = 1 - (sequence.unique.duration/rotation.unique.duration)
difference.sa = 1 - (sequence.unique.duration/adaptive.rotation.unique.duration)
difference.sm = 1 - (sequence.unique.duration/mixture.unique.duration)
difference.ra = 1 - (rotation.unique.duration/adaptive.rotation.unique.duration)
difference.rm = 1 - (rotation.unique.duration/mixture.unique.duration)
difference.am = 1 - (adaptive.rotation.unique.duration/mixture.unique.duration)

unique.df = cbind(data.frame(sequence.unique.duration,
                             rotation.unique.duration,
                             adaptive.rotation.unique.duration,
                             mixture.unique.duration,
                             difference.sr,
                             difference.sa,
                             difference.sm,
                             difference.ra,
                             difference.rm, 
                             difference.am), parameter.space.unique)

outcome.sr = c()
outcome.sa = c()
outcome.sm = c()
outcome.ra = c()
outcome.rm = c()
outcome.am = c()

operational.outcome.sr = c()
operational.outcome.sa = c()
operational.outcome.sm = c()
operational.outcome.ra = c()
operational.outcome.rm = c()
operational.outcome.am = c()
operational.outcome.sam = c()

for(i in 1:50000){
  if(difference.sr[i] == 0){outcome.sr[i] = "draw"}
  if(difference.sr[i] > 0){outcome.sr[i] = "rotation win"}
  if(difference.sr[i] < 0){outcome.sr[i] = "sequence win"}
  
  if(difference.sr[i] <= 0.1 &
     difference.sr[i] >= -0.1){operational.outcome.sr[i] = "no operational win"}
  if(difference.sr[i] > 0.1){operational.outcome.sr[i] = "rotation operational win"}
  if(difference.sr[i] < -0.1){operational.outcome.sr[i] = "sequence operational win"} 
  
  
  
  
  if(difference.sa[i] == 0){outcome.sa[i] = "draw"}
  if(difference.sa[i] > 0){outcome.sa[i] = "adaptive rotation win"}
  if(difference.sa[i] < 0){outcome.sa[i] = "sequence win"}
  
  if(difference.sa[i] <= 0.1 &
     difference.sa[i] >= -0.1){operational.outcome.sa[i] = "no operational win"}
  if(difference.sa[i] > 0.1){operational.outcome.sa[i] = "adaptive rotation operational win"}
  if(difference.sa[i] < -0.1){operational.outcome.sa[i] = "sequence operational win"} 
  
  if(difference.sm[i] == 0){outcome.sm[i] = "draw"}
  if(difference.sm[i] > 0){outcome.sm[i] = "mixture win"}
  if(difference.sm[i] < 0){outcome.sm[i] = "sequence win"}
  
  if(difference.sm[i] <= 0.1 &
     difference.sm[i] >= -0.1){operational.outcome.sm[i] = "no operational win"}
  if(difference.sm[i] > 0.1){operational.outcome.sm[i] = "mixture operational win"}
  if(difference.sm[i] < -0.1){operational.outcome.sm[i] = "sequence operational win"} 
  
  if(difference.ra[i] == 0){outcome.ra[i] = "draw"}
  if(difference.ra[i] > 0){outcome.ra[i] = "adaptive rotation win"}
  if(difference.ra[i] < 0){outcome.ra[i] = "rotation win"}
  
  if(difference.ra[i] <= 0.1 &
     difference.ra[i] >= -0.1){operational.outcome.ra[i] = "no operational win"}
  if(difference.ra[i] > 0.1){operational.outcome.ra[i] = "adaptive rotation operational win"}
  if(difference.ra[i] < -0.1){operational.outcome.ra[i] = "rotation operational win"} 
  
  if(difference.rm[i] == 0){outcome.rm[i] = "draw"}
  if(difference.rm[i] > 0){outcome.rm[i] = "mixture win"}
  if(difference.rm[i] < 0){outcome.rm[i] = "rotation win"}
  
  if(difference.rm[i] <= 0.1 &
     difference.rm[i] >= -0.1){operational.outcome.rm[i] = "no operational win"}
  if(difference.rm[i] > 0.1){operational.outcome.rm[i] = "mixture operational win"}
  if(difference.rm[i] < -0.1){operational.outcome.rm[i] = "rotation operational win"} 
  
  if(difference.am[i] == 0){outcome.am[i] = "draw"}
  if(difference.am[i] > 0){outcome.am[i] = "mixture win"}
  if(difference.am[i] < 0){outcome.am[i] = "adaptive rotation win"}
  
  if(difference.am[i] <= 0.1 &
     difference.am[i] >= -0.1){operational.outcome.am[i] = "no operational win"}
  if(difference.am[i] > 0.1){operational.outcome.am[i] = "mixture operational win"}
  if(difference.am[i] < -0.1){operational.outcome.am[i] = "adaptive rotation operational win"} 
  
  
  if(difference.sm[i] >= 0.1 &
     difference.am[i] < 0.1 ){operational.outcome.sam[i] = "sequence loss"}
  if(difference.am[i] >= 0.1 &
     difference.sm[i] < 0.1 ){operational.outcome.sam[i] = "rotation loss"}
  if(difference.sm[i] >= 0.1 &
     difference.am[i] >= 0.1 ){operational.outcome.sam[i] = "mixture win"}
  if(difference.am[i] < -0.1 &
     difference.sa[i] > 0.1 ){operational.outcome.sam[i] = "adaptive rotation win"}
  if(difference.am[i] < -0.1 &
     difference.sa[i] < -0.1 ){operational.outcome.sam[i] = "sequence win"}
  if(difference.sm[i] < 0.1 &
     difference.am[i] < 0.1 ){operational.outcome.sam[i] = "no operational win"}
}


table(outcome.sr)
table(outcome.sr)/50000*100

table(operational.outcome.sr)
table(operational.outcome.sr)/50000*100

table(outcome.sa)
table(outcome.sa)/50000*100
table(operational.outcome.sa)
table(operational.outcome.sa)/50000*100

table(outcome.ra)
table(outcome.ra)/50000*100

table(operational.outcome.ra)
table(operational.outcome.ra)/50000*100

table(outcome.sm)
table(operational.outcome.sm)


table(outcome.rm)
table(outcome.am)

unique.df = cbind(data.frame(sequence.unique.duration,
                             rotation.unique.duration,
                             adaptive.rotation.unique.duration,
                             mixture.unique.duration,
                             difference.sr,
                             difference.sa,
                             difference.sm,
                             difference.ra,
                             difference.rm, 
                             difference.am,
                             outcome.sr,
                             outcome.sa,
                             outcome.sm,
                             outcome.ra,
                             outcome.rm,
                             outcome.am,
                             operational.outcome.sr,
                             operational.outcome.sa,
                             operational.outcome.sm,
                             operational.outcome.ra,
                             operational.outcome.rm,
                             operational.outcome.am), parameter.space.unique)



unique.df.1 = unique.df%>%
  dplyr::filter(difference.sr >= 0.1 |
                  difference.sr <= -0.1)

n.sims.1 = nrow(unique.df.1)
  
ggplot(unique.df.1, aes(x=difference.sr*100))+
  geom_histogram(bins = 50,
                 colour = "skyblue",
                 fill = "operational.outcome.sr")+
  geom_text(aes(x=-500, y=4000, label = paste0("Number of Simulations = ", n.sims.1
                ),
                size = 10))+
  xlab("Percentage Difference in Operational Lifespan")+
  ylab("Count")+
  geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
  ggtitle("Favours Sequences vs Favours Rotations")+
  theme_classic()+
  theme(legend.position = "none",
      )

plot_unique_insecticide_difference = function(){
  
  unique.df.1 = unique.df%>%
    dplyr::filter(difference.sr >= 0.1 |
                    difference.sr <= -0.1)
  
  n.sims.1 = nrow(unique.df.1)
  
  plot.difference.sr = ggplot(unique.df.1, aes(x=difference.sr*100))+
    geom_histogram(bins = 50,
                   colour = "black",
                   alpha = 0.8,
                   aes(fill = operational.outcome.sr))+
    scale_fill_manual(values = c("red", "blue"))+
    geom_text(aes(x=0, y=2000, label = paste0("N = ", n.sims.1
    ),
    size = 10))+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+
    xlim(-100, 100)+
    ylim(0, 3000)+
    geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
    ggtitle("Sequences vs Rotations")+
    theme_classic()+
    theme(legend.position = "none")
  
  
  unique.df.2 = unique.df%>%
     dplyr::filter(difference.sa >= 0.1 |
                     difference.sa <= -0.1)
  n.sims.2 = nrow(unique.df.2)
  
  plot.difference.sa = ggplot(unique.df.2, aes(x=difference.sa*100))+
    geom_histogram(bins = 50,
                   alpha = 0.8,
                   colour = "black",
                   aes(fill = operational.outcome.sa))+
    scale_fill_manual(values = c("purple", "blue"))+
    geom_text(aes(x=0, y=2200, label = paste0("N = ", n.sims.2
    ),
    size = 10))+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+
    xlim(-100, 100)+
    ylim(0, 3000)+
    geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
    ggtitle("Sequences vs Adaptive Rotations")+
    theme_classic()+
    theme(legend.position = "none")
  
  
  unique.df.3 = unique.df%>%
    dplyr::filter(difference.sm >= 0.1 |
                    difference.sm <= -0.1)
  n.sims.3 = nrow(unique.df.3)
  
  
  plot.difference.sm = ggplot(unique.df.3, aes(x=difference.sm*100))+
    geom_histogram(bins = 50,
                   colour = "black",
                   alpha = 0.8,
                   aes(fill = operational.outcome.sm))+
    scale_fill_manual(values = c("green", "blue"))+
    geom_text(aes(x=0, y=2000, label = paste0("N = ", n.sims.3
    ),
    size = 10))+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+    
    xlim(-100, 100)+
    ylim(0, 3000)+
    geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
    ggtitle("Sequences vs Mixtures")+
    theme_classic()+
    theme(legend.position = "none")
  
  unique.df.4 = unique.df%>%
    dplyr::filter(difference.ra >= 0.1 |
                    difference.ra <= -0.1)
  n.sims.4 = nrow(unique.df.4)
  
  
  plot.difference.ra = ggplot(unique.df.4, aes(x=difference.ra*100))+
    geom_histogram(bins = 50,
                   colour = "black",
                   alpha = 0.8,
                   aes(fill = operational.outcome.ra))+
    scale_fill_manual(values = c("purple", "red"))+
    geom_text(aes(x=-50, y=2500, label = paste0("N = ", n.sims.4
    ),
    size = 10))+
    xlab("Percentage Difference in Operational Lifespan")+    
    ylab("Count")+
    xlim(-100, 100)+
    ylim(0, 3000)+
    geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
    ggtitle("Rotations vs Adaptive Rotations")+
    theme_classic()+
    theme(legend.position = "none")
  
  
  unique.df.5 = unique.df%>%
    dplyr::filter(difference.rm >= 0.1 |
                    difference.rm <= -0.1)
  n.sims.5 = nrow(unique.df.5)
  
  
  plot.difference.rm = ggplot(unique.df.5, aes(x=difference.rm*100))+
    geom_histogram(bins = 50,
                   colour = "black",
                   alpha = 0.8,
                   aes(fill = operational.outcome.rm))+
    scale_fill_manual(values = c("green", "red"))+
    geom_text(aes(x=-20, y=2500, label = paste0("N = ", n.sims.5
    ),
    size = 10))+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+
    xlim(-100, 100)+
    ylim(0, 3000)+
    geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
    ggtitle("Rotations vs Mixtures")+
    theme_classic()+
    theme(legend.position = "none")
  
  
  
  unique.df.6 = unique.df%>%
    dplyr::filter(difference.am >= 0.1 |
                    difference.am <= -0.1)
  
  n.sims.6 = nrow(unique.df.6)
  
  plot.difference.am = ggplot(unique.df.6, aes(x=difference.am*100))+
    geom_histogram(bins = 50,
                   colour = "black",
                   alpha = 0.8,
                   aes(fill = operational.outcome.am))+
    scale_fill_manual(values = c("purple", "green"))+
    geom_text(aes(x=0, y=2000, label = paste0("N = ", n.sims.6
    ),
    size = 10))+
    xlab("Percentage Difference in Operational Lifespan")+
    ylab("Count")+
    xlim(-100, 100)+
    ylim(0, 3000)+
    geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
    ggtitle("Adaptive Rotations vs Mixtures")+
    theme_classic()+
    theme(legend.position = "none")
  
  
  
  the.plot = (plot.difference.sr + plot.difference.sa + plot.difference.sm) /(
    plot.difference.ra + plot.difference.rm + plot.difference.am) + plot_annotation(title = "Comparing Insecticide Resistance Management Strategies with Unique Insecticides")
  
  return(the.plot)
}
plot_unique_insecticide_difference()


unique.df.mix.lost = unique.df%>%
  dplyr::filter(operational.outcome.sm == "sequence operational win")


ggplot(unique.df.mix.lost, aes(x=Heritability.1 - Heritability.2,
                               y=Start.1 - Start.2))+
  geom_point()+
  xlab("Difference in Heritability of Traits")+
  ylab("Difference in Starting Polygenic Resistance Score")+
  theme_classic()





#######################################################################
## 3. Data Analysis: Sensitivity Analysis ; Partial Rank Correlation ##
#######################################################################

#Partial Rank Correlation:: (uses EpiR)
df_from_pcor_test = function(irm.strategy,
                             data,
                             start.resistance,
                             cross.selection){
  
  pcor.df = data%>%
    dplyr::filter(strategy == strategy)%>%
    dplyr::filter(start.resistance == start.resistance)%>%
    dplyr::filter(cross.selection == cross.selection)%>%
        dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                  "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration")
  
  
  #Perform two sided partial rank correlation. 95% CIs
  pcor.output = epiR::epi.prcc(dat = pcor.df,
                               sided.test = 2,
                               conf.level = 0.95)
  
  x = ifelse(start.resistance == 0, 
             yes = "novel insecticides",
             no = "pre-used insecticdes")
  
  Simulation.Conditions = stringr::str_c(irm.strategy, " with ", x)
  
  estimate = pcor.output$est
  pvalue = pcor.output$p.value
  teststat = pcor.output$test.statistic
  lower.95.ci = pcor.output$lower
  upper.95.ci = pcor.output$upper

  strategy = irm.strategy
  cross.selection = cross.selection
  
  #parameters in correct order::
  parameter = c("Insecticide Resistance Heritability", "Fitness Cost", "Male Insecticide Exposure",
                "Intervention Coverage", "Mosquito Dispersal", "Female Insecticide Exposure")
  
  output = data.frame(estimate, lower.95.ci, upper.95.ci,  pvalue, teststat, strategy, Simulation.Conditions, parameter, cross.selection)
  
  return(output)
}

pcor_df_function = function(X,
                            dataset){

cross.selection.vals = seq(-0.5, 0.5, by = 0.1)
start.resist.vals = c(0, 50)

pcor.list = list()
pcor.list2 = list()
for(j in 1:2){
  for(i in 1:11){
  pcor.list[[i]] = df_from_pcor_test(X, dataset, start.resist.vals[j], cross.selection.vals[i])
  }
  pcor.list2[[j]] = pcor.list
}

pcor.df = do.call(rbind, do.call(rbind, pcor.list2))

return(pcor.df)
}
sequence.set = rbind(sequence.df.10, sequence.df.30)
rotation.set = rbind(rotation.df.10, rotation.df.30)
mixture.set = rbind(mixture.df.10, mixture.df.30)

pcor.seq.df = pcor_df_function("sequence", sequence.set)
pcor.rot.df = pcor_df_function("rotation", rotation.set)
pcor.mix.df = pcor_df_function("mixture", mixture.set)


pcor.df.all = rbind(pcor.seq.df,
                    pcor.rot.df,
                    pcor.mix.df)

make_pcor_plot = function(){
  
  pals = c("#74c476", "#006d2c", "#fb6a4a", "#a50f15", "#4eb3d3", "#084081")

  
  label.df = data.frame(
    text.label = c("Increases Resistance", "Decreases Resistance"),
    label_x_coord = c(-0.5, 0.5), #have the label fairly central.
    label_y_coord = c("Mosquito Dispersal", "Mosquito Dispersal")) #Should be far enough away to not be overlapping any bars/points

  figure = ggplot(pcor.df.all, aes(x=estimate, y=parameter, xmin =lower.95.ci,
                       xmax = upper.95.ci, fill = Simulation.Conditions)) +
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbarh(position = position_dodge())+
    scale_fill_manual(values = pals)+
    geom_vline(xintercept = 0) +
    xlim(-0.75, 0.75)+
    ylab("Parameter") +
    xlab("Partial Rank Correlation") +
    guides(fill=guide_legend(ncol=3)) +
    theme_classic()+
    facet_wrap(~as.factor(cross.selection))+
    theme(legend.position = "bottom",
          legend.justification = "left")
  
    return(figure)
}

make_pcor_plot()


####################################################
## 4. Generalised Linear (and Additive) Modelling ##
####################################################

##Generalised Linear (and Addditive) Modelling::
  #join the 3 dataframes together::

sequence.set.glm = sequence.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration",
                "strategy", "start.resistance", "cross.selection", "dep.freq")

rotation.set.glm = rotation.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration",
                "strategy", "start.resistance", "cross.selection", "dep.freq")

mixture.set.glm = mixture.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration",
                "strategy", "start.resistance", "cross.selection", "dep.freq")


df.for.glm = rbind(sequence.set.glm,
                   rotation.set.glm,
                   mixture.set.glm)


##First do Generalised Additive Models to Check for non-linear Relationship

df.for.glm$maximum.generations = 500

df.for.glm$starting.status = ifelse(df.for.glm$start.resistance == 0,
                                    yes = "novel",
                                    no = "pre-used")

df.for.glm.2 = df.for.glm%>%
  dplyr::filter(simulation.duration != 500)

gam.heritability = mgcv::bam(simulation.duration ~ 
                               as.factor(strategy)+
                               s(Heritability) +
                               Fitness.Cost +
                               Male.Insecticide.Exposure+
                               Female.Insecticide.Exposure+
                               Dispersal+
                               cross.selection+
                               Intervention.Coverage +
                               as.factor(starting.status)+
                               as.factor(dep.freq),
                             data = df.for.glm.2,
                             family = "poisson")

plot(gam.heritability)#broadly linear, as expected

gam.fitness.cost = mgcv::bam(simulation.duration ~ 
                               as.factor(strategy)+
                               Heritability +
                               s(Fitness.Cost) +
                               Male.Insecticide.Exposure+
                               Female.Insecticide.Exposure+
                               Dispersal+
                               cross.selection+
                               Intervention.Coverage +
                               as.factor(starting.status)+
                               as.factor(dep.freq),
                             data = df.for.glm.2,
                             family = "poisson")

plot(gam.fitness.cost)#linear

gam.male.exposure = mgcv::bam(simulation.duration ~ 
                                as.factor(strategy)+
                                Heritability +
                                Fitness.Cost +
                                s(Male.Insecticide.Exposure)+
                                Female.Insecticide.Exposure+
                                Dispersal+
                                cross.selection+
                                Intervention.Coverage +
                                as.factor(starting.status)+
                                as.factor(dep.freq),
                              data = df.for.glm.2,
                              family = "poisson")

plot(gam.male.exposure)#linear relationship


gam.female.exposure = mgcv::bam(simulation.duration ~ 
                                  as.factor(strategy)+
                                  Heritability +
                                  Fitness.Cost +
                                  Male.Insecticide.Exposure+
                                  s(Female.Insecticide.Exposure)+
                                  Dispersal+
                                  cross.selection+
                                  Intervention.Coverage +
                                  as.factor(starting.status)+
                                  as.factor(dep.freq),
                               data = df.for.glm.2,
                               family = "poisson")


plot(gam.female.exposure)#linear


gam.dispersal= mgcv::bam(simulation.duration ~ 
                           as.factor(strategy)+
                           Heritability +
                           Fitness.Cost +
                           Male.Insecticide.Exposure+
                           Female.Insecticide.Exposure+
                           s(Dispersal)+
                           cross.selection+
                           Intervention.Coverage +
                           as.factor(starting.status)+
                           as.factor(dep.freq),
                         data = df.for.glm.2,
                         family = "poisson")

plot(gam.dispersal)#hugely wiggly
#Look to put splines in at: ~0.3, ~0.8 and maximise likelihood
summary(gam.dispersal)

gam.coverage= mgcv::bam(simulation.duration ~ 
                          as.factor(strategy)+
                          Heritability +
                          Fitness.Cost +
                          Male.Insecticide.Exposure+
                          Female.Insecticide.Exposure+
                          Dispersal+
                          cross.selection+
                          s(Intervention.Coverage) +
                          as.factor(starting.status)+
                          as.factor(dep.freq),
                        data = df.for.glm.2,
                        family = "poisson")


plot(gam.coverage) #broadly linear


#find the location of the splines which maximise the likelihood
find.max.logLik = function(dspline1, dspline2,  data){
  
  data$disperal.spline1 = ifelse(data$Dispersal > dspline1, data$Dispersal-dspline1, 0)
  data$dispersal.spline2  = ifelse(data$Dispersal > dspline2, data$Dispersal-dspline2, 0 )

  
  temp.glm = glm(simulation.duration ~ 
                   strategy +
                   Heritability +
                   Fitness.Cost +
                   Male.Insecticide.Exposure+
                   Female.Insecticide.Exposure+
                   Dispersal+
                   disperal.spline1+
                   dispersal.spline2+
                   Intervention.Coverage+
                   starting.status+
                   as.factor(dep.freq)+
                   cross.selection,
                 data = data,
                 family = "poisson")
  
  return(logLik(temp.glm))
}

find.max.logLik(dspline1 = 0.238, 
                dspline2 = 0.785, 
                data = df.for.glm.2)

find.max.logLik(dspline1 = 0.238, 
                dspline2 = 0.785, 
                data = df.for.glm.2)

df.for.glm.2$disperal.spline1 = ifelse(df.for.glm.2$Dispersal > 0.238, df.for.glm.2$Dispersal-0.238, 0)
df.for.glm.2$dispersal.spline2  = ifelse(df.for.glm.2$Dispersal > 0.785, df.for.glm.2$Dispersal-0.785, 0 )



poisson.glm =  glm(simulation.duration ~ 
                     strategy +
                     Heritability +
                     Fitness.Cost +
                     Male.Insecticide.Exposure+
                     Female.Insecticide.Exposure+
                     Dispersal+
                     disperal.spline1+
                     dispersal.spline2+
                     Intervention.Coverage+
                     starting.status +
                     as.factor(dep.freq)+
                     cross.selection,
                     data = df.for.glm.2,
                   family = "poisson")

AER::dispersiontest(poisson.glm) #overdispersed

neg.bin.glm =  MASS::glm.nb(simulation.duration ~ 
                              strategy +
                              Heritability +
                              Fitness.Cost +
                              Male.Insecticide.Exposure+
                              Female.Insecticide.Exposure+
                              Dispersal+
                              disperal.spline1+
                              dispersal.spline2+
                              Intervention.Coverage+
                              as.factor(dep.freq)+
                              starting.status +
                              cross.selection,
                            data = df.for.glm.2)
  
summary(neg.bin.glm)
confint(neg.bin.glm)


############################################################################
##5. Random Forest to Identify Variables with the Best Predictive Ability ##
############################################################################

library(randomForest)
#First for choosing between sequences and rotations:::
rf.dataset.rot.seq = sequence.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure",
                "start.resistance", "cross.selection")%>%
  dplyr::mutate(start.resistance = as.factor(start.resistance))

rf.dataset.rot.seq$operational.outcome = c(rot.seq.10.operational.outcome, rot.seq.30.operational.outcome)

rf.dataset.rot.seq = rf.dataset.rot.seq%>%
   dplyr::mutate(operational.outcome = as.factor(operational.outcome))


## 75% of the sample size
sample.size = floor(0.7 * nrow(rf.dataset.rot.seq))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(rf.dataset.rot.seq)), size = sample.size)

train.rf.dataset.rot.seq = rf.dataset.rot.seq[train.ind, ]
test.rf.dataset.rot.seq = rf.dataset.rot.seq[-train.ind, ]

rf.model = randomForest::randomForest(operational.outcome ~ .,
                                      data = train.rf.dataset.rot.seq,
                                      type = "classification",
                                      importance = TRUE,
                                      ntree = 300,
                                      mtry = 4,
                                      node.size = 1000)


randomForest::varImpPlot(rf.model)

rf.model.seq.rot.df = data.frame(rf.model$importance)
rf.model.seq.rot.df.sd = data.frame(rf.model$importanceSD)

rf.model.seq.rot.df$parameter = c("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
  "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure",
  "Start.Resistance", "Cross.Selection")


rf.predic.seq.rot = predict(rf.model, test.rf.dataset.rot.seq)

accuracy = ifelse(rf.predic.seq.rot == test.rf.dataset.rot.seq$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/66000*100
#Model accuracy is 87.65%

seq.rot.rf.plot.acc = ggplot(rf.model.seq.rot.df, aes(x=MeanDecreaseAccuracy,
                                y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("A")+
  theme_classic()

seq.rot.rf.plot.gini = ggplot(rf.model.seq.rot.df, aes(x=MeanDecreaseGini,
                                y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("B")+
  theme_classic()

gridExtra::grid.arrange(seq.rot.rf.plot.acc, seq.rot.rf.plot.gini,
                        nrow = 1)



##Unique Insecticides Seq vs Adaptive Rot:
parameter.space.unique.rf = parameter.space.unique%>%
  dplyr::select(-"X")

parameter.space.unique.rf$operational.outcome = as.factor(operational.outcome.sa)
## 75% of the sample size
sample.size = floor(0.7 * nrow(parameter.space.unique.rf))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(parameter.space.unique.rf)), size = sample.size)

train.rf.dataset.seq.adrot = parameter.space.unique.rf[train.ind, ]
test.rf.dataset.seq.adrot= parameter.space.unique.rf[-train.ind, ]

rf.model.seq.adrot = randomForest::randomForest(operational.outcome ~ .,
                                      data = train.rf.dataset.seq.adrot,
                                      type = "classification",
                                      importance = TRUE,
                                      ntree = 300,
                                      mtry = 4,
                                      node.size = 1000)






randomForest::varImpPlot(rf.model.seq.adrot)

rf.model.seq.adrot.df = data.frame(rf.model.seq.adrot$importance)

rf.model.seq.adrot.df$parameter = c("Heritability.1", "Heritability.2", "Fitness.Cost.1",
                                 "Fitness.Cost.2", "Start.Resistace.1", "Start.Resistance.2",
                                 "Cross.Selection", "Dispersal", "Intervention.Coverage", 
                                 "Female.Insecticide.Exposure", "Male.Insecticide.Exposure")


rf.predic.seq.adrot= predict(rf.model.seq.adrot, test.rf.dataset.seq.adrot)

accuracy = ifelse(rf.predic.seq.adrot == test.rf.dataset.seq.adrot$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/15000*100
#Model accuracy is 90.98

seq.adrot.rf.plot.acc = ggplot(rf.model.seq.adrot.df, aes(x=MeanDecreaseAccuracy,
                                                      y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("C")+
  theme_classic()

seq.adrot.rf.plot.gini = ggplot(rf.model.seq.adrot.df, aes(x=MeanDecreaseGini,
                                                       y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("D")+
  theme_classic()

gridExtra::grid.arrange(seq.adrot.rf.plot.acc, seq.adrot.rf.plot.gini,
                        nrow = 1)


((seq.rot.rf.plot.acc + seq.rot.rf.plot.gini)/
(seq.adrot.rf.plot.acc + seq.adrot.rf.plot.gini)) + plot_annotation(title = "Random Forest Models: Sequences vs Rotations")
                        



#Random Forest: Sequence, Rotation and Mixtures

rf.dataset.rot.seq.mix = sequence.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure",
                "start.resistance", "cross.selection")%>%
  dplyr::mutate(start.resistance = as.factor(start.resistance))

rf.dataset.rot.seq.mix$operational.outcome = as.factor(rot.seq.mix.operational.outcome)

# 70% of the sample size
sample.size = floor(0.7 * nrow(rf.dataset.rot.seq.mix))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(rf.dataset.rot.seq.mix)), size = sample.size)

train.rf.dataset.rot.seq.mix = rf.dataset.rot.seq.mix[train.ind, ]
test.rf.dataset.rot.seq.mix = rf.dataset.rot.seq.mix[-train.ind, ]

rf.model.seq.rot.mix = randomForest::randomForest(operational.outcome ~ .,
                                      data = train.rf.dataset.rot.seq.mix,
                                      type = "classification",
                                      importance = TRUE,
                                      ntree = 300,
                                      mtry = 4,
                                      node.size = 1000)

rf.model.seq.rot.mix.df = data.frame(rf.model.seq.rot.mix$importance)
rf.model.seq.rot.mix.df$parameter = c("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                                     "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure",
                                     "Start.Resistance", "Cross.Selection")

rf.predic.seq.rot.mix= predict(rf.model.seq.rot.mix, test.rf.dataset.rot.seq.mix)


accuracy = ifelse(rf.predic.seq.rot.mix == test.rf.dataset.rot.seq.mix$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/66000*100
#Model accuracy is 96.14%

seq.rot.mix.rf.plot.acc = ggplot(rf.model.seq.rot.mix.df, aes(x=MeanDecreaseAccuracy,
                                                      y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("A")+
  theme_classic()

seq.rot.mix.rf.plot.gini = ggplot(rf.model.seq.rot.mix.df, aes(x=MeanDecreaseGini,
                                                       y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("B")+
  theme_classic()


seq.rot.mix.rf.plot.acc
seq.rot.mix.rf.plot.gini


##Unique Insecticides Seq vs Adaptive Rot vs Mixtures
rf.parameter.space.unique.sam = parameter.space.unique%>%
  dplyr::select(-"X")

rf.parameter.space.unique.sam$operational.outcome = as.factor(operational.outcome.sam)
## 75% of the sample size
sample.size = floor(0.7 * nrow(rf.parameter.space.unique.sam))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(rf.parameter.space.unique.sam)), size = sample.size)

train.rf.dataset.sam = rf.parameter.space.unique.sam[train.ind, ]
test.rf.dataset.sam= rf.parameter.space.unique.sam[-train.ind, ]

rf.model.sam = randomForest::randomForest(operational.outcome ~ .,
                                                data = train.rf.dataset.sam,
                                                type = "classification",
                                                importance = TRUE,
                                                ntree = 300,
                                                mtry = 4,
                                                node.size = 1000)

rf.model.sam.df = data.frame(rf.model.sam$importance)

rf.model.sam.df$parameter = c("Heritability.1", "Heritability.2", "Fitness.Cost.1",
                                    "Fitness.Cost.2", "Start.Resistace.1", "Start.Resistance.2",
                                    "Cross.Selection", "Dispersal", "Intervention.Coverage", 
                                    "Female.Insecticide.Exposure", "Male.Insecticide.Exposure")


rf.predict.sam= predict(rf.model.sam, test.rf.dataset.sam)

accuracy = ifelse(rf.predict.sam == test.rf.dataset.sam$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/15000*100
#Model accuracy is 91.16

rf.plot.acc.sam = ggplot(rf.model.sam.df, aes(x=MeanDecreaseAccuracy,
                                                          y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("C")+
  theme_classic()

rf.plot.gini.sam = ggplot(rf.model.sam.df, aes(x=MeanDecreaseGini,
                                                           y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("D")+
  theme_classic()

gridExtra::grid.arrange(rf.plot.acc.sam, rf.plot.gini.sam,
                        nrow = 1)


((seq.rot.mix.rf.plot.acc + seq.rot.mix.rf.plot.gini)/
    (rf.plot.acc.sam + rf.plot.gini.sam)) + plot_annotation(title = "Random Forest Models: Sequences/Rotations vs Mixtures")


#Figure out what could be considered high/low coverage::

violin.plot.a = ggplot(rf.dataset.rot.seq, aes(x=Intervention.Coverage,
                                   y=operational.outcome))+
  geom_violin(fill = "aquamarine", alpha = 0.5,
              colour = "deepskyblue4")+
  geom_vline(xintercept = 0.5, colour = "black",
             linetype = "dashed", size = 2)+
  xlab("Intervention Coverage")+
  ylab("Operational Outcome")+
  ggtitle("A")+
  theme_classic()

violin.plot.b = ggplot(parameter.space.unique.rf, aes(x=Coverage,
                                          y=operational.outcome))+
  geom_violin(fill = "aquamarine", alpha = 0.5,
              colour = "deepskyblue4")+
  geom_vline(xintercept = 0.5, colour = "black",
             linetype = "dashed", size = 2)+
  xlab("Intervention Coverage")+
  ylab("Operational Outcome")+
  ggtitle("B")+
  theme_classic()

violin.plot.c = ggplot(rf.dataset.rot.seq.mix, aes(x=Intervention.Coverage,
                               y=operational.outcome))+
  geom_violin(fill = "aquamarine", alpha = 0.5,
              colour = "deepskyblue4")+
  geom_vline(xintercept = 0.5, colour = "black",
             linetype = "dashed", size = 2)+
  xlab("Intervention Coverage")+
  ylab("Operational Outcome")+
  ggtitle("C")+
  theme_classic()

violin.plot.d = ggplot(rf.parameter.space.unique.sam, aes(x=Coverage,
                                      y=operational.outcome))+
  geom_violin(fill = "aquamarine", alpha = 0.5,
               colour = "deepskyblue4")+
  geom_vline(xintercept = 0.5, colour = "black",
             linetype = "dashed", size = 2)+
  xlab("Intervention Coverage")+
  ylab("Operational Outcome")+
  ggtitle("D")+
  theme_classic()


(violin.plot.a + violin.plot.b)/(violin.plot.c + violin.plot.d)


ggplot(rf.dataset.rot.seq.mix, aes(x=Intervention.Coverage,
                                   y=operational.outcome))+
  geom_violin()+
  facet_wrap(~cross.selection)

#Remove simulations where coverage was "low": sequences and rotations:
#First for choosing between sequences and rotations:::


rf.dataset.rot.seq.high = rf.dataset.rot.seq%>%
  dplyr::filter(Intervention.Coverage >= 0.5)
## 70% of the sample size
sample.size = floor(0.7 * nrow(rf.dataset.rot.seq.high))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(rf.dataset.rot.seq.high)), size = sample.size)

train.rf.dataset.rot.seq.high = rf.dataset.rot.seq.high[train.ind, ]
test.rf.dataset.rot.seq.high = rf.dataset.rot.seq.high[-train.ind, ]

rf.model.high = randomForest::randomForest(operational.outcome ~ .,
                                      data = train.rf.dataset.rot.seq.high,
                                      type = "classification",
                                      importance = TRUE,
                                      ntree = 300,
                                      mtry = 4,
                                      node.size = 1000)


rf.model.seq.rot.df.high = data.frame(rf.model.high$importance)

rf.model.seq.rot.df.high$parameter = c("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                                  "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure",
                                  "Start.Resistance", "Cross.Selection")


rf.predic.seq.rot.high = predict(rf.model.high, test.rf.dataset.rot.seq.high)

accuracy = ifelse(rf.predic.seq.rot.high == test.rf.dataset.rot.seq.high$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/33000*100
#Model accuracy is 79.09%

seq.rot.rf.plot.acc.high = ggplot(rf.model.seq.rot.df.high, aes(x=MeanDecreaseAccuracy,
                                                      y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("A")+
  theme_classic()

seq.rot.rf.plot.gini.high = ggplot(rf.model.seq.rot.df.high, aes(x=MeanDecreaseGini,
                                                       y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("B")+
  theme_classic()

gridExtra::grid.arrange(seq.rot.rf.plot.acc.high, seq.rot.rf.plot.gini.high,
                        nrow = 1)



##Unique Insecticides Seq vs Adaptive Rot:
parameter.space.unique.rf.high = parameter.space.unique.rf%>%
  dplyr::filter(Coverage >= 0.5)

## 70% of the sample size
sample.size = floor(0.7 * nrow(parameter.space.unique.rf.high))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(parameter.space.unique.rf.high)), size = sample.size)

train.rf.dataset.seq.adrot.high = parameter.space.unique.rf.high[train.ind, ]
test.rf.dataset.seq.adrot.high= parameter.space.unique.rf.high[-train.ind, ]

rf.model.seq.adrot.high = randomForest::randomForest(operational.outcome ~ .,
                                                data = train.rf.dataset.seq.adrot.high,
                                                type = "classification",
                                                importance = TRUE,
                                                ntree = 300,
                                                mtry = 4,
                                                node.size = 1000)



rf.model.seq.adrot.df.high = data.frame(rf.model.seq.adrot.high$importance)

rf.model.seq.adrot.df.high$parameter = c("Heritability.1", "Heritability.2", "Fitness.Cost.1",
                                    "Fitness.Cost.2", "Start.Resistace.1", "Start.Resistance.2",
                                    "Cross.Selection", "Dispersal", "Intervention.Coverage", 
                                    "Female.Insecticide.Exposure", "Male.Insecticide.Exposure")


rf.predic.seq.adrot.high= predict(rf.model.seq.adrot.high, test.rf.dataset.seq.adrot.high)

accuracy = ifelse(rf.predic.seq.adrot.high == test.rf.dataset.seq.adrot.high$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/7500*100
#Model accuracy is 84.73%

seq.adrot.rf.plot.acc.high = ggplot(rf.model.seq.adrot.df.high, aes(x=MeanDecreaseAccuracy,
                                                          y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("C")+
  theme_classic()

seq.adrot.rf.plot.gini.high = ggplot(rf.model.seq.adrot.df.high, aes(x=MeanDecreaseGini,
                                                           y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("D")+
  theme_classic()

gridExtra::grid.arrange(seq.adrot.rf.plot.acc, seq.adrot.rf.plot.gini,
                        nrow = 1)


((seq.rot.rf.plot.acc.high + seq.rot.rf.plot.gini.high)/
    (seq.adrot.rf.plot.acc.high + seq.adrot.rf.plot.gini.high)) + plot_annotation(title = "Random Forest Models: Sequences vs Rotations for Intervention Coverage Greater than 0.5")






#Remove simulations where coverage was "low":
rf.dataset.rot.seq.mix.high = rf.dataset.rot.seq.mix%>%
  dplyr::filter(Intervention.Coverage >= 0.5)%>%
  dplyr::mutate(operational.outcome = as.factor(operational.outcome))

# 70% of the sample size
sample.size = floor(0.7 * nrow(rf.dataset.rot.seq.mix.high))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(rf.dataset.rot.seq.mix.high)), size = sample.size)

train.rf.dataset.rot.seq.mix.high = rf.dataset.rot.seq.mix.high[train.ind, ]
test.rf.dataset.rot.seq.mix.high = rf.dataset.rot.seq.mix.high[-train.ind, ]

rf.model.seq.rot.mix.high = randomForest::randomForest(operational.outcome ~ .,
                                                  data = train.rf.dataset.rot.seq.mix.high,
                                                  type = "classification",
                                                  importance = TRUE,
                                                  ntree = 300,
                                                  mtry = 4,
                                                  node.size = 1000)

rf.model.seq.rot.mix.high.df = data.frame(rf.model.seq.rot.mix.high$importance)
rf.model.seq.rot.mix.high.df$parameter = c("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                                      "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure",
                                      "Start.Resistance", "Cross.Selection")

rf.predic.seq.rot.mix.high= predict(rf.model.seq.rot.mix.high, test.rf.dataset.rot.seq.mix.high)


accuracy = ifelse(rf.predic.seq.rot.mix.high == test.rf.dataset.rot.seq.mix.high$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/33000*100
#Model accuracy is 94.31%

seq.rot.mix.rf.plot.acc.high = ggplot(rf.model.seq.rot.mix.high.df, aes(x=MeanDecreaseAccuracy,
                                                              y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("A")+
  theme_classic()

seq.rot.mix.rf.plot.gini.high = ggplot(rf.model.seq.rot.mix.high.df, aes(x=MeanDecreaseGini,
                                                               y=parameter))+
  
  geom_col(fill = "skyblue",
           colour = "blue")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("B")+
  theme_classic()


seq.rot.mix.rf.plot.acc.high
seq.rot.mix.rf.plot.gini.high




##Same for Unique Insecticides Seq vs Adaptive Rot vs Mixtures
rf.parameter.space.unique.sam.high = rf.parameter.space.unique.sam%>%
  dplyr::filter(Coverage >= 0.5)



## 75% of the sample size
sample.size = floor(0.7 * nrow(rf.parameter.space.unique.sam.high))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(rf.parameter.space.unique.sam.high)), size = sample.size)

train.rf.dataset.sam.high = rf.parameter.space.unique.sam.high[train.ind, ]
test.rf.dataset.sam.high= rf.parameter.space.unique.sam.high[-train.ind, ]

rf.model.sam.high = randomForest::randomForest(operational.outcome ~ .,
                                          data = train.rf.dataset.sam.high,
                                          type = "classification",
                                          importance = TRUE,
                                          ntree = 300,
                                          mtry = 4,
                                          node.size = 1000)

rf.model.sam.df.high = data.frame(rf.model.sam.high$importance)

rf.model.sam.df.high$parameter = c("Heritability.1", "Heritability.2", "Fitness.Cost.1",
                              "Fitness.Cost.2", "Start.Resistace.1", "Start.Resistance.2",
                              "Cross.Selection", "Dispersal", "Intervention.Coverage", 
                              "Female.Insecticide.Exposure", "Male.Insecticide.Exposure")


rf.predict.sam.high= predict(rf.model.sam.high, test.rf.dataset.sam.high)

accuracy = ifelse(rf.predict.sam.high == test.rf.dataset.sam.high$operational.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/7500*100
#Model accuracy is 87.63%

rf.plot.acc.sam.high = ggplot(rf.model.sam.df.high, aes(x=MeanDecreaseAccuracy,
                                              y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("C")+
  theme_classic()

rf.plot.gini.sam.high = ggplot(rf.model.sam.df.high, aes(x=MeanDecreaseGini,
                                               y=parameter))+
  
  geom_col(fill = "seagreen1",
           colour = "seagreen")+
  xlab("Mean Decrease Gini")+
  ylab("Parameter")+
  ggtitle("D")+
  theme_classic()

rf.plot.acc.sam.high
rf.plot.gini.sam.high






((seq.rot.mix.rf.plot.acc.high + seq.rot.mix.rf.plot.gini.high)/
    (rf.plot.acc.sam.high + rf.plot.gini.sam.high)) + plot_annotation(title = "Random Forest Models: Sequences/Rotations vs Mixtures for Intervention Coverage Greater than 0.5")







#Example plots of simulations::::
no.cross.selection = sequence.df.10%>%
  dplyr::filter(cross.selection == 0)%>%
  dplyr::filter(start.resistance == 0)%>%
  dplyr::filter(simulation.duration != 500)

sample.to.visualise = dplyr::sample_n(no.cross.selection, 1) #limit to those starting at 0. 



print(sample.to.visualise)

plot.seq.df = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 2,
                                                                              exposure.scaling.factor = 10,
                                                                              nsim = 1,
                                                                              minimum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              maximum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              minimum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              maximum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              minimum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              maximum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              resistance.cost = sample.to.visualise$Fitness.Cost,
                                                                              starting.treatment.site.intensity = sample.to.visualise$start.resistance.values,
                                                                              starting.refugia.intensity = sample.to.visualise$start.resistance.values,
                                                                              min.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              max.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              min.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              max.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              maximum.generations = 500, 
                                                                              irm.strategy = "sequence",
                                                                              half.population.bioassay.survival.resistance = 900,
                                                                              withdrawal.threshold.value = 0.1, 
                                                                              return.threshold.value = 0.08, 
                                                                              deployment.frequency = 10, 
                                                                              maximum.resistance.value = 25000),
                                  maximum.generations = 500, number.of.insecticides = 2)

plot.rot.df = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 2,
                                                                              exposure.scaling.factor = 10,
                                                                              nsim = 1,
                                                                              minimum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              maximum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              minimum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              maximum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              minimum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              maximum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              resistance.cost = sample.to.visualise$Fitness.Cost,
                                                                              starting.treatment.site.intensity = sample.to.visualise$start.resistance.values,
                                                                              starting.refugia.intensity = sample.to.visualise$start.resistance.values,
                                                                              min.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              max.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              min.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              max.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              maximum.generations = 500, 
                                                                              irm.strategy = "rotation",
                                                                              half.population.bioassay.survival.resistance = 900,
                                                                              withdrawal.threshold.value = 0.1, 
                                                                              return.threshold.value = 0.08, 
                                                                              deployment.frequency = 10, 
                                                                              maximum.resistance.value = 25000),
                                  maximum.generations = 500, number.of.insecticides = 2)

plot.mix.df = get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures(number.of.insecticides = 2,
                                                                                            exposure.scaling.factor = 10,
                                                                                            nsim = 1,
                                                                                            minimum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                                            maximum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                                            minimum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                                            maximum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                                            minimum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                                            maximum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                                            resistance.cost = sample.to.visualise$Fitness.Cost,
                                                                                            starting.treatment.site.intensity = sample.to.visualise$start.resistance.values,
                                                                                            starting.refugia.intensity = sample.to.visualise$start.resistance.values,
                                                                                            min.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                                            max.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                                            min.dispersal.rate = sample.to.visualise$Dispersal,
                                                                                            max.dispersal.rate = sample.to.visualise$Dispersal,
                                                                                            maximum.generations = 500, 
                                                                                            irm.deployment.strategy = "mixtures", 
                                                                                            mixture.strategy = "mix.sequential.discrete",
                                                                                            irm.switch.strategy = "sequence", 
                                                                                            half.population.bioassay.survival.resistance = 900,
                                                                                            withdrawal.threshold.value = 0.1, 
                                                                                            return.threshold.value = 0.08, 
                                                                                            deployment.frequency = 10, 
                                                                                            maximum.resistance.value = 25000, 
                                                                                            conversion.factor = 0.48,
                                                                                            intercept = 0.15),
                                                maximum.generations = 500, number.of.insecticides = 2)



###only plot the intervention sites:: convert resistance to bioassay
  plot.seq.df = plot.seq.df%>%
  dplyr::filter(site == "treatment")%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(mean.population.resistance = resistance.intensity,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      sd.population.resistance = 0, #measured without error
                                                                      nsim = 1,
                                                                      michaelis.menten.slope = 1)*100)
  
  
  

  plot.rot.df = plot.rot.df%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(mean.population.resistance = resistance.intensity,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      sd.population.resistance = 0, #measured without error
                                                                      nsim = 1,
                                                                      michaelis.menten.slope = 1)*100)
  
  
  
  plot.mix.df = plot.mix.df%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(mean.population.resistance = resistance.intensity,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      sd.population.resistance = 0, #measured without error
                                                                      nsim = 1,
                                                                      michaelis.menten.slope = 1)*100)


  panel_sequence = ggplot(plot.seq.df, aes(x=time.in.generations,
                                           y=bioassay.survival,
                                           colour = as.character(insecticide.tracked)))+
    geom_line(size = 2.5, alpha = 0.7)+
    scale_colour_manual(values = c("#e31a1c", "#377eb8"))+
    geom_point(aes(x=time.in.generations,
                  y=10, colour = insecticide.deployed),
              size = 1)+
    geom_hline(yintercept = 8)+
    geom_vline(xintercept = max(plot.seq.df$time.in.generations),
               colour = "orange",
               linetype = "dashed",
               size = 0.5)+
     scale_y_continuous(breaks = c(0, 5, 8, 10),
                        limits = c(0, 11))+
    xlim(0, 500)+
       ggtitle("Sequence")+
    xlab("Time in Generations")+
    ylab("Bioassay Survival (%)")+
    theme_classic()+
    theme(legend.position = "none")+
    theme(text = element_text(size = 15))
  

panel_rotation = ggplot(plot.rot.df, aes(x=time.in.generations,
                        y=bioassay.survival,
                        colour = as.character(insecticide.tracked)))+
  geom_line(size = 2.5, alpha = 0.7)+
  scale_colour_manual(values = c("#e31a1c", "#377eb8"))+
  geom_point(aes(x=time.in.generations,
                y=10, colour = insecticide.deployed,
                alpha = 0.6), size = 1)+
  geom_hline(yintercept = 8)+
  geom_vline(xintercept = max(plot.rot.df$time.in.generations),
               linetype ="dashed", 
               colour = "orange")+
  scale_y_continuous(breaks = c(0, 5, 8, 10),
                     limits = c(0, 11))+
  xlim(0, 500)+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Rotation")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(text = element_text(size = 15))
  
plot.mix.df.1 = plot.mix.df%>%
  dplyr::filter(insecticide.tracked == 1)

plot.mix.df.2 = plot.mix.df%>%
  dplyr::filter(insecticide.tracked == 2)


panel_mixture = ggplot(plot.mix.df, aes(x=time.in.generations,
                        y=bioassay.survival))+
  geom_line(data = plot.mix.df.1,
            aes(x=time.in.generations,
                y=bioassay.survival),
            colour = "#377eb8",
            size = 4, alpha =0.7)+
    geom_line(data = plot.mix.df.1,
              aes(x=time.in.generations,
                  y=bioassay.survival),
              size = 2,
              colour = "#e31a1c",
              alpha = 0.7)+
  geom_hline(yintercept = 10, colour = "#377eb8",
             size = 3)+
  geom_hline(yintercept = 10, colour = "#e31a1c",
             size = 1.5)+
  geom_hline(yintercept = 8)+
  geom_vline(xintercept = max(plot.mix.df$time.in.generations),
               linetype ="dashed", 
               colour = "orange")+
  scale_y_continuous(breaks = c(0, 5, 8, 10),
                     limits = c(0, 11))+
  xlim(0, 500)+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Mixture")+
  theme_classic()+
  theme(text = element_text(size = 15))

gridExtra::grid.arrange(panel_sequence,
                        panel_rotation,
                        panel_mixture,
                        nrow = 1)


####
#Demonstrating the Scaling of Beta (exposure scaling factor):::
response.values = response_to_insecticide_selection(exposure.scaling.factor = 10,
                                                    minimum.insecticide.resistance.heritability = 0.05,
                                                    maximum.insecticide.resistance.heritability = 0.30,
                                                    minimum.female.insecticide.exposure = 0.4,
                                                    maximum.female.insecticide.exposure = 0.9,
                                                    minimum.male.insecticide.exposure = 0,
                                                    maximum.male.insecticide.exposure = 1,
                                                    nsim = 100000)
calibrated =  10/response.values
df = data.frame(calibrated)

ggplot(df, aes(x=calibrated))+
  geom_histogram(bins = 100,
                 colour = "black",
                 fill = "lightblue",)+
  geom_vline(xintercept = 8, colour = "red",
             size = 2, alpha = 0.7)+
  geom_vline(xintercept = 12, colour = "red",
             size = 2, alpha = 0.7)+
  scale_x_continuous(breaks = seq(0, 80, by = 10))+
  xlab("Years to 10% Bioassay Survival")+
  ylab("Frequency")+
  theme_classic()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))





table_resistance_from_survival_and_sd(half.population.bioassay.survival.resistance = 900, 
                                      maximum.bioassay.survival.proportion = 1, 
                                      michaelis.menten.slope = 1, 
                                      bioassay.survival.values = c(0.01, 0.05, 0.1, 0.2, 0.5), 
                                      sd.population.values = c(0, 1, 5, 10, 25), 
                                      estimate.precision = 0.01, 
                                      nsim = 1000, 
                                      minimum.resistance.value = 0, 
                                      maximum.resistance.value = 25000)


df = table_resistance_from_survival_and_sd(half.population.bioassay.survival.resistance = 900, 
                                           maximum.bioassay.survival.proportion = 1, 
                                           michaelis.menten.slope = 1, 
                                           bioassay.survival.values = c(0, 0.05, 0.1, 0.2, 0.5), 
                                           sd.population.values = c(0, 1, 5, 10, 25), 
                                           estimate.precision = 0.001, 
                                           nsim = 1000, 
                                           minimum.resistance.value = 0, 
                                           maximum.resistance.value = 25000)

df = df%>%
  dplyr::mutate(resistance.values = round(resistance.values, digits = 3))

df








#Identifying Parameters to Inform Decision Making

library(randomForest)

parameter.space.rf = rbind(parameter.space.10.30,
                           parameter.space.10.30)


parameter.space.rf$outcome = rot.seq.mix.operational.outcome
parameter.space.rf$deployment.freq = c(rep(10, 110000), rep(30, 110000))
parameter.space.rf$replicate = seq(1, 220000, 1)


sample = caTools::sample.split(parameter.space.rf$replicate, SplitRatio = 0.7)

train = subset(parameter.space.rf, sample == TRUE)
test  = subset(parameter.space.rf, sample == FALSE)

rf.fit = randomForest(as.factor(outcome)~
                        Heritability+               
                        Male.Insecticide.Exposure+
                        Female.Insecticide.Exposure+
                        Fitness.Cost+
                        Intervention.Coverage+
                        Dispersal+
                        cross.selection.values+    
                        start.resistance.values+
                        deployment.freq,
                      data = parameter.space.rf,
                      ntree = 1000,
                      max.nodes = 20,
                      importance = TRUE)




pred = predict(rf.fit, newdata = test)

table(test$outcome, pred)

varImpPlot(rf.fit)





