#Hobbs_Quantatitive_Genetics_IR_Seq_vs_Rot_Simulations v1.2.R

##################################################################
#1. Simulations: Sequences and Rotations without Cross Selection.#
##################################################################
library(devtools)
load_all() #loads in polyres if opened the polyres R project first
library(lhs) #for latin hypercube sampling
library(ggplot2) #for plotting results
library(magrittr) # for %>%
#dplyr is also used but functions called through dplyr:: 
#please ensure install.packages("dplyr") has been used


#Make latin hypercube for sampling, that allows for x simulations total. There are 7 inputs that can be changed
#insecticide.resistance.hertiability
#male.insecticide.exposure
#female.insecticide.exposure
#resistance.cost; please not that this is often interchangably referred to as fitness costs.
#intervention.coverage
#dispersal ; please not that this is often interchangably referred to as migration

#prevent from re-running after original sampling
####df = data.frame(lhs::randomLHS(5000, 6)) #5000 random samples of the 6 input parameters.

#Rename columns; and change distributions to be correct
#for easier tracking of which variable is which.

df = df%>%
  dplyr::rename(insecticide.resistance.hertiability = X1)%>%
  dplyr::rename(male.insecticide.exposure = X2)%>%
  dplyr::rename(female.insecticide.exposure = X3)%>%
  dplyr::rename(resistance.cost = X4)%>%
  dplyr::rename(intervention.coverage = X5)%>%
  dplyr::rename(dispersal = X6)%>%
  dplyr::mutate(insecticide.resistance.hertiability = qunif(insecticide.resistance.hertiability, 0.003, 0.97))%>%#these are the extremes of published realised heritabilities (gives best and worst case scenarios - but do not take into account environmental effects)
  dplyr::mutate(male.insecticide.exposure = qunif(male.insecticide.exposure, 0, 1))%>% #
  dplyr::mutate(female.insecticide.exposure = qunif(female.insecticide.exposure, 0.4, 0.9))%>% #Defaults from Ian
  dplyr::mutate(resistance.cost = qunif(resistance.cost, 0.01, 0.2))%>%
  dplyr::mutate(intervention.coverage = qunif(intervention.coverage, 0.1, 0.9))%>%
  dplyr::mutate(dispersal = qunif(dispersal, 0.1, 0.9))

#Save the random parameter values as a csv file. 
#prevent saving over original samples
####write.csv(df, ".//lhs_values.csv") #only has the randomly selected values

#Read in the random parameter values.
df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/lhs_values.csv")
parameter.space = df[,2:7]
parameter.space = parameter.space%>%
  dplyr::rename(Heritability = insecticide.resistance.hertiability)%>%
  dplyr::rename(`Male Exposure` = male.insecticide.exposure)%>%
  dplyr::rename(`Female Exposure` = female.insecticide.exposure)%>%
  dplyr::rename(`Intervention Coverage` = intervention.coverage)%>%
  dplyr::rename(`Fitness Cost` = resistance.cost)%>%
  dplyr::rename(Dispersal = dispersal)

#Visually check suitable parameter space coverage - eg no patches with large gaps:
#if there is either 1.re-do parameter space sampling OR 2.increase number of samples 
plot(parameter.space)


#Strategy effectiveness could be impacted by frequency of deployment decisions (how swiftly insecticide changes can be made)
#and the number of insecticides available.
df = rbind(df, df, df) #3x as 2, 3, 4 insecticides

no.insecticides = c(rep(2, 5000), rep(3, 5000), rep(4, 5000))
df = cbind(df, no.insecticides)


freq.deployment = c(rep(5, 15000), rep(10, 15000), rep(20, 15000))#deployment decisions every 6 (IRS), 12 (IRS / LLIN), 24 (LLIN) months
df=cbind(df, freq.deployment)

#What would be the important outputs for comparison:
# 1. Total Duration of Interventions [Operational Lifespan]
# 2. The number of generations where resistance.intensity > withdrawal threshold
# 3. Number of generations where insecticide.deployed resistance > withdrawal threshold [Control Failure Generations]
# 5. average resistance intensity of the deployed insecticide 

temp.list.sequence = list() #create empty list to hold the loop

#First run for sequences [45000 runs]
for(v in 1: nrow(df)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = df$no.insecticides[v],
                                                               exposure.scaling.factor = 10,
                                                               nsim = 1,
                                                               minimum.insecticide.resistance.heritability = df$insecticide.resistance.hertiability[v],
                                                               maximum.insecticide.resistance.heritability = df$insecticide.resistance.hertiability[v],
                                                               minimum.male.insecticide.exposure = df$male.insecticide.exposure[v],
                                                               maximum.male.insecticide.exposure = df$male.insecticide.exposure[v],
                                                               minimum.female.insecticide.exposure = df$female.insecticide.exposure[v],
                                                               maximum.female.insecticide.exposure = df$female.insecticide.exposure[v],
                                                               resistance.cost = df$resistance.cost[v],
                                                               starting.treatment.site.intensity = 0,
                                                               starting.refugia.intensity = 0,
                                                               min.intervention.coverage = df$intervention.coverage[v],
                                                               max.intervention.coverage = df$intervention.coverage[v],
                                                               min.dispersal.rate = df$dispersal[v],
                                                               max.dispersal.rate = df$dispersal[v],
                                                               maximum.generations = 500, #appoximately 50 years
                                                               irm.strategy = "sequence", 
                                                               half.population.bioassay.survival.resistance = 900, 
                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = df$freq.deployment[v], #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, df$no.insecticides[v])
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = df$no.insecticides[v]
  
  dep.freq = df$freq.deployment[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  strategy = "sequence"
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)
  
  temp.list.sequence[[v]] = temp_2
}

#Convert to a single dataframe
sequence.df = do.call(rbind, temp.list.sequence)
sequence.df = cbind(sequence.df, df)



#Do the Rotation Runs
temp.list.rotation = list()
for(v in 1: nrow(df)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = df$no.insecticides[v],
                                                               exposure.scaling.factor = 10,
                                                               nsim = 1,
                                                               minimum.insecticide.resistance.heritability = df$insecticide.resistance.hertiability[v],
                                                               maximum.insecticide.resistance.heritability = df$insecticide.resistance.hertiability[v],
                                                               minimum.male.insecticide.exposure = df$male.insecticide.exposure[v],
                                                               maximum.male.insecticide.exposure = df$male.insecticide.exposure[v],
                                                               minimum.female.insecticide.exposure = df$female.insecticide.exposure[v],
                                                               maximum.female.insecticide.exposure = df$female.insecticide.exposure[v],
                                                               resistance.cost = df$resistance.cost[v],
                                                               starting.treatment.site.intensity = 0,
                                                               starting.refugia.intensity = 0,
                                                               min.intervention.coverage = df$intervention.coverage[v],
                                                               max.intervention.coverage = df$intervention.coverage[v],
                                                               min.dispersal.rate = df$dispersal[v],
                                                               max.dispersal.rate = df$dispersal[v],
                                                               maximum.generations = 500, #appoximately 50 years
                                                               irm.strategy = "rotation", 
                                                               half.population.bioassay.survival.resistance = 900, 
                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = df$freq.deployment[v], #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, df$no.insecticides[v])
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = df$no.insecticides[v]
  
  dep.freq = df$freq.deployment[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "rotation"
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)
  
  temp.list.rotation[[v]] = temp_2
}

rotation.df = do.call(rbind, temp.list.rotation)
rotation.df = cbind(rotation.df, df)

sequence.rotation.df = rbind(sequence.df, rotation.df)

#export data as csv file
#prevent overwriting
###write.csv(sequence.rotation.df, ".//sequence_rotation_df.csv")

sequence.rotation.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/sequence_rotation_df.csv")

sequence.df = sequence.rotation.df%>%
  dplyr::filter(strategy == "sequence")%>%
  dplyr::select(-"X.1")

replicate = seq(1, 45000, by = 1)

sequence.df = data.frame(sequence.df, replicate)

rotation.df = sequence.rotation.df%>%
  dplyr::filter(strategy == "rotation")%>%
  dplyr::select(-"X.1")

rotation.df = data.frame(rotation.df, replicate)

sequence_df_2 = sequence.df%>%
  dplyr::rename("simulation.duration.sequence" = simulation.duration)%>%
  dplyr::rename("average.resistance.intensity.sequence" = mean.resistance.intensity.)%>%
  dplyr::rename("exceedance.generations.sequence" = exceedance.generations)%>%
  dplyr::rename("exceedance.generations.deployed.sequence" = exceedance.generations.deployed)%>%
  dplyr::rename("peak.resistance.sequence" = peak.resistance)%>%
  dplyr::select(-"strategy")%>%
  dplyr::rowwise()%>%
  dplyr::mutate(peak.survival.sequence = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                         mean.population.resistance = peak.resistance.sequence,
                                                                         michaelis.menten.slope = 1, 
                                                                         half.population.bioassay.survival.resistance = 900,
                                                                         sd.population.resistance = 0, 
                                                                         nsim = 1))%>%
  dplyr::mutate(average.survival.sequence = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                            mean.population.resistance = average.resistance.intensity.sequence,
                                                                            michaelis.menten.slope = 1, 
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            sd.population.resistance = 0, 
                                                                            nsim = 1))


rotation_df_2 = rotation.df%>%
  dplyr::rename("simulation.duration.rotation" = simulation.duration)%>%
  dplyr::rename("average.resistance.intensity.rotation" = mean.resistance.intensity.)%>%
  dplyr::rename("exceedance.generations.rotation" = exceedance.generations)%>%
  dplyr::rename("exceedance.generations.deployed.rotation" = exceedance.generations.deployed)%>%
  dplyr::rename("peak.resistance.rotation" = peak.resistance)%>%
  dplyr::select(-"strategy")%>%
  dplyr::rowwise()%>%
  dplyr::mutate(average.survival.rotation = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                            mean.population.resistance = average.resistance.intensity.rotation,
                                                                            michaelis.menten.slope = 1, 
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            sd.population.resistance = 0, 
                                                                            nsim = 1))%>%
  dplyr::mutate(peak.survival.rotation = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                         mean.population.resistance = peak.resistance.rotation,
                                                                         michaelis.menten.slope = 1, 
                                                                         half.population.bioassay.survival.resistance = 900,
                                                                         sd.population.resistance = 0, 
                                                                         nsim = 1))



all_sims_join =  dplyr::inner_join(sequence_df_2, rotation_df_2)

all_sims_join = all_sims_join%>%
  dplyr::mutate(diff.duration = simulation.duration.sequence - simulation.duration.rotation)%>%
  dplyr::mutate(diff.av.resistance = average.resistance.intensity.sequence - average.resistance.intensity.rotation)%>%
  dplyr::mutate(diff.exceed.gens = exceedance.generations.sequence - exceedance.generations.rotation)%>%
  dplyr::mutate(diff.exceed.gens.deployed = exceedance.generations.deployed.sequence - exceedance.generations.deployed.rotation)%>%
  dplyr::mutate(diff.peak.resistance = peak.resistance.sequence - peak.resistance.rotation)%>%
  dplyr::mutate(diff.av.survival = average.survival.sequence - average.survival.rotation)%>%
  dplyr::mutate(diff.peak.survival = peak.survival.sequence - peak.survival.rotation)



#Export data as csv file
#prevent overwriting
###write.csv(all_sims_join, ".//seq_rot_sims.csv")


###################################################################################################
#2. Parameter space testing comparing the SEQUENCE IRM strategy versus the ROTATION IRM strategy. #
#                 INCLUDING CROSS SELECTION                                                       #
###################################################################################################

#Cross selction values can be c(-0.3, -0.2, -0.1, 0.1, 0.2, 0.3). #0 cross resistance taken from previous simulations
#Use only 2 insecticides and 10 generation deployment: as this will allow comparison against mixtures better
#WHY: Differences between rotation and sequences greatest at this point.
#Also, issue with cross resistance with higher number of insecticides 
sequence.rotation.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/sequence_rotation_df.csv")

#Take from no cross resistance for the 0 cross resistance to save computational time:
sequence.df.zero = sequence.rotation.df%>%
  dplyr::filter(no.insecticides == 2)%>%
  dplyr::filter(freq.deployment == 10)%>%
  dplyr::filter(strategy == "sequence")%>%
  dplyr::mutate(cross.resistance = 0) #cross resistance is zero

rotation.df.zero = sequence.rotation.df%>%
  dplyr::filter(no.insecticides == 2)%>%
  dplyr::filter(freq.deployment == 10)%>%
  dplyr::filter(strategy == "rotation")%>%
  dplyr::mutate(cross.resistance = 0) # cross resistance is zero

#read in previously used random parameters
df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.2/lhs_values.csv")

cross.resistance = c(rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000), 
                     rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000)) #30,000

df.cross = rbind(df, df, df,
                 df, df, df)

df.cross = cbind(df.cross, cross.resistance)


#Do the Sequence Runs
temp.list.sequence.cross = list()
for(v in 1: nrow(df.cross)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = df.cross$insecticide.resistance.hertiability[v],
                                                                               maximum.insecticide.resistance.heritability = df.cross$insecticide.resistance.hertiability[v],
                                                                               minimum.male.insecticide.exposure = df.cross$male.insecticide.exposure[v],
                                                                               maximum.male.insecticide.exposure = df.cross$male.insecticide.exposure[v],
                                                                               minimum.female.insecticide.exposure = df.cross$female.insecticide.exposure[v],
                                                                               maximum.female.insecticide.exposure = df.cross$female.insecticide.exposure[v],
                                                                               resistance.cost = df.cross$resistance.cost[v],
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = df.cross$intervention.coverage[v],
                                                                               max.intervention.coverage = df.cross$intervention.coverage[v],
                                                                               min.dispersal.rate = df.cross$dispersal[v],
                                                                               max.dispersal.rate = df.cross$dispersal[v],
                                                                               maximum.generations = 500, #appoximately 50 years
                                                                               irm.strategy = "sequence", 
                                                                               half.population.bioassay.survival.resistance = 900, 
                                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                               maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                               min.cross.selection = df.cross$cross.resistance[v],
                                                                               max.cross.selection = df.cross$cross.resistance[v]), 
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
  
  cross.resistance = df.cross$cross.resistance[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "sequence"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
                      cross.resistance)
  
  temp.list.sequence.cross[[v]] = temp_2
}

sequence.df.cross = do.call(rbind, temp.list.sequence.cross)
sequence.df.cross = cbind(sequence.df.cross, df.cross)

#Do the Rotation Runs
temp.list.rotation.cross = list()
for(v in 1: nrow(df.cross)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = df.cross$insecticide.resistance.hertiability[v],
                                                                               maximum.insecticide.resistance.heritability = df.cross$insecticide.resistance.hertiability[v],
                                                                               minimum.male.insecticide.exposure = df.cross$male.insecticide.exposure[v],
                                                                               maximum.male.insecticide.exposure = df.cross$male.insecticide.exposure[v],
                                                                               minimum.female.insecticide.exposure = df.cross$female.insecticide.exposure[v],
                                                                               maximum.female.insecticide.exposure = df.cross$female.insecticide.exposure[v],
                                                                               resistance.cost = df.cross$resistance.cost[v],
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = df.cross$intervention.coverage[v],
                                                                               max.intervention.coverage = df.cross$intervention.coverage[v],
                                                                               min.dispersal.rate = df.cross$dispersal[v],
                                                                               max.dispersal.rate = df.cross$dispersal[v],
                                                                               maximum.generations = 500, #appoximately 50 years
                                                                               irm.strategy = "rotation", 
                                                                               half.population.bioassay.survival.resistance = 900, 
                                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                               maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                               min.cross.selection = df.cross$cross.resistance[v],
                                                                               max.cross.selection = df.cross$cross.resistance[v]), 
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
  
  cross.resistance = df.cross$cross.resistance[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "rotation"
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
                      cross.resistance)
  
  temp.list.rotation.cross[[v]] = temp_2
}

rotation.df.cross = do.call(rbind, temp.list.rotation.cross)
rotation.df.cross = cbind(rotation.df.cross, df.cross)

sequence.rotation.cross.df = rbind(sequence.df.cross, rotation.df.cross)

#export data as csv file
#prevent overwriting
#write.csv(sequence.rotation.cross.df, ".//sequence_rotation_cross_df.csv")

sequence.rotation.cross.df = sequence.rotation.cross.df%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.","exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "insecticides.in.sim", "dep.freq", "cross.resistance", "insecticide.resistance.hertiability",
                "male.insecticide.exposure", "female.insecticide.exposure", "resistance.cost", "intervention.coverage", "dispersal", "cross.resistance")


sequence.df.zero = sequence.df.zero%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.","exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "insecticides.in.sim", "dep.freq", "cross.resistance", "insecticide.resistance.hertiability",
                "male.insecticide.exposure", "female.insecticide.exposure", "resistance.cost", "intervention.coverage", "dispersal", "cross.resistance")


rotation.df.zero = rotation.df.zero%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.","exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "insecticides.in.sim", "dep.freq", "cross.resistance", "insecticide.resistance.hertiability",
                "male.insecticide.exposure", "female.insecticide.exposure", "resistance.cost", "intervention.coverage", "dispersal", "cross.resistance")


sequence.rotation.cross.full.df = rbind(sequence.df.zero, rotation.df.zero, sequence.rotation.cross.df)

#Convert intensities to bioassay survival::
sequence.rotation.cross.full.df = sequence.rotation.cross.full.df%>%
  rowwise()%>%
  dplyr::mutate(average.bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                            mean.population.resistance = mean.resistance.intensity.,
                                                                            michaelis.menten.slope = 1, 
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            sd.population.resistance = 0, 
                                                                            nsim = 1))%>%
  dplyr::mutate(peak.bioassay.survival =  resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                          mean.population.resistance = peak.resistance,
                                                                          michaelis.menten.slope = 1, 
                                                                          half.population.bioassay.survival.resistance = 900,
                                                                          sd.population.resistance = 0, 
                                                                          nsim = 1))


#give each row a unique replicate:
sim.replicate = seq(1, 70000, by = 1)

sequence.rotation.cross.full.df = data.frame(sequence.rotation.cross.full.df, sim.replicate)

#write and then subsequently prevent from overwriting
###write.csv(sequence.rotation.cross.full.df, ".//sequence.rotation.cross.full.df.csv")






