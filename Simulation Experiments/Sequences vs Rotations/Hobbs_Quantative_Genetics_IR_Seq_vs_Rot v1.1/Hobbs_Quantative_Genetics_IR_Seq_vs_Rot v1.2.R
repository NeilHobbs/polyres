##################################################################################################
#1. Parameter space testing comparing the SEQUENCE IRM strategy versus the ROTATION IRM strategy.#
##################################################################################################
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
df = read.csv("./lhs_values.csv")
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


sequence.rotation.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/sequence_rotation_df.csv")

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


##########Data Analysis and Data Visualisation###########
library(ggplot2)
library(dplyr)
library(epiR)
library(gridExtra)

sequence.rotation.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/sequence_rotation_df.csv")

df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/lhs_values.csv")#The parameters used in the parameter space testing

#get random sample for example plots
sample1 = dplyr::sample_n(df, 1, replace=TRUE) #random parameters from parameter space
sample.insecticides = sample(c(2, 3, 4), 1, replace = TRUE)
sample.frequency = sample(c(5, 10, 20), 1, replace = TRUE)

#Then run with -0.3, 0, and 0.3 cross selection. #worst and best case examples.

#Run first time then prevent from running
#example.dataframe = data.frame(sample1, sample.insecticides, sample.frequency)
#write.csv(example.dataframe, ".//random_parameter_sample.csv")

#read in the random sample
example.dataframe = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/random_parameter_sample.csv")

seq.example.c0 = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                   exposure.scaling.factor = 10,
                                                                   nsim = 1,
                                                                   minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                   maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                   minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                   maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                   minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                   maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                   resistance.cost = example.dataframe$resistance.cost,
                                                                   starting.treatment.site.intensity = 0,
                                                                   starting.refugia.intensity = 0,
                                                                   min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                   max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                   min.dispersal.rate = example.dataframe$dispersal,
                                                                   max.dispersal.rate = example.dataframe$dispersal,
                                                                   maximum.generations = 500, #appoximately 50 years
                                                                   irm.strategy = "sequence", 
                                                                   half.population.bioassay.survival.resistance = 900, 
                                                                   withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                   return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                   deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                   maximum.resistance.value = 25000 #have arbitrarily high just in case
) , 500, 2)

rot.example.c0 = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                   exposure.scaling.factor = 10,
                                                                   nsim = 1,
                                                                   minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                   maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                   minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                   maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                   minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                   maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                   resistance.cost = example.dataframe$resistance.cost,
                                                                   starting.treatment.site.intensity = 0,
                                                                   starting.refugia.intensity = 0,
                                                                   min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                   max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                   min.dispersal.rate = example.dataframe$dispersal,
                                                                   max.dispersal.rate = example.dataframe$dispersal,
                                                                   maximum.generations = 500, #appoximately 50 years
                                                                   irm.strategy = "rotation", 
                                                                   half.population.bioassay.survival.resistance = 900, 
                                                                   withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                   return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                   deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                   maximum.resistance.value = 25000 #have arbitrarily high just in case
) , 500, 2)

seq.example.neg = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                      exposure.scaling.factor = 10,
                                                                      nsim = 1,
                                                                      minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      resistance.cost = example.dataframe$resistance.cost,
                                                                      starting.treatment.site.intensity = 0,
                                                                      starting.refugia.intensity = 0,
                                                                      min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      min.dispersal.rate = example.dataframe$dispersal,
                                                                      max.dispersal.rate = example.dataframe$dispersal,
                                                                      maximum.generations = 500, #appoximately 50 years
                                                                      irm.strategy = "sequence", 
                                                                      half.population.bioassay.survival.resistance = 900, 
                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                      deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                      maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                      min.cross.selection = -0.3,
                                                                      max.cross.selection = -0.3), 500, 2)

rot.example.neg = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                      exposure.scaling.factor = 10,
                                                                      nsim = 1,
                                                                      minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      resistance.cost = example.dataframe$resistance.cost,
                                                                      starting.treatment.site.intensity = 0,
                                                                      starting.refugia.intensity = 0,
                                                                      min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      min.dispersal.rate = example.dataframe$dispersal,
                                                                      max.dispersal.rate = example.dataframe$dispersal,
                                                                      maximum.generations = 500, #appoximately 50 years
                                                                      irm.strategy = "rotation", 
                                                                      half.population.bioassay.survival.resistance = 900, 
                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                      deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                      maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                      min.cross.selection = -0.3,
                                                                      max.cross.selection = -0.3), 500, 2)


seq.example.pos = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                                       exposure.scaling.factor = 10,
                                                                                       nsim = 1,
                                                                                       minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       resistance.cost = example.dataframe$resistance.cost,
                                                                                       starting.treatment.site.intensity = 0,
                                                                                       starting.refugia.intensity = 0,
                                                                                       min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       min.dispersal.rate = example.dataframe$dispersal,
                                                                                       max.dispersal.rate = example.dataframe$dispersal,
                                                                                       maximum.generations = 500, #appoximately 50 years
                                                                                       irm.strategy = "sequence", 
                                                                                       half.population.bioassay.survival.resistance = 900, 
                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                       deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                       maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                                       min.cross.selection = 0.3,
                                                                                       max.cross.selection = 0.3), 500, 2)

rot.example.pos = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                       exposure.scaling.factor = 10,
                                                                       nsim = 1,
                                                                       minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                       maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                       minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                       maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                       minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                       maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                       resistance.cost = example.dataframe$resistance.cost,
                                                                       starting.treatment.site.intensity = 0,
                                                                       starting.refugia.intensity = 0,
                                                                       min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                       max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                       min.dispersal.rate = example.dataframe$dispersal,
                                                                       max.dispersal.rate = example.dataframe$dispersal,
                                                                       maximum.generations = 500, #appoximately 50 years
                                                                       irm.strategy = "rotation", 
                                                                       half.population.bioassay.survival.resistance = 900, 
                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                       deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                       maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                       min.cross.selection = 0.3,
                                                                       max.cross.selection = 0.3) , 500, 2)

rot.example.c0.plot = plot_simulation(simulation.dataframe = rot.example.c0, 
                                      half.population.bioassay.survival.resistance = 900, 
                                      withdrawal.threshold = 0.1,
                                      return.threshold = 0.05)

seq.example.c0.plot = plot_simulation(simulation.dataframe = seq.example.c0,
                                      half.population.bioassay.survival.resistance = 900, 
                                      withdrawal.threshold = 0.1,
                                      return.threshold = 0.05)

rot.example.neg.plot = plot_simulation(simulation.dataframe = rot.example.neg,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

seq.example.neg.plot = plot_simulation(simulation.dataframe = seq.example.neg,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

rot.example.pos.plot = plot_simulation(simulation.dataframe = rot.example.pos,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

seq.example.pos.plot = plot_simulation(simulation.dataframe = seq.example.pos,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

#This is figure 1
gridExtra::grid.arrange(seq.example.c0.plot, rot.example.c0.plot,
                        seq.example.neg.plot, rot.example.neg.plot,
                        seq.example.pos.plot, rot.example.pos.plot,
                        ncol = 2, nrow = 3,
                        top = "Sequences                                                                                                                                       Rotations",
                        left = "Positive Cross Selection          Negative Cross Selection          No Cross Selection")


#Has differences between Rot and Seq
all_sims_join = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/seq_rot_sims.csv")

replicate = seq(1, 45000, by = 1)

all_sims_join = data.frame(all_sims_join, replicate)

#Where the final simulation duration was a tie
sim_equal_gens = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence == simulation.duration.rotation)

rotation_wins = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence < simulation.duration.rotation)

sequence_wins = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence > simulation.duration.rotation)


#Calculate as proprtions
nrow(rotation_wins)/nrow(all_sims_join) #rotation wins
nrow(sequence_wins)/nrow(all_sims_join) #sequence wins
nrow(sim_equal_gens)/nrow(all_sims_join) #draws

#Calculate as proprtions
nrow(rotation_wins%>%
       dplyr::filter(no.insecticides == 2))

nrow(rotation_wins%>%
       dplyr::filter(no.insecticides == 3))

nrow(rotation_wins%>%
       dplyr::filter(no.insecticides == 4))


nrow(sequence_wins)/nrow(all_sims_join) #sequence wins
nrow(sim_equal_gens)/nrow(all_sims_join) #draws




#Creates a dataset for putting in labels for putting in the 
label.df = data.frame(
  text.label = c("Favours Sequences", "Favours Rotations"),
  label_x_coord = c(46000, 46000), #have the label fairly central.
  label_y_coord = c(200, -200)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq = data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(7500, 22500, 37500),
  label_y_coord = c(360, 360, 360)
)

#This is currently Figure 2
#Scatterplot of difference in duration (n=16815).
ggplot(rotation_wins, aes(x = replicate.1, y=diff.duration)) +
  geom_point(aes(colour = as.factor(insecticides.in.sim)),
                 alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
  geom_vline(xintercept = 15000, linetype = "dashed") +
  geom_vline(xintercept = 30000, linetype = "dashed")+
  geom_hline(yintercept = 0, size = 1)+
  xlab("Simulation Replicate Pair")+ 
  ylab("Difference in Simulation Duration (Generations)")+ 
  ylim(-360, 360)+#set as the maximum range
  geom_text(data = label.df, aes(label = text.label,
                                  x=label_x_coord,
                                  y=label_y_coord),
                                  angle = 270, size = 5)+
  geom_label(data = label.df.depfreq, aes(label = text.label,
                                          x=label_x_coord,
                                          y=label_y_coord),
                                          fill = "orchid")+
  theme_classic()+
  theme(legend.position = "none")


#Creates a dataset for putting in labels for putting in the 
label.df1 = data.frame(
  text.label = c("Favours Rotations", "Favours Sequences"),
  label_x_coord = c(45500, 45500), #have the label fairly central.
  label_y_coord = c(2.5, -2.50)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq1 = data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(7500, 22500, 37500),
  label_y_coord = c(6, 6, 6)
)


#plot of the difference average bioassay survival to the deployed insecticide (n=15589)
    #this is figure 3.
ggplot(sim_equal_gens, aes(x = replicate.1, y= diff.av.survival*100))+
  geom_hline(yintercept = 0, size = 2)+
  geom_point(aes(colour=as.factor(insecticides.in.sim)),alpha = 0.3)+
  geom_vline(xintercept = 15000, linetype = "dashed") +
  geom_vline(xintercept = 30000, linetype = "dashed")+
  xlab("Simulation Replicate Pair")+ 
  ylab("Difference in Mean Bioassay Survival Percentage to Deployed Insecticide")+ 
  ylim(-6, 6)+#set as the maximum range
  geom_text(data = label.df1, aes(label = text.label,
                                 x=label_x_coord,
                                 y=label_y_coord),
            angle = 270, size = 5)+
  geom_label(data = label.df.depfreq1, aes(label = text.label,
                                          x=label_x_coord,
                                          y=label_y_coord),
             fill = "orchid")+
  theme_bw()+
  theme(legend.position = "none")



######################################
range(sim_equal_gens$diff.peak.survival)
#Creates a dataset for putting in labels for putting in the 
label.df3 = data.frame(
  text.label = c("Favours Sequences", "Favours Rotations"),
  label_x_coord = c(45500, 45500), #have the label fairly central.
  label_y_coord = c(-0.05, 0.05)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq3= data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(7500, 22500, 37500),
  label_y_coord = c(0.12, 0.12, 0.12)
)

#Plot of the difference in the peak bioassay survival
#this is Figure 4
ggplot(sim_equal_gens, aes(x = replicate.1, y= diff.peak.survival)) +
  geom_point(aes(colour = as.factor(insecticides.in.sim)), alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
  xlab("Simulation Replicate")+ 
  ylab("Difference in the Peak Bioassay Survival Percentage")+ 
  ylim(-0.12, 0.12)+#set as the maximum range
  geom_vline(xintercept = 15000, linetype = "dashed") +
  geom_vline(xintercept = 30000, linetype = "dashed")+
  xlab("Simulation Replicate Pair")+ 
  ylab("Difference in Peak Bioassay Survival Percentage")+ 
  geom_text(data = label.df3, aes(label = text.label,
                                  x=label_x_coord,
                                  y=label_y_coord),
            angle = 270, size = 5)+
  geom_label(data = label.df.depfreq3, aes(label = text.label,
                                           x=label_x_coord,
                                           y=label_y_coord),
             fill = "orchid")+
  theme_bw()+
  theme(legend.position = "none")


range(sim_equal_gens$diff.exceed.gens.deployed)
#Creates a dataset for putting in labels for putting in the 
label.df4 = data.frame(
  text.label = c("Favours Rotations", "Favours Sequences"),
  label_x_coord = c(45500, 45500), #have the label fairly central.
  label_y_coord = c(85, -85)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq4= data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(7500, 22500, 37500),
  label_y_coord = c(170, 170, 170)
)



#Plot of the difference in the number of control failure generations
  #This is figure 5
ggplot(sim_equal_gens, aes(x = replicate.1, y= diff.exceed.gens.deployed)) +
  geom_point(alpha = 0.3,
             aes(colour = as.factor(no.insecticides))
             )+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
  xlab("Simulation Replicate Pair")+ 
  ylab("Difference in the Number of Control Failure Generations")+ 
  ylim(-170, 170)+#set as the maximum range
  geom_vline(xintercept = 15000, linetype = "dashed") +
  geom_vline(xintercept = 30000, linetype = "dashed")+
  geom_text(data = label.df4, aes(label = text.label,
                                  x=label_x_coord,
                                  y=label_y_coord),
            angle = 270, size = 4)+
  geom_label(data = label.df.depfreq4, aes(label = text.label,
                                           x=label_x_coord,
                                           y=label_y_coord),
             fill = "orchid")+
  theme_bw()+
  theme(legend.position = "none")


#partial correlation needs each endpoint as the final column. 
df_from_pcor_test = function(number.insecticides,
                             irm.strategy,
                             deploy.interval,
                             data){
  
  pcor.df = data%>%
    dplyr::filter(strategy == strategy)%>%
    dplyr::filter(no.insecticides == number.insecticides)%>%
    dplyr::filter(dep.freq == deploy.interval)%>%
    dplyr::select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
                  "intervention.coverage", "dispersal", "female.insecticide.exposure", "mean.resistance.intensity.")
  
  
  pcor.output = epiR::epi.prcc(dat = pcor.df,
                               sided.test = 2,
                               conf.level = 0.95)
  
  
  estimate = pcor.output$est
  pvalue = pcor.output$p.value
  teststat = pcor.output$test.statistic
  lower.95.ci = pcor.output$lower
  upper.95.ci = pcor.output$upper
  deployment.interval = deploy.interval
  
  
  insecticides = as.factor(number.insecticides)
  strategy = irm.strategy
  IRM.Strategy = stringr::str_c(irm.strategy, " with ", number.insecticides, " insecticides")
  
  #parameters in correct order::
  parameter = c("Insecticide Resistance Heritability", "Fitness Cost", "Male Insecticide Exposure",
                "Intervention Coverage", "Mosquito Dispersal", "Female Insecticide Exposure")
  
  output = data.frame(estimate, lower.95.ci, upper.95.ci,  pvalue, teststat, insecticides, strategy, parameter, deployment.interval, IRM.Strategy)
  
  return(output)
}

pcor.seq.2.5.df = df_from_pcor_test(2, "sequence", 5, sequence.rotation.df)
pcor.seq.3.5.df = df_from_pcor_test(3, "sequence", 5, sequence.rotation.df)
pcor.seq.4.5.df = df_from_pcor_test(4, "sequence", 5, sequence.rotation.df)
pcor.rot.2.5.df = df_from_pcor_test(2, "rotation", 5, sequence.rotation.df)
pcor.rot.3.5.df = df_from_pcor_test(3, "rotation", 5, sequence.rotation.df)
pcor.rot.4.5.df = df_from_pcor_test(4, "rotation", 5, sequence.rotation.df)

pcor.seq.2.10.df = df_from_pcor_test(2, "sequence", 10, sequence.rotation.df)
pcor.seq.3.10.df = df_from_pcor_test(3, "sequence", 10, sequence.rotation.df)
pcor.seq.4.10.df = df_from_pcor_test(4, "sequence", 10, sequence.rotation.df)
pcor.rot.2.10.df = df_from_pcor_test(2, "rotation", 10, sequence.rotation.df)
pcor.rot.3.10.df = df_from_pcor_test(3, "rotation", 10, sequence.rotation.df)
pcor.rot.4.10.df = df_from_pcor_test(4, "rotation", 10, sequence.rotation.df)

pcor.seq.2.20.df = df_from_pcor_test(2, "sequence", 20, sequence.rotation.df)
pcor.seq.3.20.df = df_from_pcor_test(3, "sequence", 20, sequence.rotation.df)
pcor.seq.4.20.df = df_from_pcor_test(4, "sequence", 20, sequence.rotation.df)
pcor.rot.2.20.df = df_from_pcor_test(2, "rotation", 20, sequence.rotation.df)
pcor.rot.3.20.df = df_from_pcor_test(3, "rotation", 20, sequence.rotation.df)
pcor.rot.4.20.df = df_from_pcor_test(4, "rotation", 20, sequence.rotation.df)





pcor.df.all = rbind(pcor.seq.2.5.df,
                    pcor.seq.3.5.df,
                    pcor.seq.4.5.df, 
                    pcor.rot.2.5.df, 
                    pcor.rot.3.5.df, 
                    pcor.rot.4.5.df,
                    pcor.seq.2.10.df,
                    pcor.seq.3.10.df,
                    pcor.seq.4.10.df, 
                    pcor.rot.2.10.df, 
                    pcor.rot.3.10.df, 
                    pcor.rot.4.10.df,
                    pcor.seq.2.20.df,
                    pcor.seq.3.20.df,
                    pcor.seq.4.20.df, 
                    pcor.rot.2.20.df, 
                    pcor.rot.3.20.df, 
                    pcor.rot.4.20.df)


  #Prevent overwriting
####write.csv(pcor.df.all, ".//partial_rank_correlation.csv")

pcor.df.all = read.csv(".//partial_rank_correlation.csv")
pcor.df.all = pcor.df.all%>%
  dplyr::rename(`IRM Strategy` = IRM.Strategy)

#This is Figure 65
pals = c("#b8e186", "#7fbc41", "#4d9221", "#f1b6da", "#de77ae", "#c51b7d")


#https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
#Artem Sokolov
shift_legend3 = function(p) {
  pnls = cowplot::plot_to_gtable(p) %>% 
    gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% 
    purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

facet.labels.deployment.interval = c("5 Generation Deployment Interval",
                                     "10 Generation Deployment Interval",
                                     "20 Generation Deployment Interval")

#This is FIGURE 6
shift_legend3(ggplot(pcor.df.all, aes(x=estimate, y=parameter, xmin = lower.95.ci,
                                      xmax = upper.95.ci, fill = `IRM Strategy`)) +
                geom_bar(stat = "identity", position = position_dodge(), aes(x=estimate)) +
                geom_errorbarh(position = position_dodge())+
                scale_fill_manual(values = pals)+
                geom_vline(xintercept = 0) +
                xlim(-0.75, 0.75)+
                ylab("Parameter") +
                xlab("Correlation") +
                guides(fill=guide_legend(ncol=2)) +
                theme_bw()+
                facet_wrap(~deployment.interval,
                           ncol = 2))





#Exceedance generations deployed is poisson
sequence.rotation.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/sequence_rotation_df.csv")

#make sure the number of insecticides and deployment intervals are set as factors
sequence.rotation.df$dep.freq = factor(sequence.rotation.df$dep.freq,
                                       levels = c("5", "10", "20"))

sequence.rotation.df$insecticides.in.sim = factor(sequence.rotation.df$insecticides.in.sim,
                                       levels = c("2", "3", "4"))

#Distributions of the response variables:
ggplot(sequence.rotation.df, aes(x=simulation.duration))+
  geom_histogram(binwidth = 20)

sequence.rotation.df$time.remaining = 500 - sequence.rotation.df$simulation.duration
sequence.rotation.df$duration.completion = sequence.rotation.df$simulation.duration / 500


#Distributions of the response variables:
ggplot(sequence.rotation.df, aes(x=duration.completion))+
  geom_histogram(binwidth = 0.05)


#Fit Geneneral Additive Models: Smooth over each parameter to check for non-linear relationships
gam.fit = mgcv::gam(formula = duration.completion ~ 
              resistance.cost+ # linear
              s(dispersal)+ #spline at 0.25; makes sense as eventually the populations average out at the higher dispersals.
              intervention.coverage+ #broadly linear
              insecticide.resistance.hertiability+ #broadly linear
              male.insecticide.exposure+ #linear
              female.insecticide.exposure+ #linear
                dep.freq+
                insecticides.in.sim+
                strategy,
            data = sequence.rotation.df)

plot(gam.fit)

sequence.rotation.df$dispersal.spline1 = ifelse(sequence.rotation.df$dispersal > 0.25, sequence.rotation.df$dispersal, 0)

find.max.logLik = function(disp1){
  
  sequence.rotation.df$dispersal.spline1 = ifelse(sequence.rotation.df$dispersal > disp1, sequence.rotation.df$dispersal, 0)
 
  glm.fit.A = glm(formula = duration.completion ~ 
                    resistance.cost+
                    dispersal+
                    dispersal.spline1+
                    intervention.coverage+
                    insecticide.resistance.hertiability+
                    male.insecticide.exposure+
                    female.insecticide.exposure+
                    dep.freq+
                    insecticides.in.sim+
                    strategy,
                  method = "glm.fit",
                  family = "binomial",
                  data = sequence.rotation.df)
  
  return(logLik(glm.fit.A))
}

find.max.logLik(disp1 = 0.22) #'log Lik.' -17056.78 (df=13)
find.max.logLik(disp1 = 0.23) #'log Lik.' -17057.3 (df=13)
find.max.logLik(disp1 = 0.24) #'log Lik.' -17057.16 (df=13)

#dispersal spline at 0.23 maximises the likelihood.

sequence.rotation.df$dispersal.spline1 = ifelse(sequence.rotation.df$dispersal > 0.23, sequence.rotation.df$dispersal, 0)


glm.fit.binomial = glm(formula = duration.completion ~ 
                         resistance.cost+
                         dispersal+
                         dispersal.spline1+
                         intervention.coverage+
                         insecticide.resistance.hertiability+
                         male.insecticide.exposure+
                         female.insecticide.exposure+
                         dep.freq+
                         insecticides.in.sim+
                         strategy,
                       method = "glm.fit",
                       family = "binomial",
                       data = sequence.rotation.df)

summary(glm.fit.binomial)
AIC(glm.fit.binomial)
MASS::stepAIC(glm.fit.binomial)

glm.fit.binomial.step = glm(formula = duration.completion ~ 
                              resistance.cost +
                              dispersal + 
                              intervention.coverage + 
                              insecticide.resistance.hertiability + 
                              male.insecticide.exposure + 
                              female.insecticide.exposure + 
                              dep.freq + 
                              insecticides.in.sim + 
                              strategy, family = "binomial", 
                            data = sequence.rotation.df,
                            method = "glm.fit")

summary(glm.fit.binomial.step)


###################################################################################################
#2. Parameter space testing comparing the SEQUENCE IRM strategy versus the ROTATION IRM strategy. #
#                 INCLUDING CROSS SELECTION                                                       #
###################################################################################################

#Cross selction values can be c(-0.3, -0.2, -0.1, 0.1, 0.2, 0.3). #0 cross resistance taken from previous simulations
  #Use only 2 insecticides and 10 generation deployment: as this will allow comparison against mixtures better
    #WHY: Differences between rotation and sequences greatest at this point.
        #Also, issue with cross resistance with higher number of insecticides 

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
df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/lhs_values.csv")

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
###write.csv(sequence.rotation.cross.df, ".//sequence_rotation_cross_df.csv")

#Read in sequence and rotation dataframe from cross selection simulations.
sequence.rotation.cross.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.1/sequence_rotation_cross_df.csv")

#Add the no cross selection dataframe to the cross selection dataframe:
    #sequence.rotation.cross.df
    #seqence.df.zero
    #rotation.df.zero


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
                "male.insecticide.exposure", "female.insecticide.exposure", "resistance.cost", "intervention.coverage", "dispersal", "cross.selection")


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


#Cross Resistance Analysis and Visualisation
ggplot(data = sequence.rotation.cross.full.df, aes(x=simulation.duration,
                                                   fill = strategy)) +
  geom_histogram()+
  facet_grid(cross.resistance~strategy)

sequence.rotation.cross.full.df$duration.completion = sequence.rotation.cross.full.df$simulation.duration / 500


#For each cross.resistance and strategy (rot and seq); calculate total completion rate

filter_cross_df = function(cr.val,
                           strat){
filtered.cross.df = sequence.rotation.cross.full.df%>%
  dplyr::filter(cross.resistance == cr.val)%>%
  dplyr::filter(strategy == strat)%>%
  dplyr::filter(simulation.duration == 500)

prop.completion = nrow(filtered.cross.df)/5000

return(prop.completion)
}

count.max.dur = c(filter_cross_df(-0.3, "sequence"),
                  filter_cross_df(-0.2, "sequence"),
                  filter_cross_df(-0.1, "sequence"),
                  filter_cross_df(0, "sequence"),
                  filter_cross_df(0.1, "sequence"),
                  filter_cross_df(0.2, "sequence"),
                  filter_cross_df(0.3, "sequence"),
                  filter_cross_df(-0.3, "rotation"),
                  filter_cross_df(-0.2, "rotation"),
                  filter_cross_df(-0.1, "rotation"),
                  filter_cross_df(0, "rotation"),
                  filter_cross_df(0.1, "rotation"),
                  filter_cross_df(0.2, "rotation"),
                  filter_cross_df(0.3, "rotation"))

strat.vec = c(rep("sequence", 7), rep("rotation", 7))
cross.vals = rep(seq(-0.3, 0.3, 0.1), 2)

count.max.dur.df = data.frame(count.max.dur,
                              stat.vec,
                              cross.vals)


pals.1 = c("#4d9221", 
           "#c51b7d")

ggplot(data=count.max.dur.df, aes(x=cross.vals, y=count.max.dur,
                                  colour = strat.vec))+
  geom_point(size = 3)+
  geom_line()+
  scale_colour_manual(values = pals.1)+
  xlab("Cross Selection")+
  ylab("Proportion of Simulations Reaching Maximum Duration")+
  theme_bw()
  
ggplot(data=sequence.rotation.cross.full.df, aes(x=cross.selection, y=simulation.duration,
                                  colour = strategy))+
  geom_point(alpha = 0.1)+
  geom_line()+
  scale_colour_manual(values = pals.1)+
  xlab("Cross Selection")+
  ylab("Sim Duration")+
  theme_bw()  
  
  

glm.fit.binomial.cross = glm(formula = duration.completion ~ 
                               resistance.cost+
                               dispersal+
                               intervention.coverage+
                               insecticide.resistance.hertiability+
                               male.insecticide.exposure+
                               female.insecticide.exposure+
                               cross.resistance+
                               strategy,
                             data = sequence.rotation.cross.full.df,
                             method = "glm.fit",
                             family = "binomial")

summary(glm.fit.binomial.cross)







