###############################################################################################
#Parameter space testing comparing the SEQUENCE IRM strategy versus the ROTATION IRM strategy.#
###############################################################################################
library(devtools)
load_all() #loads in polyres
library(lhs) #for latin hypercube
library(ppcor) #for parameter space testing
library(ggplot2) #for plotting


#Make latin hypercube for sampling, that allows x simulations total. There are 7 inputs that can be changed
#insecticide.resistance.hertiability
#male.insecticide.exposure
#female.insecticide.exposure
#resistance.cost
#intervention.coverage
#dispersal

df = data.frame(lhs::randomLHS(5000, 7))

#Rename columns; and change distributions to be correct
#for easier tracking of which variable is which.

df = df%>%
  dplyr::rename(insecticide.resistance.hertiability = X1)%>%
  dplyr::rename(male.insecticide.exposure = X2)%>%
  dplyr:: rename(female.insecticide.exposure = X3)%>%
  dplyr::rename(resistance.cost = X4)%>%
  dplyr::rename(intervention.coverage = X5)%>%
  dplyr::rename(dispersal = X6)%>%
  dplyr::rename(cross.selection = X7)%>%
  dplyr::mutate(insecticide.resistance.hertiability = qunif(insecticide.resistance.hertiability, 0.003, 0.97))%>%
  dplyr::mutate(male.insecticide.exposure = qunif(male.insecticide.exposure, 0, 1))%>%
  dplyr::mutate(female.insecticide.exposure = qunif(female.insecticide.exposure, 0.4, 0.9))%>%
  dplyr::mutate(resistance.cost = qunif(resistance.cost, 0.01, 0.2))%>%
  dplyr::mutate(intervention.coverage = qunif(intervention.coverage, 0.1, 0.9))%>%
  dplyr::mutate(dispersal = qunif(dispersal, 0.1, 0.9))

write.csv(df, ".//lhs_values.csv") #only has the randomly selected values


df = read.csv("./lhs_values.csv")

parameter.space = df[,2:7]
parameter.space = parameter.space%>%
  dplyr::dplyr::rename(Heritability = insecticide.resistance.hertiability)%>%
  dplyr::rename(`Male Exposure` = male.insecticide.exposure)%>%
  dplyr::rename(`Female Exposure` = female.insecticide.exposure)%>%
  dplyr::rename(`Intervention Coverage` = intervention.coverage)%>%
  dplyr::rename(`Fitness Cost` = resistance.cost)%>%
  dplyr::rename(Dispersal = dispersal)

#Visually check suitable parameter space coverage - eg no patches with large gaps.
plot(parameter.space)



df = rbind(df, df, df) #3x as 2, 3, 4 insecticides

no.insecticides = c(rep(2, 5000), rep(3, 5000), rep(4, 5000))
df = cbind(df, no.insecticides)

freq.deployment = c(rep(5, 15000), rep(10, 15000), rep(20, 15000))#deployment decisions every 6, 12, 24 months
df=cbind(df, freq.deployment)


temp.list.sequence = list() #create empty list to hold the loop

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
    summarise(mean(resistance.intensity)))

strategy = "sequence"
temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                    exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)

temp.list.sequence[[v]] = temp_2
}

sequence.df = do.call(rbind, temp.list.sequence)
sequence.df = cbind(sequence.df, df)

#What would be the important outputs for comparison:
  # 1. Total Duration of Interventions [how long the simulation was]
  # 2. The number of generations where resistance.intensity > withdrawal threshold
  # 3. Number of generations where insecticide.deployed resistance > withdrawal threshold [how many generations was a low quality intervention in use]
  # 4. Peak resistance intensity
  # 5. average resistance intensity of the deployed insecticide
  # 5.




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

write.csv(sequence.rotation.df, ".//sequence_rotation_df.csv")

sequence_df_2 = sequence.df%>%
  rename("simulation.duration.sequence" = simulation.duration)%>%
  rename("average.resistance.intensity.sequence" = mean.resistance.intensity.)%>%
  rename("exceedance.generations.sequence" = exceedance.generations)%>%
  rename("exceedance.generations.deployed.sequence" = exceedance.generations.deployed)%>%
  rename("peak.resistance.sequence" = peak.resistance)%>%
  dplyr::select(-"strategy")%>%
  mutate(peak.survival.sequence = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                                  mean.population.resistance = peak.resistance.sequence,
                                                                  michaelis.menten.slope = 1, 
                                                                  half.population.bioassay.survival.resistance = 900,
                                                                  sd.population.resistance = 0, 
                                                                  nsim = 1))%>%
  mutate(average.survival.sequence = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                                     mean.population.resistance = average.resistance.intensity.sequence,
                                                                     michaelis.menten.slope = 1, 
                                                                     half.population.bioassay.survival.resistance = 900,
                                                                     sd.population.resistance = 0, 
                                                                     nsim = 1))

replicate = seq(1, 18000, by = 1)
sequence_df_2 = data.frame(sequence_df_2, replicate)

rotation_df_2 = rotation.df%>%
  rename("simulation.duration.rotation" = simulation.duration)%>%
  rename("average.resistance.intensity.rotation" = mean.resistance.intensity.)%>%
  rename("exceedance.generations.rotation" = exceedance.generations)%>%
  rename("exceedance.generations.deployed.rotation" = exceedance.generations.deployed)%>%
  rename("peak.resistance.rotation" = peak.resistance)%>%
  dplyr::select(-"strategy")%>%
  mutate(average.survival.rotation = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                                     mean.population.resistance = average.resistance.intensity.rotation,
                                                                     michaelis.menten.slope = 1, 
                                                                     half.population.bioassay.survival.resistance = 900,
                                                                     sd.population.resistance = 0, 
                                                                     nsim = 1))%>%
  mutate(peak.survival.rotation = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                                    mean.population.resistance = peak.resistance.rotation,
                                                                    michaelis.menten.slope = 1, 
                                                                    half.population.bioassay.survival.resistance = 900,
                                                                    sd.population.resistance = 0, 
                                                                    nsim = 1))

rotation_df_2 = data.frame(rotation_df_2, replicate)

all_sims_join = inner_join(sequence_df_2, rotation_df_2)

all_sims_join = all_sims_join%>%
  mutate(diff.duration = simulation.duration.sequence - simulation.duration.rotation)%>%
  mutate(diff.av.resistance = average.resistance.intensity.sequence - average.resistance.intensity.rotation)%>%
  mutate(diff.exceed.gens = exceedance.generations.sequence - exceedance.generations.rotation)%>%
  mutate(diff.exceed.gens.deployed = exceedance.generations.deployed.sequence - exceedance.generations.deployed.rotation)%>%
  mutate(diff.peak.resistance = peak.resistance.sequence - peak.resistance.rotation)%>%
  mutate(diff.av.survival = average.survival.sequence - average.survival.rotation)%>%
  mutate(diff.peak.survival = peak.survival.sequence - peak.survival.rotation)
           
  


write.csv(all_sims_join, ".//seq_rot_sims.csv")


##########Data Analysis and Data Visualisation###########
library(ggplot2)
library(dplyr)
library(ppcor)

#Has differences between Rot and Seq
all_sims_join = read.csv("./seq_rot_sims.csv")
all_sims_join = all_sims_join%>%
  mutate(percent_diff_duration = (simulation.duration.rotation - simulation.duration.sequence)/simulation.duration.sequence * 100)%>%
  mutate(percent_diff_intensity = (average.resistance.intensity.rotation - average.resistance.intensity.sequence)/average.resistance.intensity.sequence * 100)%>%
  mutate(percent_diff_survival = (average.survival.rotation - average.survival.sequence)/average.survival.sequence * 100)
 


#Has each individual simulation parameters and outcomes
sequence.rotation.df = read.csv("./sequence_rotation_df.csv")

#Where the final simulation duration was a tie
sim_equal_gens = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence == simulation.duration.rotation)

rotation_wins = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence < simulation.duration.rotation)

sequence_wins = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence > simulation.duration.rotation)


#Calculate as percentages (will need changing)
2411/18000*100
15589/18000*100



ggplot(all_sims_join, aes(x=percent_diff_duration))+
  geom_histogram(binwidth = 10)


#Creates a dataset for putting in labels for putting in the 
label.df = data.frame(
  text.label = c("Favours Rotations", "Favours Sequences"),
  label_x_coord = c(18300, 18300), #have the label fairly central.
  label_y_coord = c(40, -40)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq = data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(3000, 9000, 15000),
  label_y_coord = c(100, 100, 100)
)


#Scatterplot of difference in duration (n=2411).
ggplot(rotation_wins, aes(x = replicate, y=percent_diff_duration)) +
  geom_point(aes(colour = as.factor(insecticides.in.sim)),
                 alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
  geom_vline(xintercept = 6000, linetype = "dashed") +
  geom_vline(xintercept = 12000, linetype = "dashed")+
  geom_hline(yintercept = 0, size = 1)+
  xlab("Simulation Replicate")+ 
  ylab("Percentage Difference in Simulation Duration")+ 
  ylim(-100, 100)+#set as the maximum range
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
  text.label = c("Favours Sequences", "Favours Rotations"),
  label_x_coord = c(18300, 18300), #have the label fairly central.
  label_y_coord = c(50, -50)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq1 = data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(3000, 9000, 15000),
  label_y_coord = c(100, 100, 100)
)


#Lollipop plot of the difference average resistance to the deployed insecticide (n=15589)
ggplot(sim_equal_gens, aes(x = replicate, y= percent_diff_intensity))+
  geom_hline(yintercept = 0, size = 2)+
  geom_point(aes(colour=as.factor(insecticides.in.sim)),alpha = 0.1)+
  geom_vline(xintercept = 6000, linetype = "dashed") +
  geom_vline(xintercept = 12000, linetype = "dashed")+
  xlab("Simulation Replicate")+ 
  ylab("Percentage Difference in Average Resistance to Deployed Insecticide")+ 
  ylim(-100, 100)+#set as the maximum range
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
range(sim_equal_gens$diff.peak.resistance)
#Creates a dataset for putting in labels for putting in the 
label.df3 = data.frame(
  text.label = c("Favours Rotations", "Favours Sequences"),
  label_x_coord = c(8000, 8000), #have the label fairly central.
  label_y_coord = c(120, -120)) #Should be far enough away to not be overlapping any bars/points

#Lollipop plot of the difference in the number of exceedance events (deployed)
ggplot(sim_equal_gens, aes(x = replicate, y= diff.peak.resistance)) +
  geom_point(alpha = 0.1)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
  xlab("Simulation Replicate")+ 
  ylab("Difference in the Number of Exceedance Generations to the Deployed Insecticide")+ 
  ylim(-150, 150)+#set as the maximum range
  geom_label(data = label.df3, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord,
                                   fill= text.label))+
  theme_bw()+
  theme(legend.position = "none")

#generate mean and 95%CI for each system:



mean_95_CI = function(dataset, irm.strategy, insecticides, freq.dep,
                      outcome){
  
  temp.df = dataset%>%
    dplyr::filter(strategy == irm.strategy)%>%
    dplyr::filter(insecticides.in.sim == insecticides)%>%
    dplyr::filter(dep.freq == freq.dep)%>%
    dplyr::select(outcome)
  
  temp.df = temp.df[, 1]
  
  mean.outcome = mean(temp.df)
  sd.outcome = sd(temp.df)
  se.outcome = sd.outcome/sqrt(length(temp.df))
  upperCI = mean.outcome + (1.96*se.outcome)
  lowerCI = mean.outcome - (1.96*se.outcome)
  
  A = data.frame(mean.outcome, upperCI, lowerCI)
  
  return(A)
}

strat.vec = c(rep("sequence", times = 9), rep("rotation", times = 9))
insecticide.vec = rep(rep(c(2, 3, 4), times = 3), times = 2)
freq.vec = rep(c(5, 5, 5, 10, 10, 10, 20, 20, 20), times= 2)
             

#Loop for simulation duration
sim.duration.list = list()
for(i in 1:length(strat.vec)){
  sim.duration.list[[i]] = mean_95_CI(sequence.rotation.df, strat.vec[i],
                                 insecticide.vec[i], freq.vec[i],
                                  "simulation.duration") 
}


mean_CI_Duration = do.call(rbind, sim.duration.list)
mean_CI_Duration$strategy = strat.vec
mean_CI_Duration$freq = freq.vec
mean_CI_Duration$number.insecticide = insecticide.vec
mean_CI_Duration$comparison = rep(seq(1, 9, by =1), times = 2)


ggplot(mean_CI_Duration, aes(x=strategy, y = mean.outcome))+
  geom_point(aes(colour = strategy)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI,
                    colour = as.factor(strategy))) +
  xlab("Insecticide Resistance Management Strategy")+
  ylab("Mean Simulation Duration")+
  theme_bw()+
  theme(legend.position = "none")+
  facet_grid(number.insecticide~freq)

colnames(sequence.rotation.df)


#Loop for mean resistance intensity
mean.resistance.intensity.list = list()
for(i in 1:length(strat.vec)){
  mean.resistance.intensity.list[[i]] = mean_95_CI(sequence.rotation.df, strat.vec[i],
                               insecticide.vec[i], freq.vec[i],
                               "mean.resistance.intensity.") 
}


mean_CI_Intensity = do.call(rbind, mean.resistance.intensity.list)
mean_CI_Intensity$strategy = strat.vec
mean_CI_Intensity$freq = freq.vec
mean_CI_Intensity$number.insecticide = insecticide.vec
mean_CI_Intensity$comparison = rep(seq(1, 9, by =1), times = 2)


ggplot(mean_CI_Intensity, aes(x=strategy, y = mean.outcome))+
  geom_point(aes(colour = strategy)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI,
                    colour = as.factor(strategy))) +
  facet_grid(number.insecticide~freq)+
  xlab("Insecticide Resistance Management Strategy")+
  ylab("Mean Resistance Intensity to Deployed Insecticide")+
  theme_bw()+
  theme(legend.position = "none")


#Exceedance Generations Deployed
exceedance.deployed.list = list()
for(i in 1:length(strat.vec)){
  exceedance.deployed.list[[i]] = mean_95_CI(sequence.rotation.df, strat.vec[i],
                                                   insecticide.vec[i], freq.vec[i],
                                                   "exceedance.generations.deployed") 
}


mean_CI_Exceedance = do.call(rbind, exceedance.deployed.list)
mean_CI_Exceedance$strategy = strat.vec
mean_CI_Exceedance$freq = freq.vec
mean_CI_Exceedance$number.insecticide = insecticide.vec
mean_CI_Exceedance$comparison = rep(seq(1, 9, by =1), times = 2)


ggplot(mean_CI_Exceedance, aes(x=strategy, y = mean.outcome))+
  geom_point(aes(colour = strategy)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI,
                    colour = as.factor(strategy))) +
  facet_grid(number.insecticide~freq)+
  xlab("Insecticide Resistance Management Strategy")+
  ylab("Mean Number of Generations Deployed Above Withdrawal Threshold")+
  theme_bw()+
  theme(legend.position = "none")


#Peak Resistance Intensity
peak.list = list()
for(i in 1:length(strat.vec)){
  peak.list[[i]] = mean_95_CI(sequence.rotation.df, strat.vec[i],
                                             insecticide.vec[i], freq.vec[i],
                                             "peak.resistance") 
}


mean_CI_Peak = do.call(rbind, peak.list)
mean_CI_Peak$strategy = strat.vec
mean_CI_Peak$freq = freq.vec
mean_CI_Peak$number.insecticide = insecticide.vec
mean_CI_Peak$comparison = rep(seq(1, 9, by =1), times = 2)


ggplot(mean_CI_Peak, aes(x=strategy, y = mean.outcome))+
  geom_point(aes(colour = strategy)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI,
                    colour = as.factor(strategy))) +
  facet_grid(number.insecticide~freq)+
  xlab("Insecticide Resistance Management Strategy")+
  ylab("Peak Resistance Intensity")+
  theme_bw()+
  theme(legend.position = "none")

#partial correlation needs each endpoint as the final column. 
#As number of insecticides and deployment frequency not random need to do a 
  #separate pcor on each combination.

pcor.rotation.df = sequence.rotation.df%>%
  dplyr::filter(strategy == "rotation")%>%
  dplyr::select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
         "intervention.coverage", "dispersal", "female.insecticide.exposure", "mean.resistance.intensity.")

pcor.sequence.df = sequence.rotation.df%>%
  dplyr::filter(strategy == "sequence")%>%
  dplyr::select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
                "intervention.coverage", "dispersal", "female.insecticide.exposure", "mean.resistance.intensity.")


pcor.output.rotation = pcor(pcor.rotation.df, method = "pearson")
pcor.output.sequence = pcor(pcor.sequence.df, method = "pearson")

parameter = c("Insecticide Resistance Heritability", "Resistance Cost", "Male Insecticide Exposure",
              "Intervention Coverage", "Mosquito Dispersal", "Female Insecticide Exposure", NA)

estimate.rot = pcor.output.rotation$estimate[7,]
pvalue.rot = pcor.output.rotation$p.value[7,]
teststat.rot= pcor.output.rotation$statistic[7,]

estimate.seq = pcor.output.sequence$estimate[7,]
pvalue.seq = pcor.output.sequence$p.value[7,]
teststat.seq= pcor.output.sequence$statistic[7,]

rot_pcor_df = data.frame(estimate.rot, pvalue.rot, teststat.rot, parameter)
seq_pcor_df = data.frame(estimate.seq, pvalue.seq, teststat.seq, parameter)
rot_pcor_df = rot_pcor_df[1:6,]
seq_pcor_df = seq_pcor_df[1:6,]

#Join rot_pcor_df and seq_pcor_df
rot_pcor_df = rot_pcor_df%>%
  rename(estimate = estimate.rot)%>%
  rename(pvalue = pvalue.rot)%>%
  rename(teststat = teststat.rot)%>%
  mutate(`IRM Strategy` = "rotation")

seq_pcor_df = seq_pcor_df%>%
  rename(estimate = estimate.seq)%>%
  rename(pvalue = pvalue.seq)%>%
  rename(teststat = teststat.seq)%>%
  mutate(`IRM Strategy` = "sequence")

pcor_df = rbind(rot_pcor_df, seq_pcor_df)

ggplot(pcor_df, aes(x=estimate, y=parameter, fill = `IRM Strategy`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  xlim(-1, 1)+
  ylab("Parameter") +
  xlab("Correlation") +
  theme_classic()




sample1 = dplyr::sample_n(df, 1, replace=TRUE)
sample.insecticides = sample(c(2, 3, 4), 1, replace = TRUE)
sample.frequency = sample(c(5, 10, 20), 1, replace = TRUE)

example.dataframe = data.frame(sample1, sample.insecticides, sample.frequency)

write.csv(example.dataframe, ".//random_parameter_sample.csv")


seq_example = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = sample.insecticides,
                                                     exposure.scaling.factor = 10,
                                                     nsim = 1,
                                                     minimum.insecticide.resistance.heritability = sample1$insecticide.resistance.hertiability,
                                                     maximum.insecticide.resistance.heritability = sample1$insecticide.resistance.hertiability,
                                                     minimum.male.insecticide.exposure = sample1$male.insecticide.exposure,
                                                     maximum.male.insecticide.exposure = sample1$male.insecticide.exposure,
                                                     minimum.female.insecticide.exposure = sample1$female.insecticide.exposure,
                                                     maximum.female.insecticide.exposure = sample1$female.insecticide.exposure,
                                                     resistance.cost = sample1$resistance.cost,
                                                     starting.treatment.site.intensity = 0,
                                                     starting.refugia.intensity = 0,
                                                     min.intervention.coverage = sample1$intervention.coverage,
                                                     max.intervention.coverage = sample1$intervention.coverage,
                                                     min.dispersal.rate = sample1$dispersal,
                                                     max.dispersal.rate = sample1$dispersal,
                                                     maximum.generations = 500, #appoximately 50 years
                                                     irm.strategy = "sequence", 
                                                     half.population.bioassay.survival.resistance = 900, 
                                                     withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                     return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                     deployment.frequency = sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                     maximum.resistance.value = 25000 #have arbitrarily high just in case
) , 500, 3)

rot_example = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = sample.insecticides,
                                                                   exposure.scaling.factor = 10,
                                                                   nsim = 1,
                                                                   minimum.insecticide.resistance.heritability = sample1$insecticide.resistance.hertiability,
                                                                   maximum.insecticide.resistance.heritability = sample1$insecticide.resistance.hertiability,
                                                                   minimum.male.insecticide.exposure = sample1$male.insecticide.exposure,
                                                                   maximum.male.insecticide.exposure = sample1$male.insecticide.exposure,
                                                                   minimum.female.insecticide.exposure = sample1$female.insecticide.exposure,
                                                                   maximum.female.insecticide.exposure = sample1$female.insecticide.exposure,
                                                                   resistance.cost = sample1$resistance.cost,
                                                                   starting.treatment.site.intensity = 0,
                                                                   starting.refugia.intensity = 0,
                                                                   min.intervention.coverage = sample1$intervention.coverage,
                                                                   max.intervention.coverage = sample1$intervention.coverage,
                                                                   min.dispersal.rate = sample1$dispersal,
                                                                   max.dispersal.rate = sample1$dispersal,
                                                                   maximum.generations = 500, #appoximately 50 years
                                                                   irm.strategy = "rotation", 
                                                                   half.population.bioassay.survival.resistance = 900, 
                                                                   withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                   return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                   deployment.frequency = sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                   maximum.resistance.value = 25000 #have arbitrarily high just in case
) , 500, 3)


plot_simulation = function(simulation.dataframe){
  
  #Make as separate dataframes: 1 for treatment, 1 for refugia  
  temp.df.treatment = simulation.dataframe%>%
    dplyr::filter(site == "treatment")
  
  temp.df.refugia = simulation.dataframe%>%
    dplyr::filter(site == "refugia")

  withdraw.intensity = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1, #must be set to 1 to work properly
                                                       michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                       half.population.bioassay.survival.resistance = 900, 
                                                       bioassay.survival = 0.1, 
                                                       estimate.precision = 0.0001, 
                                                       sd.population.resistance = 0,
                                                       nsim = 1000,
                                                       minimum.resistance.value = 0, 
                                                       maximum.resistance.value = 25000)
  
  return.intensity = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1, #must be set to 1 to work properly
                                                     michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                     half.population.bioassay.survival.resistance = 900, 
                                                     bioassay.survival = 0.05, 
                                                     estimate.precision = 0.0001, 
                                                     sd.population.resistance = 0,
                                                     nsim = 1000,
                                                     minimum.resistance.value = 0, 
                                                     maximum.resistance.value = 25000)
  
  #Create a plot for the treatment site: this will have the deployment sequence and the threshold lines.
  treatment.plot = ggplot(temp.df.treatment, aes(x=time.in.generations, y = resistance.intensity, colour = insecticide.tracked,))+
    geom_point(aes(x=time.in.generations, y=withdraw.intensity, colour=insecticide.deployed, alpha = 0.3)) + #This needs to be an input: calculated from bioassay_survival_to_resistance()
    geom_point(aes(x=time.in.generations, y=return.intensity), colour="lightgrey", alpha = 0.3) + #This needs to be an input: calculated from bioassay_survival_to_resistance()
    geom_line(aes(x=time.in.generations, y=resistance.intensity, colour=insecticide.tracked))+
    scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$resistance.intensity) < withdraw.intensity, yes = withdraw.intensity, no = max(simulation.dataframe$resistance.intensity) + 10)))+
    ylab("Insecticide Resistance Intensity") +
    xlab("Time in Generations") +
    ggtitle("Treatment Site")+
    theme_classic()+
    theme(legend.position = "none")
  
  #Create a plot for the refugia. Does not have deployment/threshold lines.
  refugia.plot = ggplot(temp.df.refugia, aes(x=time.in.generations, y = resistance.intensity))+
    geom_line(data = temp.df.refugia, mapping = aes(x=time.in.generations, y=resistance.intensity, colour = insecticide.tracked)) +
    scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$resistance.intensity) < withdraw.intensity, yes = withdraw.intensity, no = max(simulation.dataframe$resistance.intensity) + 10)))+
    ylab(" ") +
    xlab("Time in Generations") +
    ggtitle("Refugia")+
    theme_classic()+
    theme(legend.position = "none")
  
  
  return(gridExtra::grid.arrange(treatment.plot, refugia.plot, ncol=2))
}

rot_example_plot = plot_simulation(rot_example)
seq_example_plot = plot_simulation(seq_example)


#Diff is reverse poisson: Do 500 - sim duration.
#Mean resisance is broadly poisson
#Exceedance generations deployed is poisson


library(MASS)
library(AER)

#Negative binomial glm on simulation duration.

sequence.rotation.df = sequence.rotation.df%>%
  mutate(dep.freq = as.factor(dep.freq))%>%
  mutate(insecticides.in.sim = as.factor(insecticides.in.sim))

seq_rot_nbglm = glm.nb(formula = simulation.duration ~ 
                                  strategy+
                                  dep.freq +
                                  male.insecticide.exposure+
                                  resistance.cost+
                                  dispersal+
                                  insecticides.in.sim+
                                  insecticide.resistance.hertiability+
                                  female.insecticide.exposure,
                         method = "glm.fit", 
                      data = sequence.rotation.df)

summary(seq_rot_nbglm)
stepAIC(seq_rot_nbglm)
dispersiontest(seq_rot_poisson)


seq_rot_nbglm_2 = glm.nb(formula = simulation.duration ~ 
                         strategy,
                       method = "glm.fit", 
                       data = sequence.rotation.df)
summary(seq_rot_nbglm_2)
stepAIC(seq_rot_nbglm_2)
