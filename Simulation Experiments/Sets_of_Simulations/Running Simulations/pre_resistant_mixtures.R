#Create the input parameters for the simulations::::
library(devtools)
load_all()

start.resistance.old = c(rep(0, 5000),
                         rep(18, 5000),
                         rep(47, 5000),
                         rep(100, 5000),
                         rep(225, 5000),
                         rep(900, 5000),
                         rep(3600, 5000),
                         rep(8100, 5000))

parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space.df = parameter.space[1:5000,2:7]


#ID1	Novel AI full dose alone
temp.ID1= c()
for(v in 1:nrow(parameter.space.df)){
  
  temp.ID1[v] = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 1,
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
                                                                                   deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                   maximum.resistance.value = 80000) , 500, 1)%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::summarise(max(time.in.generations))
  
  if(v %% 10 == 0){print(v)}
  
}

ID1.Duration = c(unlist(temp.ID1))

parameter.space.df$ID1.Duration = ID1.Duration


write.csv(parameter.space.df, ".//one.insecticide.csv")


#ID2	Mixture full dose Novel AI with full dose “old” AI. 
  #Second insecticide is the one which there is already resistance to.
  #First insecticide is that which deployment decisions are made.



##
 #Now do as mixture
parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[1:5000,2:7]

parameter.space.df = rbind(parameter.space, parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space, parameter.space)

start.resistance.old = c(rep(0, 5000),
                         rep(18, 5000),
                         rep(47, 5000),
                         rep(100, 5000),
                         rep(225, 5000),
                         rep(900, 5000),
                         rep(3600, 5000),
                         rep(8100, 5000))

parameter.space.df$start.resistance.old = start.resistance.old

temp.ID2= c()
for(v in 1:nrow(parameter.space.df)){
  
  #Special cases function used, but all the special cases are essentially switched off so acts exactly as the run_simulation_intervention_mixtures()
    #Used because this function has the facility to make decisions only on insecticide 1, which is what we are looking at.
  
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
                                                                                                     deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                                     maximum.resistance.value = 80000, #have arbitrarily high just in case
                                                                                                     min.cross.selection = 0,
                                                                                                     max.cross.selection = 0,
                                                                                                     applied.insecticide.dose = 1, #deployment is a ratio.
                                                                                                     recommended.insecticide.dose = 1,
                                                                                                     threshold.generation = 0,
                                                                                                     base.efficacy.decay.rate = 0, 
                                                                                                     rapid.decay.rate = 0, 
                                                                                                     intercept = 0.15,
                                                                                                     conversion.factor = 0.48,
                                                                                                     insecticide.suppression = FALSE
  ) , 500, 2)%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::summarise(max(time.in.generations))
  
  if(v %% 10 == 0){print(v)}
  
}

ID2.Duration = c(unlist(temp.ID2))

parameter.space.df$ID2.Duration = ID2.Duration

write.csv(parameter.space.df, ".//pre.resistant.mixture.csv")

#####################

pre.resistant.mixture = read.csv("pre.resistant.mixture.csv")
one.insecticide = read.csv("one.insecticide.csv")

duration.1 = one.insecticide$ID1.Duration
duration.1 = rep(duration.1, times = 8)

pre.resistant.mixture$duration.1 = duration.1
pre.resistant.mixture$replicate = rep(seq(1, 5000, 1), 8)
pre.resistant.mixture$start.bioassay.old = c(rep(0, 5000), rep(2, 5000),
                                             rep(5, 5000), rep(10, 5000), rep(20, 5000),
                                             rep(50, 5000), rep(80, 5000), rep(90, 5000))

pre.resistant.mixture$percent.change = ((pre.resistant.mixture$ID2.Duration - pre.resistant.mixture$duration.1)/pre.resistant.mixture$ID2.Duration)*100
pre.resistant.mixture$change = pre.resistant.mixture$ID2.Duration - pre.resistant.mixture$duration.1

pre.resistant.mixture.subset = pre.resistant.mixture%>%
  dplyr::filter(percent.change != 0)
 

pre.resistant.mixture$change.category = ifelse(pre.resistant.mixture$change == 0,
                                               yes = "no change",
                                               no = ifelse(pre.resistant.mixture$change > 0,
                                                           yes = "favours mixture",
                                                           no = "favours solo")) 

table(pre.resistant.mixture.subset$start.resistance.old)

ggplot(pre.resistant.mixture, aes(x=change/10),
       fill = change.category)+
  geom_histogram(binwidth = 1, aes(fill = change.category),
                 colour = "black")+
  scale_fill_manual(values = c("#8e0152", "#276419", "#999999"))+
  facet_wrap(~start.bioassay.old)+
  ylab("Frequency")+
  xlab("Change in Opertional Lifespan (years)")+
  theme_classic()


##Parameter Sensitivity - regression coefficient and intercept::

# field.surival.parameter.space = data.frame(lhs::randomLHS(1000, 2))
# 
# field.surival.parameter.space = field.surival.parameter.space%>%
#   dplyr::rename("intercept" = X1)%>%
#   dplyr::rename("coefficient" = X2)%>%
#   dplyr::mutate("intercept" = qunif(intercept, 0.0000001, 0.3649152))%>%
#   dplyr::mutate("coefficient" = qunif(coefficient, 0.17376870, 0.7949830))
# 
# plot(field.surival.parameter.space)
# 
# #Regression Intercept must be above 0 in order for the model to take off.
# 
# write.csv(field.surival.parameter.space, ".//field.surival.parameter.space.csv")

field.surival.parameter.space = read.csv("field.surival.parameter.space.csv")


#Find which paramter sets to re-run:
min(pre.resistant.mixture$change) #-380
max(pre.resistant.mixture$change) #440

pre.resistant.mixture.max = pre.resistant.mixture%>%
  dplyr::filter(change == 440)

unique(pre.resistant.mixture.max$replicate)

pre.resistant.mixture.min = pre.resistant.mixture%>%
  dplyr::filter(change <= -370)

unique(pre.resistant.mixture.min$replicate)

#Run with the parameter sets where mixtures were:
  #most best (top 3)
  #most worst (bottom 3)

parameter.set = rbind(pre.resistant.mixture.max[1:3, 2:7], 
                      pre.resistant.mixture.min[1:3, 2:7])


parameter.set.1 = do.call("rbind", replicate(8000, parameter.set[1, 1:6], simplify = FALSE))
parameter.set.2 = do.call("rbind", replicate(8000, parameter.set[2, 1:6], simplify = FALSE))
parameter.set.3 = do.call("rbind", replicate(8000, parameter.set[3, 1:6], simplify = FALSE))
parameter.set.4 = do.call("rbind", replicate(8000, parameter.set[4, 1:6], simplify = FALSE))
parameter.set.5 = do.call("rbind", replicate(8000, parameter.set[5, 1:6], simplify = FALSE))
parameter.set.6 = do.call("rbind", replicate(8000, parameter.set[6, 1:6], simplify = FALSE))


field.surival.parameter.space.full = rbind(field.surival.parameter.space,
                                           field.surival.parameter.space,
                                           field.surival.parameter.space,
                                           field.surival.parameter.space,
                                           field.surival.parameter.space,
                                           field.surival.parameter.space)
parameter.sets = rbind(parameter.set.1,
                       parameter.set.2,
                       parameter.set.3,
                       parameter.set.4,
                       parameter.set.5,
                       parameter.set.6)

start.resistance.old = rep(c(rep(0, 1000),
                         rep(18, 1000),
                         rep(47, 1000),
                         rep(100, 1000),
                         rep(225, 1000),
                         rep(900, 1000),
                         rep(3600, 1000),
                         rep(8100, 1000)), 6)


sensivity.space.df = data.frame(parameter.sets, field.surival.parameter.space.full, start.resistance.old)

temp.ID3= c()
for(v in 1:nrow(sensivity.space.df)){
  
  #Special cases function used, but all the special cases are essentially switched off so acts exactly as the run_simulation_intervention_mixtures()
  #Used because this function has the facility to make decisions only on insecticide 1, which is what we are looking at.
  
  temp.ID3[v] = get_simulation_dataframe_mixtures(run_simulation_intervention_mixtures_special_cases(number.of.insecticides = 2,
                                                                                                     exposure.scaling.factor = 10,
                                                                                                     nsim = 1,
                                                                                                     insecticide.resistance.heritability = sensivity.space.df$Heritability[v],
                                                                                                     minimum.male.insecticide.exposure = sensivity.space.df$Male.Insecticide.Exposure[v],
                                                                                                     maximum.male.insecticide.exposure = sensivity.space.df$Male.Insecticide.Exposure[v],
                                                                                                     minimum.female.insecticide.exposure = sensivity.space.df$Female.Insecticide.Exposure[v],
                                                                                                     maximum.female.insecticide.exposure = sensivity.space.df$Female.Insecticide.Exposure[v],
                                                                                                     resistance.cost = sensivity.space.df$Fitness.Cost[v],
                                                                                                     starting.treatment.site.intensity = c(0, sensivity.space.df$start.resistance.old[v]),
                                                                                                     starting.refugia.intensity = c(0, sensivity.space.df$start.resistance.old[v]),
                                                                                                     min.intervention.coverage = sensivity.space.df$Intervention.Coverage[v],
                                                                                                     max.intervention.coverage = sensivity.space.df$Intervention.Coverage[v],
                                                                                                     min.dispersal.rate = sensivity.space.df$Dispersal[v],
                                                                                                     max.dispersal.rate = sensivity.space.df$Dispersal[v],
                                                                                                     maximum.generations = 500, #appoximately 50 years
                                                                                                     irm.deployment.strategy = "mixtures", 
                                                                                                     mixture.strategy = "mix.sequential.discrete", 
                                                                                                     irm.switch.strategy = "insecticide.1", 
                                                                                                     half.population.bioassay.survival.resistance = 900, 
                                                                                                     withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                                     return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                                     deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                                     maximum.resistance.value = 80000, #have arbitrarily high just in case
                                                                                                     min.cross.selection = 0,
                                                                                                     max.cross.selection = 0,
                                                                                                     applied.insecticide.dose = 1, #deployment is a ratio.
                                                                                                     recommended.insecticide.dose = 1,
                                                                                                     threshold.generation = 0,
                                                                                                     base.efficacy.decay.rate = 0, 
                                                                                                     rapid.decay.rate = 0, 
                                                                                                     intercept = sensivity.space.df$intercept[v],
                                                                                                     conversion.factor = sensivity.space.df$coefficient[v],
                                                                                                     insecticide.suppression = FALSE
  ) , 500, 2)%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::summarise(max(time.in.generations))
  
  if(v %% 10 == 0){print(v)}
  
}

ID3.Duration = c(unlist(temp.ID3))

sensivity.space.df$ID3.Duration = ID3.Duration

write.csv(sensivity.space.df, ".//pre.resistant.mixture.sensivity.space.df.csv")

