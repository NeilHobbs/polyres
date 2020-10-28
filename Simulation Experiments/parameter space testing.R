# library(lhs)
# library(ppcor)
# library(dplyr)
# library(ggplot2)
# #Make latin hypercube for sampling, that allows 2000 simulations total
# df = data.frame(randomLHS(5, 6))
# 
# #Rename columns; and change distributions to be correct
# #for easier tracking of which variable is which.
# #run and extract value for treatment and refugia at 100, 300, 500 gens
# #uses the run_simulation_endpoint function in file called "modified runs" in the statsanalysis folder
# 
# df = df%>%
#   rename(insecticide.resistance.hertiability = X1)%>%
#   rename(male.insecticide.exposure = X2)%>%
#   rename(female.insecticide.exposure = X3)%>%
#   rename(resistance.cost = X4)%>%
#   rename(intervention.coverage = X5)%>%
#   rename(dispersal = X6)%>%
#   mutate(insecticide.resistance.hertiability = qunif(insecticide.resistance.hertiability, 0.05, 0.3))%>%
#   mutate(male.insecticide.exposure = qunif(male.insecticide.exposure, 0, 1))%>%
#   mutate(female.insecticide.exposure = qunif(female.insecticide.exposure, 0.4, 0.9))%>%
#   mutate(resistance.cost = qunif(resistance.cost, 0.01, 0.2))%>%
#   mutate(intervention.coverage = qunif(intervention.coverage, 0.1, 0.9))%>%
#   mutate(dispersal = qunif(dispersal, 0.1, 0.9))
# 
# 
# temp.list = list()
# for(v in 1: nrow(df)){
# 
#     temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 3,
#                                                        exposure.scaling.factor = 10,
#                                                        nsim = 1,
#                                                        minimum.insecticide.resistance.hertitability = df$insecticide.resistance.hertiability[v],
#                                                        maximum.insecticide.resistance.hertitability = df$insecticide.resistance.hertiability[v],
#                                                        minimum.male.insecticide.exposure = df$male.insecticide.exposure[v],
#                                                        maximum.male.insecticide.exposure = df$male.insecticide.exposure[v],
#                                                        minimum.female.insecticide.exposure = df$female.insecticide.exposure[v],
#                                                        maximum.female.insecticide.exposure = df$female.insecticide.exposure[v],
#                                                        resistance.cost = df$resistance.cost[v],
#                                                        starting.treatment.site.intensity = 0,
#                                                        starting.refugia.intensity = 0,
#                                                        min.intervention.coverage = df$intervention.coverage[v],
#                                                        max.intervention.coverage = df$intervention.coverage[v],
#                                                        min.dispersal.rate = df$dispersal[v],
#                                                        max.dispersal.rate = df$dispersal[v],
#                                                        maximum.generations = 500,
#                                                        irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
#                                                        half.population.bioassay.survival.resistance = 900,
#                                                        withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                        return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                        deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                        maximum.resistance.value = 25000 #have arbitrarily high just in case
#   ) , 500, 3)
# 
# 
#   temp$replicate = v
#   temp.list[[v]] = temp
# 
# }
# 
# final_df = do.call(rbind, temp.list)
# 
# 
# final_temp = final_df%>%
#   filter()
# 
# 
# 
# #Important outputs for comparison:
#   # 1. Total Duration of Interventions
#   # 2. Number of generations where insecticide.deployed resistance > withdrawal threshold
#   # 3. Peak resistance intensity
#   # 4. average resistance intensity
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
