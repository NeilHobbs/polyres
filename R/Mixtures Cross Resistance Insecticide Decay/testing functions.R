# test_simulation = run_simulation_intervention(number.of.insecticides = 3,
#                        exposure.scaling.factor = 10,
#                        nsim = 1,
#                         minimum.insecticide.resistance.hertitability = 0.3,
#                                maximum.insecticide.resistance.hertitability = 0.30,
#                                minimum.male.insecticide.exposure = 0.9,
#                                maximum.male.insecticide.exposure = 0.9,
#                                minimum.female.insecticide.exposure = 0.9,
#                                maximum.female.insecticide.exposure = 0.9,
#                                resistance.cost = 0.2,
#                                starting.treatment.site.intensity = 0,
#                                starting.refugia.intensity = 0,
#                                min.intervention.coverage = 0.8,
#                                max.intervention.coverage = 0.8,
#                                min.dispersal.rate = 0.9,
#                                max.dispersal.rate = 0.9,
#                                maximum.generations = 1000,
#                                irm.strategy = "rotation", #will be "sequence" or "rotation" (plus mixture later on),
#                                half.population.bioassay.survival.resistance = 900,
#                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                return.threshold.value = 0.1, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                maximum.resistance.value = 25000 #have arbitrarily high just in case
# 
# )
# 
# 
# test2 = get_simulation_dataframe(test_simulation, 1000, 3)
# 
# ggplot(test2, aes(x=time.in.generations, y=resistance.intensity, color = insecticide.tracked)) +
#          geom_line() +
#         facet_wrap(~site)
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
# 
# 
# 
