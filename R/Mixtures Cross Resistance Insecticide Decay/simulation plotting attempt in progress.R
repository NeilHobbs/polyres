# library(gridExtra)
# 
# 
# temp.df = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 4,
#                                                                exposure.scaling.factor = 10,
#                                                                nsim = 1,
#                                                                minimum.insecticide.resistance.hertitability = 0.30,
#                                                                maximum.insecticide.resistance.hertitability = 0.30,
#                                                                minimum.male.insecticide.exposure = 0.9,
#                                                                maximum.male.insecticide.exposure = 0.9,
#                                                                minimum.female.insecticide.exposure = 0.9,
#                                                                maximum.female.insecticide.exposure = 0.9,
#                                                                resistance.cost = 0.1,
#                                                                starting.treatment.site.intensity = 0,
#                                                                starting.refugia.intensity = 0,
#                                                                min.intervention.coverage = 0.9,
#                                                                max.intervention.coverage = 0.9,
#                                                                min.dispersal.rate = 0.5,
#                                                                max.dispersal.rate = 0.5,
#                                                                maximum.generations = 500,
#                                                                irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
#                                                                half.population.bioassay.survival.resistance = 900,
#                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                maximum.resistance.value = 25000 #have arbitrarily high just in case
# 
# ),
#                          number.of.insecticides = 4,
#                          maximum.generations = 500)
# 
# temp.df.treatment = temp.df%>%
#   dplyr::filter(site == "treatment")
# 
# temp.df.refugia = temp.df%>%
#   dplyr::filter(site == "refugia")
# 
# #Turn the following into a function:
#     #with inputs: simulation.dataframe
#     #withdrawal.threshold
#     #return.threshold
#     #irm.strategy
# 
# 
# treatment.plot = ggplot(temp.df.treatment, aes(x=time.in.generations, y = resistance.intensity, colour = insecticide.tracked,))+
#   geom_point(aes(x=time.in.generations, y=100, colour=insecticide.deployed, alpha = 0.3)) +
#   geom_point(aes(x=time.in.generations, y=50, colour=insecticide.deployed, alpha = 0.3)) +
#   geom_line(aes(x=time.in.generations, y=resistance.intensity, colour=insecticide.tracked))+
#   annotate("text", x= 70, y =102.5, label = "Withdrawal")+
#   annotate("text", x=70, y =52.5, label = "Return")+
#   scale_y_continuous(limits = c(0, ifelse(max(temp.df$resistance.intensity) < 120, yes = 120, no = max(temp.df$resistance.intensity) + 10)))+
#   ylab("Insecticide Resistance Intensity") +
#   xlab("Time in Generations") +
#   ggtitle("Treatment Site")+
#   theme_classic()+
#   theme(legend.position = "none")
# 
# refugia.plot = ggplot(temp.df.refugia, aes(x=time.in.generations, y = resistance.intensity))+
#   geom_line(data = temp.df.refugia, mapping = aes(x=time.in.generations, y=resistance.intensity, colour = insecticide.tracked)) +
#   scale_y_continuous(limits = c(0, ifelse(max(temp.df$resistance.intensity) < 120, yes = 120, no = max(temp.df$resistance.intensity) + 10)))+
#   ylab(" ") +
#   xlab("Time in Generations") +
#   ggtitle("Refugia")+
#   theme_classic()+
#   theme(legend.position = "none")
# 
# 
# gridExtra::grid.arrange(treatment.plot, refugia.plot, ncol=2)
# 
