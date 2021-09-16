#Assessing the impact of the relationship between bioassay survival and field survival.

#The simulations have been using as the regression coefficient 0.48, and intercept 0.15 as these were
 #the point estimates of the linear model between field survival and bioassay survival. However these estimates
 #also have a parameter space around them (e.g. from the 95% CI); and due to the importance of these parameters
 #in the model for mixtures, it is important to validate how variation in the regression coefficient and intercept
 #impact the outcome of mixtures. 
library(devtools)
load_all()

library(mgcv)


sim.output.df =read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/set_17_mixtures.csv")

###Get equivalent for rotations and sequences:: (duration should be same as no fitness cost)

seq.df = max(get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 2,
                                                              exposure.scaling.factor = 10,
                                                              nsim = 1,
                                                              minimum.insecticide.resistance.heritability = 0.3,
                                                              maximum.insecticide.resistance.heritability = 0.3,
                                                              minimum.male.insecticide.exposure = 1,
                                                              maximum.male.insecticide.exposure = 1,
                                                              minimum.female.insecticide.exposure = 1,
                                                              maximum.female.insecticide.exposure = 1,
                                                              resistance.cost = 0,
                                                              starting.treatment.site.intensity = 0,
                                                              starting.refugia.intensity = 0,
                                                              min.intervention.coverage = 1,
                                                              max.intervention.coverage = 1,
                                                              min.dispersal.rate = 0,
                                                              max.dispersal.rate = 0,
                                                              maximum.generations = 500,
                                                              irm.strategy = "sequence", 
                                                              half.population.bioassay.survival.resistance = 900,
                                                              withdrawal.threshold.value = 0.1, 
                                                              return.threshold.value = 0.05, 
                                                              deployment.frequency = 2, 
                                                              maximum.resistance.value = 25000),
                                  maximum.generations = 500, number.of.insecticides = 2)$time.in.generations)

rot.df = max(get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 2,
                                                              exposure.scaling.factor = 10,
                                                              nsim = 1,
                                                              minimum.insecticide.resistance.heritability = 0.3,
                                                              maximum.insecticide.resistance.heritability = 0.3,
                                                              minimum.male.insecticide.exposure = 1,
                                                              maximum.male.insecticide.exposure = 1,
                                                              minimum.female.insecticide.exposure = 1,
                                                              maximum.female.insecticide.exposure = 1,
                                                              resistance.cost = 0,
                                                              starting.treatment.site.intensity = 0,
                                                              starting.refugia.intensity = 0,
                                                              min.intervention.coverage = 1,
                                                              max.intervention.coverage = 1,
                                                              min.dispersal.rate = 0,
                                                              max.dispersal.rate = 0,
                                                              maximum.generations = 500,
                                                              irm.strategy = "rotation", 
                                                              half.population.bioassay.survival.resistance = 900,
                                                              withdrawal.threshold.value = 0.1, 
                                                              return.threshold.value = 0.05, 
                                                              deployment.frequency = 2, 
                                                              maximum.resistance.value = 25000),
                                  maximum.generations = 500, number.of.insecticides = 2)$time.in.generations)



#If intercept<0 then simulatons don't take off. 
range(sim.output.df$simulation.duration)

sim.output.df.500 = sim.output.df%>%
  dplyr::filter(simulation.duration == 500)



sim.output.df.less0 = sim.output.df%>%
  dplyr::filter(regresssion.intercept < 0)

range(sim.output.df.less0$peak.resistance)



range(sim.output.df.500$regresssion.intercept)
range(sim.output.df.500$regression.coefficient)

sim.output.df.greater0 = sim.output.df%>%
  dplyr::filter(regresssion.intercept > 0)

ggplot(sim.output.df, aes(x=regresssion.intercept,
                              y = simulation.duration))+
  geom_point()+
  geom_vline(xintercept =0.15,
             linetype = "dotted",
             size = 2,
             colour = "red")+
  geom_hline(yintercept = 70,
             linetype = "dashed",
             size = 2,
             colour = "blue")+
  xlab("Value of intercept used in the model")+
  ylab("Duration of Simulation")+
  theme_classic()


ggplot(sim.output.df.greater0, aes(x=regression.coefficient,
                          y = simulation.duration))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept =0.48,
             linetype = "dotted",
             size = 2,
             colour = "red")+
  geom_hline(yintercept = 70,
             linetype = "dashed",
             size = 2,
             colour = "blue")+
  xlab("Value of regression coefficient used in the model")+
  ylab("Duration of Simulation")+
  theme_classic()


intercept.gam = mgcv::gam(cbind(simulation.duration, 500)~
                             s(regresssion.intercept)+
                             regression.coefficient,
                          family = "binomial",
                          data = sim.output.df)
summary(intercept.gam)

plot(intercept.gam)

coefficent.gam = mgcv::gam(cbind(simulation.duration, 500)~
                            regresssion.intercept+
                            s(regression.coefficient),
                          family = "binomial",
                          data = sim.output.df)

plot(coefficent.gam)

sim.output.df.1 = sim.output.df%>%
  dplyr::filter(regresssion.intercept > 0)

min(sim.output.df.1$regresssion.intercept)

#intercept needs to be over 0.03550763 to take off

A = ggplot(sim.output.df.1, aes(x=regresssion.intercept,
                          y = simulation.duration))+
  geom_point(alpha = 0.5)+
  geom_vline(xintercept =0.15,
             size = 2,
             alpha = 0.5,
             colour = "red")+
  geom_hline(yintercept = 70,
             linetype = "dashed",
             size = 2,
             colour = "blue")+
  ggtitle("A")+
  xlab("Value of intercept used in the model")+
  ylab("Duration of Simulation")+
  theme_classic()


B = ggplot(sim.output.df.1, aes(x=regression.coefficient,
                          y = simulation.duration))+
  geom_point(alpha = 0.5)+
  geom_vline(xintercept =0.48,
             size = 2,
             alpha = 0.5,
             colour = "red")+
  geom_hline(yintercept = 70,
             linetype = "dashed",
             size = 2,
             colour = "blue")+
  ggtitle("B")+
  xlab("Value of regression coefficient used in the model")+
  ylab("Duration of Simulation")+
  theme_classic()

gridExtra::grid.arrange(A, B, nrow =1)


#Exclude == 500 for GLM
sim.output.df.not500 = sim.output.df%>%
  dplyr::filter(simulation.duration != 500)

glm.fit = glm(cbind(simulation.duration, 500)~
                            regresssion.intercept+
                            regression.coefficient,
                          family = "binomial",
                          data = sim.output.df.not500)
summary(glm.fit)
confint(glm.fit)




