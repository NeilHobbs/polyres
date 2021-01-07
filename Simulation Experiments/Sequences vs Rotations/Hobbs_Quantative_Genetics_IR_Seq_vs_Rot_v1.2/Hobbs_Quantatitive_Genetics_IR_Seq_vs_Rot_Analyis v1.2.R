#Hobbs_Quantatitive_Genetics_IR_Seq_vs_Rot_Analysis v1.2.R
##########Data Analysis###########
library(dplyr)
library(epiR)


#Other packages used but called through package.name::  lemon ; cowplot

#Read in datasets
  #Contains the output from the sequence and rot simulations.
sequence.rotation.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/sequence_rotation_df.csv")

df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/lhs_values.csv")#The parameters used in the parameter space testing

#Has differences between Rot and Seq
all_sims_join = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot v1.2/seq_rot_sims.csv")


##############################
# No Cross Selection Analysis#
##############################

replicate = seq(1, 45000, by = 1)

all_sims_join = data.frame(all_sims_join, replicate)

#Where the final simulation duration was a tie
sim_equal_gens = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence == simulation.duration.rotation)

rotation_wins = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence < simulation.duration.rotation)

sequence_wins = all_sims_join%>%
  dplyr::filter(simulation.duration.sequence > simulation.duration.rotation)

#Number of simulations that were wins/draws
nrow(rotation_wins)
nrow(sequence_wins)
nrow(sim_equal_gens)

#Calculate the wins/draws as proprtions
nrow(rotation_wins)/nrow(all_sims_join) #rotation wins
nrow(sequence_wins)/nrow(all_sims_join) #sequence wins
nrow(sim_equal_gens)/nrow(all_sims_join) #draws


#Calculate as proportions
nrow(rotation_wins%>%
       dplyr::filter(no.insecticides == 2))

nrow(rotation_wins%>%
       dplyr::filter(no.insecticides == 3))

nrow(rotation_wins%>%
       dplyr::filter(no.insecticides == 4))



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
  
  
  #Perform two sided partial rank correlation. 95% CIs
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

#Generalised Linear Modelling:
#make sure the number of insecticides and deployment intervals are set as factors
sequence.rotation.df$dep.freq = factor(sequence.rotation.df$dep.freq,
                                       levels = c("5", "10", "20"))

sequence.rotation.df$insecticides.in.sim = factor(sequence.rotation.df$insecticides.in.sim,
                                                  levels = c("2", "3", "4"))


sequence.rotation.df$duration.completion = sequence.rotation.df$simulation.duration / 500

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
                      dep.freq+ #not required to smooth over
                      insecticides.in.sim+ #not required to smooth over
                      strategy, #not required to smooth over
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
step(glm.fit.binomial)#recommends removal of dispersal spline

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


#############################
## Cross Selection Analysis ##
#############################
#Cross selection runs data:: Only run at 10 deployment interval and 2 insecticides.
sequence.rotation.cross.full.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/sequence.rotation.cross.full.df.csv")

#partial correlation needs each endpoint as the final column. 
df_from_pcor_test_cross = function(irm.strategy,
                                   cross,
                                   data){
  pcor.df = data%>%
    dplyr::filter(strategy == strategy)%>%
    dplyr::filter(cross.resistance == cross)%>%
    dplyr::select("insecticide.resistance.hertiability",
                  "resistance.cost", 
                  "male.insecticide.exposure",
                  "intervention.coverage", 
                  "dispersal", 
                  "female.insecticide.exposure", 
                  "mean.resistance.intensity.")
  
  
  #Perform two sided partial rank correlation. 95% CIs
  pcor.output = epiR::epi.prcc(dat = pcor.df,
                               sided.test = 2,
                               conf.level = 0.95)
  
  
  estimate = pcor.output$est
  pvalue = pcor.output$p.value
  teststat = pcor.output$test.statistic
  lower.95.ci = pcor.output$lower
  upper.95.ci = pcor.output$upper
  
  
  
  cross.selection = as.factor(cross)
  strategy = irm.strategy
  Scenario = stringr::str_c(irm.strategy, " with ", cross, " cross selection")
  
  #parameters in correct order::
  parameter = c("Insecticide Resistance Heritability", "Fitness Cost", "Male Insecticide Exposure",
                "Intervention Coverage", "Mosquito Dispersal", "Female Insecticide Exposure")
  
  output = data.frame(estimate, lower.95.ci, upper.95.ci,  pvalue, teststat, cross.selection,
                      strategy, parameter, Scenario)
  
  return(output)
}



pcor.cross.seq.neg.0.3 = df_from_pcor_test_cross("sequence", -0.3, sequence.rotation.cross.full.df)
pcor.cross.seq.neg.0.2 = df_from_pcor_test_cross("sequence", -0.2, sequence.rotation.cross.full.df)
pcor.cross.seq.neg.0.1 = df_from_pcor_test_cross("sequence", -0.1, sequence.rotation.cross.full.df)
pcor.cross.seq.zero = df_from_pcor_test_cross("sequence", 0, sequence.rotation.cross.full.df)
pcor.cross.seq.pos.0.1 = df_from_pcor_test_cross("sequence", 0.1, sequence.rotation.cross.full.df)
pcor.cross.seq.pos.0.2 = df_from_pcor_test_cross("sequence", 0.2, sequence.rotation.cross.full.df)
pcor.cross.seq.pos.0.3 = df_from_pcor_test_cross("sequence", 0.3, sequence.rotation.cross.full.df)

pcor.cross.rot.neg.0.3 = df_from_pcor_test_cross("rotation", -0.3, sequence.rotation.cross.full.df)
pcor.cross.rot.neg.0.2 = df_from_pcor_test_cross("rotation", -0.2, sequence.rotation.cross.full.df)
pcor.cross.rot.neg.0.1 = df_from_pcor_test_cross("rotation", -0.1, sequence.rotation.cross.full.df)
pcor.cross.rot.zero = df_from_pcor_test_cross("rotation", 0, sequence.rotation.cross.full.df)
pcor.cross.rot.pos.0.1 = df_from_pcor_test_cross("rotation", 0.1, sequence.rotation.cross.full.df)
pcor.cross.rot.pos.0.2 = df_from_pcor_test_cross("rotation", 0.2, sequence.rotation.cross.full.df)
pcor.cross.rot.pos.0.3 = df_from_pcor_test_cross("rotation", 0.3, sequence.rotation.cross.full.df)


pcor.cross.df = rbind(pcor.cross.seq.neg.0.3,
                      pcor.cross.seq.neg.0.2,
                      pcor.cross.seq.neg.0.1,
                      pcor.cross.seq.zero,
                      pcor.cross.seq.pos.0.1,
                      pcor.cross.seq.pos.0.2,
                      pcor.cross.seq.pos.0.3,
                      pcor.cross.rot.neg.0.3,
                      pcor.cross.rot.neg.0.2,
                      pcor.cross.rot.neg.0.1,
                      pcor.cross.rot.zero,
                      pcor.cross.rot.pos.0.1,
                      pcor.cross.rot.pos.0.2,
                      pcor.cross.rot.pos.0.3)

#Prevent overwriting
###write.csv(pcor.cross.df, ".//partial_rank_correlation_cross.csv")

sequence.rotation.cross.full.df$duration.completion = sequence.rotation.cross.full.df$simulation.duration/500

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

