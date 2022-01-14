
plot_decay_rates_graph = function(deployed.efficacy){
  
  generations.vector = seq(0, 30, by = 1)
  efficacy.vector.dr1 = c()
  efficacy.vector.dr2 = c()
  efficacy.vector.dr3 = c()
  efficacy.vector.dr4 = c()
  efficacy.vector.dr5 = c()
  efficacy.vector.dr6 = c()
  efficacy.vector.dr7 = c()
  efficacy.vector.dr8 = c()
  efficacy.vector.dr9 = c()
  efficacy.vector.dr10 = c()
  
  for(i in 1:length(generations.vector)){
    efficacy.vector.dr1[i] = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                    threshold.generations = 30,
                                                                    initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                    base.efficacy.decay.rate = 0,
                                                                    rapid.decay.rate = 0)
    
    efficacy.vector.dr2[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 15,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.015,
                                                                     rapid.decay.rate = 0.08)
    
    efficacy.vector.dr3[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 15,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.025,
                                                                     rapid.decay.rate = 0.08)
    
    efficacy.vector.dr4[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 20,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.025,
                                                                     rapid.decay.rate = 0.08)
    
    efficacy.vector.dr5[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 10,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.025,
                                                                     rapid.decay.rate = 0.08)
    
    efficacy.vector.dr6[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 15,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.005,
                                                                     rapid.decay.rate = 0.08)
    
    efficacy.vector.dr7[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 20,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.005,
                                                                     rapid.decay.rate = 0.08)
    
    efficacy.vector.dr8[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 10,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.005,
                                                                     rapid.decay.rate = 0.08)
    
    efficacy.vector.dr9[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                     threshold.generations = 10,
                                                                     initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                     base.efficacy.decay.rate = 0.015,
                                                                     rapid.decay.rate = 0.08)
    efficacy.vector.dr10[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                      threshold.generations = 20,
                                                                      initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                      base.efficacy.decay.rate = 0.015,
                                                                      rapid.decay.rate = 0.08)
    
    
    
  }
  
  df = data.frame(generations.vector,
                  efficacy.vector.dr1,
                  efficacy.vector.dr2,
                  efficacy.vector.dr3,
                  efficacy.vector.dr4,
                  efficacy.vector.dr5,
                  efficacy.vector.dr6,
                  efficacy.vector.dr7,
                  efficacy.vector.dr8,
                  efficacy.vector.dr9,
                  efficacy.vector.dr10)
  
  
  the.plot = ggplot(df, aes(x=generations.vector, y = efficacy.vector.dr1))+
    geom_line(size = 2, colour = "black")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr2),
              size=2, colour = "red")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr3),
              size=2, colour = "green")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr4),
              size=2, colour = "yellow")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr5),
              size=2, colour = "blue")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr6),
              size=2, colour = "orange")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr7),
              size=2, colour = "purple")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr8),
              size=2, colour = "grey")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr9),
              size=2, colour = "brown")+
    geom_line(aes(x=generations.vector, y=efficacy.vector.dr10),
              size=2, colour = "pink")+
    xlab("Generations Since Insecticide Deployment")+
    ylim(0, 1)+
    ylab("Killing Efficacy of Insecticide Agaist Fully Susceptible Mosquitoes")+
    theme_classic()
  
  return(the.plot)
}
#   
# 
# plot.1 = plot_decay_rates_graph(1)
# plot.2 = plot_decay_rates_graph(0.5)
# 
# gridExtra::grid.arrange(plot.1, plot.2, nrow = 1)



