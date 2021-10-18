#'@title Plot an example of how insecticide decay impacts insecticide efficacy


example_insecticide_decay_rates_plot = function(){
  
  
  generations.vector = rep(seq(0, 15, by = 1), 4)
  base.vector = c(rep(0.01, 16),rep(0.05, 16), rep(0.1, 16), rep(0.025, 16))
  rapid.vector = rep(rep(0.5, 16),4)
  threshold.vector = rep(rep(10, 16), 4)
  example.vector = c(rep("A", 16), rep("B", 16), rep("C", 16), rep("D", 16))
  efficacy.vector = c()
  
  for(i in 1:length(generations.vector)){
    efficacy.vector[i] = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                threshold.generations = threshold.vector[i],
                                                                initial.insecticide.efficacy = 1,
                                                                base.efficacy.decay.rate = base.vector[i],
                                                                rapid.decay.rate = rapid.vector[i])
  }
  
  df = data.frame(generations.vector,
                  base.vector,
                  rapid.vector,
                  threshold.vector,
                  efficacy.vector,
                  example.vector)
  
  efficacy.plot = ggplot(df, aes(x=generations.vector, y = efficacy.vector,
                                 colour = example.vector))+
    geom_line()+
    geom_vline(xintercept = 10, linetype = "dashed", colour = "red")+
    geom_point(size = 2)+
    xlab("Generations Since Insecticide Deployment")+
    ylab("Efficacy of Insecticide")+
    labs(colour = "Example")+
    theme_classic()
  
  return(efficacy.plot)
  
}

# example_insecticide_decay_rates_plot()

