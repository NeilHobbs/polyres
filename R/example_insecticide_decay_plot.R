example_insecticide_decay_plot = function(deployment.frequency,
                                         base.decay.rate,
                                         rapid.decay.rate,
                                         threshold.generation,
                                         applied.insecticide.dose,
                                         recommended.insecticide.dose){
  
  
  generations.vector = seq(1, deployment.frequency, by = 1)

  efficacy.vector = c(applied.insecticide.dose/recommended.insecticide.dose)
  
  for(i in 2:length(generations.vector)){
    efficacy.vector[i] = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                threshold.generations = threshold.generation,
                                                                initial.insecticide.efficacy = applied.insecticide.dose/recommended.insecticide.dose,
                                                                base.efficacy.decay.rate = base.decay.rate,
                                                                rapid.decay.rate = rapid.decay.rate)
  }
  



  df = data.frame(generations.vector,
                  efficacy.vector)
  
  efficacy.plot = ggplot(df, aes(x=generations.vector, y = efficacy.vector))+
    geom_line()+
    geom_vline(xintercept = threshold.generation, linetype = "dashed", colour = "red")+
    geom_point(size = 2)+
    xlab("Generations Since Insecticide Deployment")+
    ylab("Efficacy of Insecticide")+
    theme_classic()
  
  return(efficacy.plot)
  
}

# example_insecticide_decay_rates_plot()

