plot_insecticide_decay_rate = function(base.decay.rate,
                                       rapid.decay.rate,
                                       threshold.generation,
                                       deployment.frequency,
                                       deployed.dosage,
                                       recommended.dosage){
  
  
  generations.vector = seq(0, deployment.frequency, by = 1)
  efficacy.vector = c()
  
  for(i in 1:length(generations.vector)){
    efficacy.vector[i] = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                threshold.generations = threshold.generation,
                                                                initial.insecticide.efficacy = (deployed.dosage/recommended.dosage),
                                                                base.efficacy.decay.rate = base.decay.rate,
                                                                rapid.decay.rate = rapid.decay.rate)
  }
  
  df = data.frame(generations.vector,
                  efficacy.vector)
  
  label.df = data.frame(
    text.label =c("Threshold Generation"),
    label_x_coord = c(threshold.generation),
    label_y_coord = c(0.5)
  )
  
  
  max.value = ifelse(max(efficacy.vector > 1),
                     yes = max(efficacy.vector),
                     no = 1)
  
  efficacy.plot = ggplot(df, aes(x=generations.vector, y = efficacy.vector))+
    geom_line()+
    geom_vline(xintercept = threshold.generation, linetype = "dashed", colour = "red")+
    geom_point(size = 2)+
    geom_text(data = label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
              angle = 270, size = 8)+
    xlab("Generations Since Insecticide Deployment")+
    ylim(0, max.value)+
    ylab("Efficacy of Insecticide")+
    theme_classic()
  
  return(efficacy.plot)
  
}

# plotinsecticide_decay_rate(base.decay.rate = 0.001,
#                             rapid.decay.rate = 0.05,
#                             threshold.generation = 10,
#                             deployment.frequency = 20,
#                             deployed.dosage = 1,
#                             recommended.dosage = 1)

