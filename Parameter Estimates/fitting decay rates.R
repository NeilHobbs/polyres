##For Olyset®, 2% permethrin, Sumitomo Chemical Ltd ; estimates from Toé et al., 2019 Figure 1.

#Mortality of Susceptibles in Cone assays from field obtained nets. 

mortality.values = c(0.90, 0.85, 0.70, 0.70, 0.10)
generation = c(0, 5, 10, 15, 20) #months: 0, 6, 12, 18, 24

plot(y=mortality.values, x=generation)
lines(y=mortality.values, x=generation)

generations.vector = seq(0, 30, by = 1)

efficacy.vector = c(0.9/1)

for(i in 2:length(generations.vector)){
  efficacy.vector[i] = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                              threshold.generations = 15,
                                                              initial.insecticide.efficacy = 0.9/1,
                                                              base.efficacy.decay.rate = 0.015,
                                                              rapid.decay.rate = 0.08)
}

lines(generations.vector, efficacy.vector, col = "red")

predicted.decay.df = data.frame(generations.vector, efficacy.vector)
measured.decay.df = data.frame(mortality.values, generation)

ggplot(predicted.decay.df, aes(x=generations.vector, efficacy.vector))+
  geom_line()+
  geom_point()+
  geom_point(data=measured.decay.df, aes(y=mortality.values,
                                         x=generation),
             size = 2, colour = "red",
             alpha = 0.7)+
  geom_vline(xintercept = 15, linetype = "dashed", colour = "blue")+
  xlab("Time in Mosquito Generations")+
  ylab("Insecticide Efficacy")+
  ylim(0, 1)+
  theme_classic()+
 theme(axis.text = element_text(size = 12),
       axis.title = element_text(size = 18))






insecticide.efficacy


