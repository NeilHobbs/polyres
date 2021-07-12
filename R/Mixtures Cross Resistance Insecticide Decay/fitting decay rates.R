##For Olyset®, 2% permethrin, Sumitomo Chemical Ltd
mortality.values = c(0.90, 0.85, 0.70, 0.70, 0.10)
mortality.values.1 = c(0.90, 0.85, 0.70, 0.70)
generation.values = c(0, 6, 12, 18)

generation = c(0, 6, 12, 18, 24)

plot(y=mortality.values, x=generation)
lines(y=mortality.values, x=generation)

generations.vector = seq(0, 30, by = 1)

efficacy.vector = c(0.9/1)

for(i in 2:length(generations.vector)){
  efficacy.vector[i] = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                              threshold.generations = 18,
                                                              initial.insecticide.efficacy = 0.9/1,
                                                              base.efficacy.decay.rate = 0.012500 ,
                                                              rapid.decay.rate = 0.06)
}

lines(generations.vector, efficacy.vector, col = "red")

estimated.vals = efficacy.vector[c(1, 6, 12, 18, 24)]


temp.lm = lm(mortality.values.1 ~generation.values)

summary(temp.lm)
