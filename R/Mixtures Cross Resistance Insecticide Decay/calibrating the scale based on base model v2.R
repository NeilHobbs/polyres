calculate_selection_differential(male.exposure = 1,
                                 female.exposure = 1,
                                 initial.resistance.intensity = 200,
                                 population.sd = 5,
                                 insecticide.efficacy = 1,
                                 half.population.bioassay.survival.resistance = 900,
                                 conversion.factor = 0.48,
                                 intercept = 0.15)

calculate_selection_differential_fitness_cost(0.93, 5)

relative.fitness.vals = runif(100000, min = 0.9, max = 0.99)
male.exposure.vals = runif(100000, min = 0, max = 1)
female.exposure.vals = runif(100000, min = 0.4, max = 0.9)
heritability.vals = runif(100000, min = 0.05, max = 0.3)
sd.vals = runif(100000, min = 1, max=5)
response.vals = c()

for(i in 1:100000){
  
  response.vals[i] = (12.5*calculate_selection_differential(male.exposure = male.exposure.vals[i],
                                                      female.exposure = female.exposure.vals[i],
                                                      initial.resistance.intensity = 0,
                                                      population.sd = sd.vals[i],
                                                      insecticide.efficacy = 1,
                                                      half.population.bioassay.survival.resistance = 900,
                                                      conversion.factor = 0.48,
                                                      intercept = 0.15) +
    calculate_selection_differential_fitness_cost(relative.fitness = relative.fitness.vals[i],
                                                  population.sd = sd.vals[i])) * heritability.vals[i]
  
}

years = (10/response.vals)
years = data.frame(years)
range(years)

ggplot(years, aes(x=years))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 10, colour ="red")#+
  #xlim(0, 100)
  
years%>%
  dplyr::filter(years > 10)%>%
  nrow()

years%>%
  dplyr::filter(years <= 10)%>%
  nrow()


