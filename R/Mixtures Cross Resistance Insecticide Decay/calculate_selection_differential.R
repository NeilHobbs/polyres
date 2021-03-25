#
calculate_selection_differential_singe_insecticide = function(male.exposure,
                                                              female.exposure,
                                                              initial.resistance.intensity,
                                                              population.sd,
                                                              insecticide.efficacy,
                                                              half.population.bioassay.survival.resistance,
                                                              conversion.factor,
                                                              intercept
){
  #Calculate the proportion of the population encountering the insecticide
  exposure = (female.exposure + (male.exposure*female.exposure))/2
  
  
  surviving.proportion = (convert_bioassay_survival_to_field(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                 mean.population.resistance = initial.resistance.intensity,
                                                                                                                 michaelis.menten.slope = 1,
                                                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                 sd.population.resistance = 0, #Set at zero to get exact mean value
                                                                                                                 nsim = 1),
                                                             conversion.factor = conversion.factor,
                                                             intercept = intercept))^insecticide.efficacy
  
  
  #Truncation selection for selection intensity
  selection.intensity = dnorm(qnorm(1-surviving.proportion))/surviving.proportion
  selection.differential.exposed = selection.intensity * population.sd #this is the selection of those mosquitoes exposed in the intervention site
  mean.resistance.intensity.survived = selection.differential.exposed + initial.resistance.intensity
  
  
  numerator = ((1-exposure)*initial.resistance.intensity) + (exposure*surviving.proportion*mean.resistance.intensity.survived)
  denominator = ((1-exposure)+(exposure*surviving.proportion))
  
  selection.differential = (numerator/denominator) - initial.resistance.intensity
  
  
  return(selection.differential)
}

calculate_selection_differential_singe_insecticide(male.exposure = 0.5,
                                 female.exposure = 0.5,
                                 initial.resistance.intensity = 0,
                                 population.sd = 10,
                                 insecticide.efficacy = 1,
                                 half.population.bioassay.survival.resistance = 900,
                                 conversion.factor = 0.48,
                                 intercept = 0.15
)


changing_selection_differential = function(sd,
                                           z.value,
                                           male.exposure,
                                           female.exposure){
  efficacy.values = seq(0, 1.2, by=0.01) #includes 20% overspraying
  
  selection.differential.vals = c()
  
  for(i in 1:121){
    
    selection.differential.vals[i] =  calculate_selection_differential_singe_insecticide(male.exposure = male.exposure,
                                                                       female.exposure = female.exposure,
                                                                       initial.resistance.intensity = z.value,
                                                                       population.sd = sd,
                                                                       insecticide.efficacy = efficacy.values[i],
                                                                       half.population.bioassay.survival.resistance = 900,
                                                                       conversion.factor = 0.48,
                                                                       intercept = 0.15
    )
    
    
  }
  
  
  df = data.frame(efficacy.values, selection.differential.vals)
  
  graph = ggplot(df, aes(x=efficacy.values, y=selection.differential.vals))+
    geom_point()+
    xlab("Insecticide Efficacy Relative")+
    ylab("Selection Differential")+
    ggtitle(paste0("SD =", sd, ",  z value=", z.value))+
    theme_bw()
  
  return(graph)
}



graph.a = changing_selection_differential(sd = 5, z.value = 0, male.exposure = 0.5, female.exposure = 0.5)
graph.b = changing_selection_differential(sd = 10, z.value = 0, male.exposure = 0.5, female.exposure = 0.5)
graph.c = changing_selection_differential(sd = 20, z.value = 0, male.exposure = 0.5, female.exposure = 0.5)
graph.d = changing_selection_differential(sd = 5, z.value = 50, male.exposure = 0.5, female.exposure = 0.5)
graph.e = changing_selection_differential(sd = 10, z.value = 50, male.exposure = 0.5, female.exposure = 0.5)
graph.f = changing_selection_differential(sd = 20, z.value = 50, male.exposure = 0.5, female.exposure = 0.5)
graph.g = changing_selection_differential(sd = 5, z.value = 200, male.exposure = 0.5, female.exposure = 0.5)
graph.h = changing_selection_differential(sd = 10, z.value = 200, male.exposure = 0.5, female.exposure = 0.5)
graph.i = changing_selection_differential(sd = 20, z.value = 200, male.exposure = 0.5, female.exposure = 0.5)

gridExtra::grid.arrange(graph.a, graph.b, graph.c,
                        graph.d, graph.e, graph.f,
                        graph.g, graph.h, graph.i)


#The selection differential appears to remain stable within the 
 #z values which would be of operational importance.





##Check for scaling Beta again

male.exposure.vals = runif(10000, min = 0, max = 1)
female.exposure.vals = runif(10000, min = 0.4, max = 0.9)
heritability.vals = runif(10000, min = 0.05, max = 0.3)

selection.vals = c()

for(i in 1:10000){
  selection.vals[i] = calculate_selection_differential_singe_insecticide(male.exposure = male.exposure.vals[i],
                                                                         female.exposure = female.exposure.vals[i],
                                                                         initial.resistance.intensity = 0,
                                                                         population.sd = 10,
                                                                         insecticide.efficacy = 1,
                                                                         half.population.bioassay.survival.resistance = 900,
                                                                         conversion.factor = 0.48,
                                                                         intercept = 0.15)
}


response.vals = 100/(selection.vals*heritability.vals)*10
response.vals = data.frame(response.vals)

years.to.failure = 10/response.vals

ggplot(response.vals, aes(x=response.vals))+
  geom_histogram(bins = 100)+
  scale_x_continuous(breaks = c(0, 10, 100, 200, 1000, 2000))



get_R_values = function(male.exposure,
                        female.exposure,
                        initial.resistance.intensity,
                        population.sd,
                        insecticide.efficacy,
                        half.population.bioassay.survival.resistance,
                        conversion.factor,
                        intercept,
                        heritability){
  
  R = calculate_selection_differential_singe_insecticide(male.exposure = male.exposure,
                                       female.exposure = female.exposure,
                                       initial.resistance.intensity = initial.resistance.intensity,
                                       population.sd = population.sd,
                                       insecticide.efficacy = insecticide.efficacy,
                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                       conversion.factor = conversion.factor,
                                       intercept = intercept) * heritability
  
  return(R)
}

heritability.vals = runif(100000, min=0.05, max=0.30)
male.exposure.vals = runif(100000, min = 0, max=1)
female.exposure.vals = runif(100000, min=0.4, max=0.9)
sd.vals = runif(100000, min = 5, max = 20)
r.vals = c()

for(i in 1:100000){
  
  r.vals[i] = get_R_values(male.exposure = male.exposure.vals[i],
                           female.exposure = female.exposure.vals[i],
                           initial.resistance.intensity = 10,
                           population.sd = sd.vals[i],
                           insecticide.efficacy = 1,
                           half.population.bioassay.survival.resistance = 900,
                           conversion.factor = 0.48,
                           intercept = 0.15,
                           heritability = heritability.vals[i])
}
years.to.fail = 10/r.vals
hist(years.to.fail, breaks=100)

