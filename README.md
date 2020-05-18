# polyres
Incorporating polygenic resistance into Insecticide Resistance Management

Deciding upon intitial scale of insecticide resistance intensity. 
The function calculate_half_population_survival allows for the calculation of a user defined resistance scale. 
The user should specify what resistance value is required at a particular cut-off point. For instance, an intuitive scale would involve a resistance value of 100 corresponding to a survival of 10% (0.1 as a proportion). Using these values the insecticide resistance intensity at which 50% of exposed mosquitoes would survive can be calculated. 

calculate_half_population_survival(desired.resistance = 100,
                                  desired.survival.proportion = 0.1,
                                  maximum.bioassay.survival.proportion = 1,
                                  michaelis.menton.slope = 1, 
                                  estimate.precision = 0.01, 
                                  sd.population.resistance = 0,
                                  nsim = 1000,
                                  minimum.resistance.value = 0, 
                                  maximum.resistance.value = 5000)
