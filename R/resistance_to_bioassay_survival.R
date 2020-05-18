#Function to translate IR levels(z) into survival 
#Kmax = the maximum proportion surviving (must be set at 1)
#z = the insecticide resistance intensity level
#n=slope of the curve (tangent to the curve at z_50)
#z_50 = the insecticide resistance intensity level that has 50% survival
#sigma = standard devation of the normal distribution
#nsim = the number of simulations for the rnorm function

resistance_to_bioassay_survival = function(maximum.bioassay.survival.proportion,
                                           mean.population.resistance,
                                           michaelis.menton.slope, 
                                           half.population.bioassay.survival.resistance,
                                           sd.population.resistance, 
                                           nsim){ 
  
  #Generate a Normal distribution around the population mean of insecticide resistance values
  resistance_values = rnorm(nsim, 
                            mean = mean.population.resistance, 
                            sd = sd.population.resistance) 
  
  
  #Prevent Insecticide Resistance being less than 0, as this would give survival less than 0
  resistance_values = ifelse(resistance_values < 0, 0, resistance_values) 
  
  ##Calculate Bioassay Survival (Equation 6)
  bioassay.survival.proportion = (maximum.bioassay.survival.proportion * 
                                    (resistance_values^michaelis.menton.slope)) / 
    (half.population.bioassay.survival.resistance + 
       (resistance_values ^michaelis.menton.slope))  
  
  bioassay.survival.proportion = ifelse(bioassay.survival.proportion < 0, 0, bioassay.survival.proportion) #Prevent survival being less than zero.
  
  return(mean(bioassay.survival.proportion))
}




