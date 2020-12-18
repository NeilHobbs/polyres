#standard deviation could be a composite of of the quality of the bioassays done and the inheriant variability of the 
  #the mosquito population.

sample_for_bioassay_resistance = function(number.of.bioassays,
                                          resistance.intensity,
                                          standard.deviation){ 
  
  
  bioassay.samples = rnorm(n = number.of.bioassays,
                                mean = resistance.intensity,
                                sd = standard.deviation)
  
  
  return(mean(bioassay.samples))
  
}


sample.values = c()
for(i in 1:10000){
sample.values[i] = sample_for_bioassay_resistance(n = 5,
                               resistance.intensity = 100,
                               standard.deviation = 10)
}


hist(sample.values)

