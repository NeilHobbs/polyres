#standard deviation could be a composite of of the quality of the bioassays done and the inheriant variability of the 
  #the mosquito population. 

sample_for_bioassay_resistance = function(number.of.bioassays,
                                          resistance.intensity.intervention,
                                          resistance.intensity.refugia,
                                          standard.deviation,
                                          intervention.coverage){ 
  
  number.intervention.bioassays = round((intervention.coverage * number.of.bioassays), digits = 0) 
    
  number.refugia.bioassays = round(((1 - intervention.coverage) * number.of.bioassays), digits = 0) 
  
  assays = number.intervention.bioassays + number.refugia.bioassays
  
  
  number.intervention.bioassays = ifelse(assays < number.of.bioassays,
                                    yes = (number.intervention.bioassays + 1), no = number.intervention.bioassays)
  
  
   bioassay.samples.refugia = rnorm(n = number.refugia.bioassays,
                                        mean = resistance.intensity.refugia,
                                        sd = standard.deviation)



  bioassay.samples.intervention = rnorm(n = number.intervention.bioassays,
                                mean = resistance.intensity.intervention,
                                sd = standard.deviation)
  
  all.samples = c(bioassay.samples.intervention, bioassay.samples.refugia)
  
  sampled.resistance = mean(all.samples)
  
  return(sampled.resistance)
  
}


A = sample_for_bioassay_resistance(number.of.bioassays = 17,
                                   resistance.intensity.intervention = 60,
                                   resistance.intensity.refugia = 50,
                                   standard.deviation = 5,
                                   intervention.coverage = 0.7)


sample.values = c()
for(i in 1:10000){
sample.values[i] = sample_for_bioassay_resistance(number.of.bioassays = 17,
                                                  resistance.intensity.intervention = 60,
                                                  resistance.intensity.refugia = 50,
                                                  standard.deviation = 5,
                                                  intervention.coverage = 0.7)
}


hist(sample.values)

