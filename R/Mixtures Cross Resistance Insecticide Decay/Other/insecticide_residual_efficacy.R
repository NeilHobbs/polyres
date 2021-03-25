calculate_insecticide_efficacy = function(start.efficacy,
                                          generations.in.deployment,
                                          decay.rate,
                                          rapid.decay,
                                          cut.off){
  
  
  generations.after.cutoff = generations.in.deployment - cut.off
  
    #Equation from Yakob, Dunning, Yan 2010.
        #Modified to allow for decay to speed up after a set time frame.
  #perhaps worth having slow decay for first X generations then rapid after that.
  #Rationale: Public Health Insecticides are designed to maintain a high efficacy then rapidly decay.
  
  if(generations.in.deployment <= cut.off){
  current.efficacy = start.efficacy * exp(-((generations.in.deployment^2) * decay.rate))
  }else{
    pre.cut.off = start.efficacy * exp(-((generations.in.deployment^2) * decay.rate))
                                                                              
    current.efficacy = pre.cut.off * exp(-((generations.after.cutoff^2) * rapid.decay))                                          
    
  }
  #Then rapid decay thereafter:
  
  
  return(current.efficacy)
  
}


time.vec = seq(0, 9, by = 1)
  

efficacy = c()
for(i in 1:length(time.vec))(
  
  efficacy[i] = calculate_insecticide_efficacy(start.efficacy = 1,
                                               generations.in.deployment = time.vec[i],
                                               decay.rate = 0.005,
                                               rapid.decay = 0.5,
                                               cut.off = 2)
  
)

plot(x=time.vec, y = efficacy)


min(efficacy)


