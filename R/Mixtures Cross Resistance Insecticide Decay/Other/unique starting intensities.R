set_starting_intervention_intensities = function(start.intervention.intensity,
                                                 sim.array){
  
  if(length(start.intervention.intensity) == 1){sim.array['treatment', , 1] = start.intervention.intensity}
  else(for(insecticide in 1:length(start.intervention.intensity){
      
      sim.array['treatment', insecticide, 1] =  start.intervention.intensity[insecticide]
      
    }))
  
  return(sim.array)
 }










set_starting_refugia_intensities = function(start.refugia.intensity,
                                            sim.array){
  
  if(length(start.refugia.intensity) == 1){
    sim.array['treatment', , 1] = start.refguia.intensity
  }else(
    for(insecticide in 1:length(start.refugia.intensity){
      
      sim.array['treatment', insecticide, 1] =  start.refugia.intensity[insecticide]
      
    })
  )
  return(sim.array)
  
}