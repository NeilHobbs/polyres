#Function that converts the array from the simulation into a dataframe

#' @param simulation.array = the array which holds the simulation data
#' @param max.generations = the total length of the simulation
#' @param number.of.insecticides = the number of insectcides that are tracked in the simulation

get_simulation_dataframe = function(simulation.array, max.generations, number.of.insecticides){
  
  data.list = list()
  
  
  sim.duration = length(simulation.array[[2]])
  
  for(insecticide in 1:number.of.insecticides){
    
    if(sim.duration >= max.generations){
      insecticide.tracked = as.character(rep(insecticide, times = (2 * max.generations))) # 2* as refugia and treatment
      
      generation.sequence = seq(1, max.generations, by = 1)
      time.in.generations = rep(generation.sequence, times = 2) # 2* as refugia and treatment
      
      resistance.intensity.refugia = simulation.array[[1]]["refugia", insecticide, ]
      resistance.intensity.refugia = head(resistance.intensity.refugia, n=max.generations)
      resistance.intensity.treatment = simulation.array[[1]]["treatment", insecticide, ]
      resistance.intensity.treatment = head(resistance.intensity.treatment, n=max.generations)
      resistance.intensity = c(resistance.intensity.refugia, resistance.intensity.treatment)
      
      site.refugia = rep("refugia", times = max.generations)
      site.treatment = rep("treatment", times = max.generations)
      site = c(site.refugia, site.treatment)
      
      deployed = simulation.array[[2]]
      deployed_temp = head(deployed, n = max.generations)
      insecticide.deployed = rep(deployed_temp, times = 2) #2 times as refugia and treatment
      
      data.list[[insecticide]]= data.frame(insecticide.tracked, 
                                           resistance.intensity, 
                                           site, 
                                           time.in.generations, 
                                           insecticide.deployed)
    } else{          #Does sim.duration-1 as the final deployed.insecticide is NA which is when the simulation stops
      insecticide.tracked = as.character(rep(insecticide, times = (2 * (sim.duration-1)))) # 2* as refugia and treatment
      time.in.generations = rep(seq(1, (sim.duration-1), by = 1), times = 2) # 2* as refugia and treatment
      
      resistance.intensity = c(head(simulation.array[[1]]["refugia", insecticide, ], n=(sim.duration-1)), #first is refugia
                               head(simulation.array[[1]]["treatment", insecticide, ], n=(sim.duration-1))) #second is treatment
      
      site = c(rep("refugia", times =  (sim.duration-1)), #first is refugia
               rep("treatment", times = (sim.duration-1))) #second is treatment
      
      insecticide.deployed = rep(head(simulation.array[[2]], n=(sim.duration-1)), times = 2) #2 times as refugia and treatment
      
      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.intensity,
                                           site,
                                           time.in.generations,
                                           insecticide.deployed)
    }
  }
  
  final_df = do.call(rbind, data.list)
  return(final_df)
}
