#' @title Convert the array from the simulation into a dataframe
#' 
#' @description 
#' Input the saved simulation array into this function to return a dataframe holding the information of regarding the 
#' simulation. The dataframe has 5 columns. insecticide.tracked is the insecticide to which the resistance.intensity corresponds to
#' at the site (either refugia or treatment), at the timepoint time.in.generations. When the corresponding insecticide.deployed is the
#' insecticide that is deployed in the treatment site at at that generation.

#' @param simulation.array = the array which holds the simulation data called from run_simulation_intervention.
#' @param maximum.generations = The number of generations the simulation was asked to run for (maximum.generations)
#' @param number.of.insecticides = the number of insectcides that are tracked in the simulation
#' 
#' @return final.df A dataframe that has 5 columns, insecticide.tracked, resistance.intensity, site,
#' time.in.generations and insecticide.deployed.




get_simulation_dataframe = function(simulation.array, maximum.generations, number.of.insecticides){
  
  data.list = list()
  
  
  sim.duration = length(simulation.array[[2]])
  
  for(insecticide in 1:number.of.insecticides){
    
    if(sim.duration >= maximum.generations){
      insecticide.tracked = as.character(rep(insecticide, times = (2 * maximum.generations))) # 2* as refugia and treatment
      
      generation.sequence = seq(1, maximum.generations, by = 1)
      time.in.generations = rep(generation.sequence, times = 2) # 2* as refugia and treatment
      
      resistance.intensity.refugia = simulation.array[[1]]["refugia", insecticide, ]
      resistance.intensity.refugia = head(resistance.intensity.refugia, n=maximum.generations)
      resistance.intensity.treatment = simulation.array[[1]]["treatment", insecticide, ]
      resistance.intensity.treatment = head(resistance.intensity.treatment, n=maximum.generations)
      resistance.intensity = c(resistance.intensity.refugia, resistance.intensity.treatment)
      
      site.refugia = rep("refugia", times = maximum.generations)
      site.treatment = rep("treatment", times = maximum.generations)
      site = c(site.refugia, site.treatment)
      
      deployed = simulation.array[[2]]
      deployed_temp = head(deployed, n = maximum.generations)
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




#This function returns a dataframe that has 5 columns. And the number of rows is n*2 (refugia and treatment)
# times the duration. Where n is the number of insecticides included in the simulation.
