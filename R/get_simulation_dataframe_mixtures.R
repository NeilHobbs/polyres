#'@title Get a dataframe of a mixture simulation
#'
#'@description Helper function to aid in the conversion of the simulation to a dataframe for use in plotting/analysis
#'
#'@param simulation.array = The simulation 
#'@param maximum.generation = The maximum number of generations the simulation was set to run for
#'@param number.of.insecticides = The number of insecticides included in the simulation

get_simulation_dataframe_mixtures = function(simulation.array, 
                                             maximum.generations, 
                                             number.of.insecticides){
  
  data.list = list()
  
  
  sim.duration = nrow(simulation.array[[2]])
  
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
      mixture.id = head(deployed$mixture.id, n = maximum.generations)
      mixture.part.1 = head(deployed$mixture.part.1, n = maximum.generations)
      mixture.part.2 = head(deployed$mixture.part.2, n = maximum.generations)
      
      
      deployed.mixture.id = as.character(rep(mixture.id, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.1 = as.character(rep(mixture.part.1, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.2 = as.character(rep(mixture.part.2, times = 2)) #2 times as refugia and treatment
      
      data.list[[insecticide]]= data.frame(insecticide.tracked, 
                                           resistance.intensity, 
                                           site, 
                                           time.in.generations, 
                                           deployed.mixture.id,
                                           deployed.mixture.part.1,
                                           deployed.mixture.part.2)
    } else{          #Does sim.duration-1 as the final deployed.insecticide is NA which is when the simulation stops
      insecticide.tracked = as.character(rep(insecticide, times = (2 * (sim.duration-1)))) # 2* as refugia and treatment
      time.in.generations = rep(seq(1, (sim.duration-1), by = 1), times = 2) # 2* as refugia and treatment
      
      resistance.intensity = c(head(simulation.array[[1]]["refugia", insecticide, ], n=(sim.duration-1)), #first is refugia
                               head(simulation.array[[1]]["treatment", insecticide, ], n=(sim.duration-1))) #second is treatment
      
      site = c(rep("refugia", times =  (sim.duration-1)), #first is refugia
               rep("treatment", times = (sim.duration-1))) #second is treatment
      
      deployed = simulation.array[[2]]
      mixture.id = head(deployed$mixture.id, n = (sim.duration - 1))
      mixture.part.1 = head(deployed$mixture.part.1, n = (sim.duration - 1))
      mixture.part.2 = head(deployed$mixture.part.2, n = (sim.duration - 1))
      
      deployed.mixture.id = as.character(rep(mixture.id, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.1 = as.character(rep(mixture.part.1, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.2 = as.character(rep(mixture.part.2, times = 2)) #2 times as refugia and treatment
      
      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.intensity,
                                           site,
                                           time.in.generations,
                                           deployed.mixture.id,
                                           deployed.mixture.part.1,
                                           deployed.mixture.part.2)
    }
  }
  
  final_df = do.call(rbind, data.list)
  return(final_df)
}
