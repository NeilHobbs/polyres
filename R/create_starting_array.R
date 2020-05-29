#'This function sets the starting conditions for the simulations
#'


#Creates a named array
create_starting_array = function(n.insecticides, maximum.generations){
  
  #dimension 1: site: refugia or treatment
  #dimension 2: insecticide to which resistance intensity corresponds
  #dimension 3: generation of mosquito

  simulation.data = array_named(site = c("refugia", "treatment"), insecticide = 1:n.insecticides,
                       generation = 1:maximum.generations)
  }

