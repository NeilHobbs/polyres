#' @title Create the array for holding the simulation.
#'
#'@description
#'Returns an array: site, insecticide, generation.
#'The site will be refugia or treatment. And will hold the corresponding resistance intensity values. The insecticide is the 
#'insecticide to which the resistance intensity corresponds to. The generation is the timepoint in the model to which the intensity 
#'corresponds to. 
#'
#' @param n.insecticides the number of different insecticides in the armoury.
#' @param maximum.generation the maximum number of generations the model would be able to run for.
#' 
#' @return simulation.data an array which will hold the data for simulation


#Creates a named array
create_starting_array = function(n.insecticides, maximum.generations){
  
  #dimension 1: site: refugia [1,,] or treatment[2,,]
  #dimension 2: insecticide to which resistance intensity corresponds
  #dimension 3: generation of mosquito

  simulation.data = array_named(site = c("refugia", "treatment"), 
                                insecticide = 1:n.insecticides, 
                                generation = 1:maximum.generations) #this is the insecticide that is currently deployed
  }


####NEED TO CHECK THAT THE PARAMETER NAMES MATCH THE OTHER FUNCTIONS##########
