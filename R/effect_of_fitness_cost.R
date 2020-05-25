#' This function implements the fitness cost associated with having insecticide resistance.
#' 
#' @param resistance.cost Costs associated with insecticide resistance. Values can be 0.01 to 0.2
#' @param resistance.selection.response Value input from response_to_insecticide_selection.
#' 
#' @return fitness.cost change in population resistance intensity


#[NB set a check in the code so that z_bar cannot fall below 0 when the insecticide is not being deployed] 

effect_of_fitness_cost = function(resistance.cost, resistance.selection.response){
  
  fitness.cost = (-resistance.cost) * resistance.selection.response
  
  return(fitness.cost)
  
}