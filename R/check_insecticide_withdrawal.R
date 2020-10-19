#' @title Check if insecticides should be withdrawn from the arsenal based on the resistance intensity.
#' 
#' @param insecticide = The insecticide to which the resistance intensity is to be checked against
#' @param current.generation = The generation where the run_simulation() is up to. 
#' @param withdrawal.threshold = The resistance intensity at which an insecticide should no longer be used and therefore withdrawn.
#' @param simulation.array = The array which holds the run_simulation results. 

check_insecticide_withdrawal = function(insecticide,
                                        current.generation,
                                        withdrawal.threshold,
                                        simulation.array){
  
  answer = ifelse(simulation.array["treatment", insecticide, current.generation] >= withdrawal.threshold, yes = TRUE, no = FALSE) 
  
  return(answer)
}


#If TRUE the insecticide should be withdrawn
#If FALSE the insecticide does not need to be withdrawn



