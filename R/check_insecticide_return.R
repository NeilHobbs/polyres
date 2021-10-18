#' @title Check whether an insecticide should be returned back to the arsenal having been withdrawn. 
#' 
#' @description 
#' Checks if a specified insecticide at a specified generation should be returned back to the arsenal for use. 
#' This is based on whether the resistance intensity to this insecticide is less than or equal to the return threshold.
#' 
#' @param insecticide = The insecticide to which the resistance intensity is to be checked against
#' @param current.generation = The generation where the run_simulation_intervention() is up to. 
#' @param return.threshold = The resistance intensity at which an insecticide should no longer be withdrawn; and returned to the arsenal.
#' @param simulation.array = The array which holds the run_simulation_intervention() results. 
#' 
#' @return A TRUE or FALSE result.



check_insecticide_return = function(insecticide,
                                    current.generation, 
                                    return.threshold, #this will be as an intensity value.
                                    simulation.array){
  
  #Is the resistance intensity less than or equal to the return threshold intensity?
  answer = ifelse(simulation.array["treatment", insecticide, current.generation] <= return.threshold, 
                  yes = TRUE, #The insecticide should be returned.
                  no = FALSE) #The insecticide should not be returned.
  
  return(answer)
  
}
#There will be a line of code which converts a user input of a survival threshold to the corresponding intensity.
#It is more user friendly to input the survival proportion rather than have users calculate the resistance intensity level separately.
#This is a TRUE or FALSE result
