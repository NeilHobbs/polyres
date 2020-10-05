#A function to check if insecticides should be returned back to the arsenal. 
#There will be a line of code which converts a user input of a survival threshold to the corresponding intensity.
#It is more user friendly to input the survival proportion rather than have users calculate the resistance intensity level separately.
#This is a TRUE or FALSE result:

#' @param insecticide = The insecticide to which the resistance intensity is to be checked against
#' @param current.generation = The generation where the run_simulation() is up to. 
#' @param return.threshold = The resistance intensity at which an insecticide should no longer be withdrawn; and returned to the arsenal.
#' @param simulation.array = The array which holds the run_simulation results. 



check_insecticide_return = function(insecticide,
                                    current.generation,
                                    return.threshold,
                                    simulation.array){
  
  answer = ifelse(simulation.array["treatment", insecticide, current.generation] <= return.threshold, yes = TRUE, no = FALSE) 
  
}
