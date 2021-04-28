#' @title Make discrete mixtures
#' 
#' @description This function makes mixture pairs that are discrete: eg. 1,2 ; 3,4 etc. 
#' Will only work if number.of.insecticides is EVEN.
#' 
#' @param number.of.insecticides = The total number of insecticides included in the simulation.

make_mixture_sequential_discrete = function(number.of.insecticides){
  
  if(number.of.insecticides %% 2 != 0){stop("the number.of.insecticides must be even")}
  
  mixture.part.1 = seq(from = 1, to = number.of.insecticides, by = 2)#odd numbered insecticides
  mixture.part.2 = seq(from = 2, to = number.of.insecticides, by = 2)#even numbered insecticides
  mixture.id = seq(from = 1, to = length(mixture.part.1), by = 1)
  
  mixture.df = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  return(mixture.df)
}
