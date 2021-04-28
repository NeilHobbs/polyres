#' @title Make all possible combinations of mixtures
#'
#' @description This function pairs all insecticides with all other insecticides.
#' 
#' @param number.of.insecticides = The total number of insecticides in the simulation.

make_pyrethroid_mixtures = function(number.of.insecticides){
  
 
  mixture.part.1 = rep(1, times = length(number.of.insecticides - 1))
  mixture.part.2 = seq(from = 2, to = number.of.insecticides, by = 1)
  mixture.id = seq(from = 1, to = (number.of.insecticides-1), by = 1)
   
  mixture.df = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  return(mixture.df)
}

