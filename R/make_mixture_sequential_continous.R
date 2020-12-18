#' @title Make mixtures with a continous overlap
#' 
#' @description Makes insecticidal mixtures in a way that the is an overlap between each mixture. 
#' 
#' @param number.of.insecticides = The total number of insecticides in the simulation.

make_mixture_sequential_continous = function(number.of.insecticides){
  
  if(number.of.insecticides == 2){
    mixture.part.1 = 1#odd numbered insecticides
    mixture.part.2 = 2#even numbered insecticides
    mixture.id = 1
  }
  else{
    mixture.part.1 = seq(from = 1, to = number.of.insecticides, by = 1)#odd numbered insecticides
    mixture.part.2 = c(seq(from = 2, to = number.of.insecticides, by = 1), 1)#even numbered insecticides
    mixture.id = seq(from = 1, to=length(mixture.part.1), by = 1)
  }
  
  mixture.df = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  return(mixture.df)
}
