#This function makes mixture pairs that are discrete: eg. 1,2 ; 3,4 etc; will only work if number.of.insecticides is EVEN.
make_mixture_sequential_discrete = function(number.of.insecticides){
  
  mixture.part.1 = seq(from = 1, to = number.of.insecticides, by = 2)#odd numbered insecticides
  mixture.part.2 = seq(from = 2, to = number.of.insecticides, by = 2)#even numbered insecticides
  mixture.id = seq(from = 1, to = length(mixture.part.1), by = 1)
  
  mixture.df = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  return(mixture.df)
}