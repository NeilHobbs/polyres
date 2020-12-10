


make_pyrethroid_mixtures = function(number.of.insecticides){
  
 
  mixture.part.1 = rep(1, times = length(non.pyrethroid))
  mixture.part.2 = seq(from = 2, to = number.of.insecticides, by = 1)
  mixture.id = seq(from = 1, to = length(mixture.part.1), by = 1)
   
  mixture.df = data.frame(mixture.part.1, mixture.part.2)
  
  return(mixture.df)
}

