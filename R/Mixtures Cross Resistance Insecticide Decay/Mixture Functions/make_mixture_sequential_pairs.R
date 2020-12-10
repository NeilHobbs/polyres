
make_mixture_sequential_pairs = function(number.of.insecticides){
  
  mixture.part.1 = seq(from = 1, to = number.of.insecticides, by = 2)#odd numbered insecticides
  mixture.part.2 = seq(from = 2, to = number.of.insecticides, by = 2)#even numbered insecticides
  mixture.id = paste("Mixture.", LETTERS[1:(number.of.insecticides/2)], sep="")
  
  mixture.df = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  return(mixture.df)

}

make_mixture_sequential_pairs(4) #this function only works if number.of.insecticides is EVEN


#This function makes mixture pairs that are discrete: eg. 1,2 ; 3,4 etc; will only work if number.of.insecticides is EVEN.
make_mixture_sequential_discrete = function(number.of.insecticides){
  
  mixture.part.1 = seq(from = 1, to = number.of.insecticides, by = 2)#odd numbered insecticides
  mixture.part.2 = seq(from = 2, to = number.of.insecticides, by = 2)#even numbered insecticides
  mixture.id = paste("Mixture.", LETTERS[1:(number.of.insecticides/2)], sep="")
  
  mixture.df = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  return(mixture.df)
  
}


make_mixture_sequential_discrete(4)

#1,2; seq(1, 4)
#2,3; c(seq(2, 4), 1)
#3,4;
#4:1

#This function makes mixtures that are continous then loops back: eg. 1,2 ; 2,3 ; 3,4 ; 4,1
make_mixture_sequential_continous = function(number.of.insecticides){
  
  if(number.of.insecticides == 2){
    mixture.part.1 = 1#odd numbered insecticides
    mixture.part.2 = 2#even numbered insecticides
    mixture.id = "Mixture.A"
  }
  else{
  mixture.part.1 = seq(from = 1, to = number.of.insecticides, by = 1)#odd numbered insecticides
  mixture.part.2 = c(seq(from = 2, to = number.of.insecticides, by = 1), 1)#even numbered insecticides
  mixture.id = paste("Mixture.", LETTERS[1:(number.of.insecticides)], sep="")
  }
  
  mixture.df = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  return(mixture.df)
}

make_mixture_sequential_continous(9)
  
  
  
  