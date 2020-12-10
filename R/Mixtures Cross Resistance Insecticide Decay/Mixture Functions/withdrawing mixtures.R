#with an insecticide when in mixture

mixture.df = make_mixture_sequential_continous(4)

#Find all insecticides that exceed withdrawal threshold

colnames(mixture.df)
withdrawn = c()

withdraw_insecticide_from_arsenal = function(withdrawn.vector,
                                             available.vector,
                                             insecticide.to.withdraw){
  
  available.vector.updated = available.vector[!available.vector %in% insecticide.to.withdraw]
  withdrawn.vector.updated = c(withdrawn.vector, insecticide.to.withdraw)
  
  return(list(available.vector.updated, withdrawn.vector.updated)) #[[1]]available vector ; [[2]]withdrawn vector
  
}

withdraw_insecticide_from_arsenal(withdrawn.vector = withdrawn,
                                  available.vector = mixture.df[2],
                                  insecticide.to.withdraw = 1)
