##Mixtures...:

#Using this method would assume all insecticides are able to be incorporated with each other as a mixture.
  #Which is fine I guess if the number of insecticides is small (3-4). But issue would be if number of 
    #available insecticides is large. 

create_insecticidal_mixture = function(available.insecticides = available.vector){
  
  M_1 = min(available.insecticides)
  
  available.insecticides_R2 = available.insecticides[!available.insecticides %in% M_1]
  
    M_2 = min(available.insecticides_R2)
  
  mixture = c(M_1, M_2)
  
  return(mixture)
}

#Would need to be prevented from being passed an initial vector of lenght=1



create_insecticidal_mixture(c(1,7,8))
