#'@title A function that allows for each insecticide in the simulation to have unique insecticide resistance heritabililties
#'
#'@description Allows insecticides to have unique heritabilities and therefore different rates of evolution.
#'
#'@param minimum.insecticide.resistance.heritability = a vector containing the heritabilities
#'@param maximum.insecticide.resistance.heritability = a vector containing the heritabilities
#'@param number.of.insecticides = The number of insecticides in the simulation


allow_unique_heritabilities = function(minimum.insecticide.resistance.heritability,
                                       maximum.insecticide.resistance.heritability,
                                       number.of.insecticides){
  
  
  heritabilities.check = isTRUE(all.equal(target = minimum.insecticide.resistance.heritability, current = maximum.insecticide.resistance.heritability))
  
  if(heritabilities.check == TRUE){
    
    if(length(minimum.insecticide.resistance.heritability) == 1){
      heritabilities = rep(minimum.insecticide.resistance.heritability, times = number.of.insecticides)}else(
    if(length(minimum.insecticide.resistance.heritability) == number.of.insecticides){
      heritabilities = minimum.insecticide.resistance.heritability
    } else(stop("length of the insecticide.resistance.heritability vectors must be 1 or equal to the number of insecticides in the simulation and
               minimum.insecticide.resistance.heritability must equal maximum.insecticide.resistance.heritability")))
    return(heritabilities)
  }
  
  if(length(minimum.insecticide.resistance.heritability) != 1 &
     length(minimum.insecticide.resistance.heritability) != number.of.insecticides){
    
    return(stop("length of the insecticide.resistance.heritability vectors must be 1 or equal to the number of insecticides in the simulation and
               minimum.insecticide.resistance.heritability must equal maximum.insecticide.resistance.heritability"))
  }
  
  if(heritabilities.check == FALSE){
    
    return(stop("length of the insecticide.resistance.heritability vectors must be 1 or equal to the number of insecticides in the simulation and
               minimum.insecticide.resistance.heritability must equal maximum.insecticide.resistance.heritability"))
    
  }
}




# if(length(heritability) == number.of.insecticides){
#   heritability = heritability}else{if(length(heritability) == 1){
#     heritability = rep(heritability, times=number.of.insecticides)}else{
#       stop("heritability must either be a vector with length equal to the number.of.insecticides or a single value")
#     }}