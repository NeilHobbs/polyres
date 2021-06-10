#'@title Function to extend the vector that contains the insecticide efficacy information
#'
#'@param previous.efficacy.vector = The vector that contains the insecticide efficacy of previously deployed insecticides
#'@param new.efficacy.vector = The vector that contains the insecticide efficacy values of the insecticide to be deployed

lengthen_insecticide_efficacy_vector = function(previous.efficacy.vector,
                                                new.efficacy.vector){

  insecticide.efficacy.vector = c(previous.efficacy.vector, new.efficacy.vector)

  return(insecticide.efficacy.vector)
}
