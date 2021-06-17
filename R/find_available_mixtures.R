#' @title Find which mixtures are allowed to be deployed
#' 
#' @description This function returns a dataframe of the available insecticide mixtures that can be used
#' for deployment.
#' 
#' @param mixture.df = A dataframe containing all the mixtures included in the simulation
#' @param withdrawn.insecticides = A vector containing the insecticides that are withdrawn from the armory.
#' 
#' @return A dataframe containing the mixtures available for deployment


find_available_mixtures = function(mixture.df,
                                   withdrawn.insecticides){
  
  #Where an insecticide has been withdrawn, set that to NA
mixture.df$mixture.part.1 = ifelse(mixture.df$mixture.part.1 %in% withdrawn.insecticides,
                                   yes = NA,
                                   no = mixture.df$mixture.part.1)
mixture.df$mixture.part.2 = ifelse(mixture.df$mixture.part.2 %in% withdrawn.insecticides, 
                                   yes = NA, 
                                   no = mixture.df$mixture.part.2)
  
#Remove all rows that have any NAs in them
available.mixtures = na.omit(mixture.df)

return(available.mixtures)
}

                                          