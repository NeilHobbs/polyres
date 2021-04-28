#' @title Choose a subset of the total mixtures
#' 
#' @description This function selects a subset of the total mixtures that have been created. This is advantageous as
#' allowing for all insecticides to be mixed (paired) with all other insecticides is likely to be an unrealistic scenario.
#' However by randomly sampling a master dataframe of mixtures, an element of realism is implemented, as there will be random 
#' samples which result in clusters of similar mixtures (which would be expectedd from similarities in residual decay rates and 
#' ease of making mixtures out of particular insecticides).
#' 
#' @param number.of.mixtures = The total number of mixtures that are allowed to be made for use in the simulations.
#' @param potential.mixtures = A master dataframe of mixtures that are to be sampled from. 
#' 
#' @import  dplyr
#' @importFrom dplyr sample_n


choose_mixture_combinations = function(number.of.mixtures,
                                       potential.mixtures){
  
  mixtures.df = dplyr::sample_n(potential.mixtures,
                                size = number.of.mixtures,
                                replace = FALSE)
  
  #at this stage set the mixture IDs
  mixtures.df$mixture.id = seq(from = 1, to = nrow(mixtures.df), by = 1)
  
  return(mixtures.df)
  
}



