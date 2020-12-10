

choose_mixture_combinations = function(number.of.mixtures,
                                       potential.mixtures){
  
  mixtures.df = dplyr::sample_n(potential.mixtures,
                                size = number.of.mixtures,
                                replace = FALSE)
  
  #at this stage set the mixture IDs
  mixtures.df$mixture.id = seq(from = 1, to = nrow(mixtures.df), by = 1)
  
  return(mixtures.df)
  
}