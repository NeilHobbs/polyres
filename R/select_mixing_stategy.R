#' @title Select how the insecticides are combined to make mixtures.
#' 
#' @param mixture.strategy = The type of mixture strategy.
#' @param number.of.insecticides = The number of insecticides in the simulation.


select_mixing_stategy = function(mixture.strategy,
                                 number.of.insecticides){
  
  if(mixture.strategy == "mix.sequential.discrete"){
    mixture.df = make_mixture_sequential_discrete(number.of.insecticides = number.of.insecticides)
  }
  if(mixture.strategy == "mix.sequential.continous"){
    mixture.df = make_mixture_sequential_continous(number.of.insecticides = number.of.insecticides)
  }
  
  if(mixture.strategy == "random.mixtures"){
    
    mixture.df = choose_mixture_combinations(number.of.mixtures = number.of.insecticides,#make it so there is the same number of mixtures as there is number of insecticides
                                             potential.mixtures = make_all_possible_mixtures(number.of.insecticides = number.of.insecticides))
    
  }
  if(mixture.strategy == "pyrethroid.plus"){
    mixture.df = make_pyrethroid_mixtures(number.of.insecticides = number.of.insecticides)
  }
  
return(mixture.df)
  
}