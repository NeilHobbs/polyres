#' @title Make all potential combinations of insecticides in mixture pairs.
#' 
#' @description This function makes all the potential combination of mixtures assuming every insecticide 
#' can be mixed with every other insecticide. It should be noted that in reality, there are likely to be 
#' chemical reasons as to why a particular insecticide cannot be mixed with another. THere is also practical 
#' reasons why two insecticides should not be mixed; such as cross resistance or different residual decay rate.
#' 
#' @import magrittr dplyr
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' 
#' @param number.of.insecticides = The total number of insecticides available in the simulation.
#' 
#' @return A two column dataframe, with each column containing one insecticide of the mixture.
#' 

make_all_possible_mixtures = function(number.of.insecticides){
  
  mixture.part.1 = rep(seq(1, number.of.insecticides, by = 1), times = number.of.insecticides)
  mixture.part.2 = sort(rep(seq(1, number.of.insecticides, by = 1), times = number.of.insecticides), decreasing = TRUE)
  
  
  mixture.df = data.frame(mixture.part.1, mixture.part.2)
  
  mixture.df = mixture.df%>%
    dplyr::filter(mixture.part.1 != mixture.part.2)%>%
    dplyr::filter(mixture.part.1 < mixture.part.2)
  
  mixture.id = seq(1, nrow(mixture.df), by = 1)
  
  mixture.df = data.frame(mixture.id, mixture.df)
  
  return(mixture.df)
  
}
