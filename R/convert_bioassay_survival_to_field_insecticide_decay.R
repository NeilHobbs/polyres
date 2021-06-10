#'@title Comvert bioassay survival to field survival when there is insecticide decay
#'
#' @param bioassay.survival = The bioassay survival at a specified resistance 
#' @param conversion.factor = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#' @param intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The insecticide efficacy of insecticide i at time since deployment τ defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.

convert_bioassay_survival_to_field_insecticide_decay = function(conversion.factor,
                                                                bioassay.survival,
                                                                intercept,
                                                                current.insecticide.efficacy){
  
  
  field.survival = ((conversion.factor * bioassay.survival) + intercept)^current.insecticide.efficacy
  
  #stop field.survival being outside of 0-1 range
  field.survival = ifelse(field.survival > 1, yes = 1, no = field.survival)
  field.survival = ifelse(field.survival < 0, yes = 0, no = field.survival)
  
  return(field.survival)
  
}