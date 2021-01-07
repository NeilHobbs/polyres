#' @title Convert bioassay survival into field based survival
#' 
#' @description A formula to convert the bioassay survival into field
#' survival. Parameter estimation was conducted using linear modelling. See 
#' Parameter Estimates: "Conversion of Bioassay Survival to Field Survival" for
#' details. Briefly, linear modelling was conducted on paired hut trial and bioassay 
#' results to estimate the relationship. Data obtained from Churcher et al 2018.
#' 
#' @param bioassay.survival = The bioassay survival at a specified resistance intensity
#' @param conversion.factor = A regression coefficient to convert bioassay survival to field survival
#' @param intercept = intercept for equation.


#A 15% base survival rate to an insecticide in the field seems plausible,
#variation in insecticide contact/duration of contact; and individual physiological status of mosquitoes.
#E.g. 0 resistance intensity = 15% survival in the field
convert_bioassay_survival_to_field = function(bioassay.survival,
                                              conversion.factor = 0.48,
                                              intercept = 0.15){
  
  field.survival = (conversion.factor * bioassay.survival) + intercept #values obtained from linear model.
  
  #stop field.survival being outside of 0-1 range
  field.survival = ifelse(field.survival > 1, yes = 1, no = field.survival)
  field.survival = ifelse(field.survival < 0, yes = 0, no = field.survival)
  
  return(field.survival)
}
