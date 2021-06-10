#'@title Calculate the initial insecticide efficacy when the insecticide is deployed in a mixture formulation
#'
#'@param mixture.dose = The deployed insecticide dosage when the insecticide is deployed in a mixture formulation.
#'@param recommended.insecticide.dose = The manufacturer’s recommended dosage for the insecticide when deployed as a single insecticide formulation.

calculate_initial_efficacy_from_insecticide_concentration_mixtures = function(mixture.dose,
                                                                              recommended.insecticide.dose){


  initial.insecticide.efficacy = mixture.dose / recommended.insecticide.dose
if(initial.insecticide.efficacy < 0)stop("Error: Insecticide efficacy cannot be negative")
  
  return(initial.insecticide.efficacy)
}
