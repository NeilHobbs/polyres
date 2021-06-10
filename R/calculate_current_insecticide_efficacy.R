#' @title Calculate the current insecticidal killing efficacy against a fully susceptible mosquito.
#'
#' @param generations.since.deployment = the time in generations since the insecticide was last deployed.
#' @param threshold.generations = the time (in generations) after which there is a change in the insecticide effective dosage decay rate; which can be due to nets becoming worn to the point mosquitoes are able to avoid the insecticide.
#' @param initial.insecticide.efficacy = The efficacy of the applied insecticide concentration of the insecticide against a fully susceptible mosquito
#' @param base.efficacy.decay.rate = The efficacy decay rate during the first generations during which there is expected to be a slow decay in insecticidal efficacy
#' @param rapid.decay.rate = Decay rate of insecticidal efficacy decay after the threshold.generations is exceeded, where the insecticidal product is expected to undergo a rapid change in efficacy.


calculate_current_insecticide_efficacy = function(generations.since.deployment,
                                                  threshold.generations,
                                                  initial.insecticide.efficacy,
                                                  base.efficacy.decay.rate,
                                                  rapid.decay.rate){

  #Warnings/Errors::
  if(base.efficacy.decay.rate < 0){warning("base.efficacy.decay.rate is less than 0. Insecticide efficacy increases over time.")}
  if(rapid.decay.rate < 0){warning("rapid.decay.rate is less than 0. Insecticide efficacy increases over time.")}
  if(initial.insecticide.efficacy < 0){stop("initial.insecticide.efficacy cannot be less than 0")}
  
  if(generations.since.deployment <= threshold.generations){

    current.insecticide.efficacy = initial.insecticide.efficacy * exp(-generations.since.deployment * base.efficacy.decay.rate)
  }

  if(threshold.generations < generations.since.deployment){

   first.decay =  initial.insecticide.efficacy * exp(-threshold.generations * base.efficacy.decay.rate)

   current.insecticide.efficacy = first.decay * exp(-(generations.since.deployment-threshold.generations)^2 * rapid.decay.rate)

  }
  #Prevent insecticide efficacy being less than zero. 
  current.insecticide.efficacy = ifelse(current.insecticide.efficacy < 0,
                                        yes = 0,
                                        no = current.insecticide.efficacy)
  return(current.insecticide.efficacy)
}






