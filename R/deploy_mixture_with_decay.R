#' @title Deploy the Insecticidal Mixture
#' 
#' @description Deploys the candidate mixture by adding to the deployment dataframe.
#' 
#' @param candidate.mixture.id = The mixture.id of the candidate mixture to be deployed.
#' @param mixture.df = The dataframe containing all the mixture combinations and IDs.
#' @param deployment.df = The dataframe that holds the deployment information.
#' @param deployment.frequency = The duration of a single insecticidal deployment in generations.

deploy_mixture_with_decay = function(candidate.mixture.id,
                                     mixture.df,
                                     deployment.df,
                                     deployment.frequency,
                                     insecticide.parameters.df){
  
  
  if(is.na(candidate.mixture.id)){
    
    mixture.id =  rep((mixture.df[candidate.mixture.id, 1]), times = deployment.frequency)
    mixture.part.1 = rep((mixture.df[candidate.mixture.id, 2]), times = deployment.frequency)
    mixture.part.2 = rep((mixture.df[candidate.mixture.id, 3]), times = deployment.frequency)
    
    part.1 = mixture.part.1[1]
    part.2 = mixture.part.2[1]
    
    insecticide.efficacy.vector.part.1 = rep(NA, times = deployment.frequency)
    
    insecticide.efficacy.vector.part.2 = rep(NA, times = deployment.frequency)
    
    mixture.to.deploy = data.frame(mixture.id, mixture.part.1, mixture.part.2, insecticide.efficacy.vector.part.1, insecticide.efficacy.vector.part.2)
    
    deployment.df = rbind(deployment.df, mixture.to.deploy)
    
    
  }else{
  
  mixture.id =  rep((mixture.df[candidate.mixture.id, 1]), times = deployment.frequency)
  mixture.part.1 = rep((mixture.df[candidate.mixture.id, 2]), times = deployment.frequency)
  mixture.part.2 = rep((mixture.df[candidate.mixture.id, 3]), times = deployment.frequency)
  
  part.1 = mixture.part.1[1]
  part.2 = mixture.part.2[1]
  
  insecticide.efficacy.vector.part.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[part.1, 2],
                                                                                                             recommended.insecticide.dose = insecticide.parameters.df[part.1, 3],
                                                                                                             threshold.generations = insecticide.parameters.df[part.1, 4],
                                                                                                             base.efficacy.decay.rate = insecticide.parameters.df[part.1, 5],
                                                                                                             rapid.decay.rate = insecticide.parameters.df[part.1, 6],
                                                                                                             deployment.frequency = deployment.frequency)
  
  insecticide.efficacy.vector.part.2 =create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[part.2, 2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df[part.2, 3],
                                                                         threshold.generations = insecticide.parameters.df[part.2, 4],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df[part.2, 5],
                                                                         rapid.decay.rate = insecticide.parameters.df[part.2, 6],
                                                                         deployment.frequency = deployment.frequency)
  
  mixture.to.deploy = data.frame(mixture.id, mixture.part.1, mixture.part.2, insecticide.efficacy.vector.part.1, insecticide.efficacy.vector.part.2)
  
  deployment.df = rbind(deployment.df, mixture.to.deploy)}
  
  return(deployment.df)
  
}
