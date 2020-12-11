#' @title Deploy the Insecticidal Mixture
#' 
#' @description Deploys the candidate mixture by adding to the deployment dataframe.
#' 
#' @param candidate.mixture.id = The mixture.id of the candidate mixture to be deployed.
#' @param mixture.df = The dataframe containing all the mixture combinations and IDs.
#' @param deployment.df = The dataframe that holds the deployment information.
#' @param deployment.frequency = The duration of a single insecticidal deployment in generations.

deploy_mixture= function(candidate.mixture.id,
                         mixture.df,
                         deployment.df,
                         deployment.frequency){
  
  mixture.id =  rep((mixture.df[candidate.mixture.id, 1]), times = deployment.frequency)
  mixture.part.1 = rep((mixture.df[candidate.mixture.id, 2]), times = deployment.frequency)
  mixture.part.2 = rep((mixture.df[candidate.mixture.id, 3]), times = deployment.frequency)
  
  mixture.to.deploy = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  deployment.df = rbind(deployment.df, mixture.to.deploy)
  
  return(deployment.df)
  
}
