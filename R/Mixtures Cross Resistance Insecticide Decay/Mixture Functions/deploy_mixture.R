deploy_mixture= function(candidate.mixture.id,
                         mixture.df,
                         deployment.df,
                         freq.deployment){
  
  mixture.id =  rep((mixture.df[candidate.mixture.id, 1]), times = freq.deployment)
  mixture.part.1 = rep((mixture.df[candidate.mixture.id, 2]), times = freq.deployment)
  mixture.part.2 = rep((mixture.df[candidate.mixture.id, 3]), times = freq.deployment)
  
  mixture.to.deploy = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  deployment.df = rbind(deployment.df, mixture.to.deploy)
  
  return(deployment.df)
  
}