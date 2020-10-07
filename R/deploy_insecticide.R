#Function which deploys for the for a specified timeframe (in generations):

#' @param insecticide.to.deploy = The value of the insecticide to be deployed
#' @param deployment.frequency = The number of generations between each insecticide deployment [5 generations is 6months, 10 generations is 12months]
#' @param deployment.vector = The vector which holds to sequence of insecticides that have been deployed,

deploy_insecticide = function(insecticide.to.deploy,
                              deployment.frequency,
                              deployment.vector){

  currrent.insecticide = c(deployment.vector, rep(insecticide.to.deploy, times = deployment.frequency))

}
