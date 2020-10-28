#' @title Withdraws all the required insecticides from the arsenal
#' 
#' @param number.of.insecticides = the number of insecticides in the simulation
#' @param current.generation = the generation in the simulation
#' @param withdrawal.threshold = the resistance intensity for withdrawal
#' @param simulation.array = the array that holds the simulation data
#' @param available.vector = a vector containing the insecticides available for deployment
#' @param withdraw.vector = a vector containing the insecticdes withdrawn from deployment

withdrawal_of_insecticides_from_arsenal = function(number.of.insecticides,
                                 current.generation,
                                 withdrawal.threshold,
                                 simulation.array,
                                 available.vector,
                                 withdrawn.vector){
  all.insecticides = seq(1, number.of.insecticides, by = 1)

  for(i in 1:length(all.insecticides)){

    A = if(check_insecticide_withdrawal(insecticide = all.insecticides[i],
                                        current.generation = current.generation,
                                        withdrawal.threshold = withdrawal.threshold,
                                        simulation.array = simulation.array)){
      withdraw_insecticide_from_arsenal(withdrawn.vector = withdrawn.vector,
                                        available.vector = available.vector,
                                        insecticide.to.withdraw = all.insecticides[i])
    } else {list(available.vector, withdrawn.vector)}
    available.vector = A[[1]]
    withdrawn.vector = A[[2]]
  }
  return(A)
}
