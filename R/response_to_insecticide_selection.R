#' Function to turn insecticide selection into selection response, using the Breeder's Equation
#'
#' @import stats
#' @importFrom stats runif
#' @name runif
#'
#'@param exposure.scaling.factor This factor converts exposure to selection differential. This requires empirically setting.
#'@param nsim number of simulations of the runif functions.
#'@param minimum.insecticide.resistance.hertitability The minimum heritability of insecticide resistance (set at 0.05)
#'@param maximum.insecticide.resistance.hertitability The maximum heritability of insecticide resistance (set at 0.3)
#'@param minimum.male.insecticide.exposure Proportion of males exposed to insecticide as a proportion of females. Set at 0.
#'@param maximum.male.insecticide.exposure  Proportion of males exposed to insecticide as a proportion of females. Set at 1.
#'@param minimum.female.insecticide.exposure Mininum proportion of females exposed to insecticide (set at 0.4)
#'@param maximum.female.insecticide.exposure Maximum proportion of females exposed to insecticide (set at 0.9)
#'
#'@return resistance.selection.response
#'
#'@example response_to_insecticide_selection(
#' exposure.scaling.factor = 10,
#' nsim = 1000, 
#' minimum.insecticide.resistance.hertitability = 0.05, 
#' maximum.insecticide.resistance.hertitability = 0.3,
#' minimum.male.insecticide.exposure = 0,
#' maximum.male.insecticide.exposure = 1, 
#' minimum.female.insecticide.exposure = 0.4, 
#' maximum.female.insecticide.exposure = 0.9)

response_to_insecticide_selection = function(exposure.scaling.factor,
                                             nsim, 
                                             minimum.insecticide.resistance.hertitability, 
                                             maximum.insecticide.resistance.hertitability,
                                             minimum.male.insecticide.exposure,
                                             maximum.male.insecticide.exposure, 
                                             minimum.female.insecticide.exposure, 
                                             maximum.female.insecticide.exposure){
  
  insecticide.resistance.hertitability = runif(nsim, 
                                               min = minimum.insecticide.resistance.hertitability, 
                                               max=maximum.insecticide.resistance.hertitability)
  
  male.insecticide.exposure.proportion = runif(nsim,
                                               min = minimum.male.insecticide.exposure, 
                                               max=maximum.male.insecticide.exposure)
  
  female.insecticide.exposure.proportion= runif(nsim, 
                                                min = minimum.female.insecticide.exposure, 
                                                max=maximum.female.insecticide.exposure)
  
  resistance.selection.reponse = exposure.scaling.factor*((insecticide.resistance.hertitability) * female.insecticide.exposure.proportion * (1 + male.insecticide.exposure.proportion)/2) #Breeders Equation
  
  return(resistance.selection.reponse) ##Return insecticide selection response as a data frame
}



