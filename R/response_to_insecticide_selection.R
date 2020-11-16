#' @title Convert insecticide selection into selection response, using the Breeder's Equation
#'
#' @import stats
#' @importFrom stats runif
#' @name runif
#'
#'@param exposure.scaling.factor This factor converts exposure to selection differential. This requires empirically setting.
#'@param nsim number of simulations of the runif functions.
#'@param minimum.insecticide.resistance.heritability The minimum heritability of insecticide resistance (set at 0.05)
#'@param maximum.insecticide.resistance.heritability The maximum heritability of insecticide resistance (set at 0.3)
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
#' minimum.insecticide.resistance.heritability = 0.05, 
#' maximum.insecticide.resistance.heritability = 0.3,
#' minimum.male.insecticide.exposure = 0,
#' maximum.male.insecticide.exposure = 1, 
#' minimum.female.insecticide.exposure = 0.4, 
#' maximum.female.insecticide.exposure = 0.9)

response_to_insecticide_selection = function(exposure.scaling.factor = 10,
                                             nsim = 1000, 
                                             minimum.insecticide.resistance.heritability = 0.05, 
                                             maximum.insecticide.resistance.heritability = 0.30,
                                             minimum.male.insecticide.exposure = 0,
                                             maximum.male.insecticide.exposure = 1, 
                                             minimum.female.insecticide.exposure = 0.4, 
                                             maximum.female.insecticide.exposure = 0.9){
  
  #Error Messages to prevent incorrect parameter values being included.
  if(0 > minimum.insecticide.resistance.heritability |minimum.insecticide.resistance.heritability > 1){stop("minimum.insecticide.resistance.heritability must be between 0 and 1")}
  if(0 > maximum.insecticide.resistance.heritability |maximum.insecticide.resistance.heritability > 1){stop("maximum.insecticide.resistance.heritability must be between 0 and 1")}
  if(minimum.insecticide.resistance.heritability > maximum.insecticide.resistance.heritability){stop("minimum.insecticide.resistance.heritability is greater than maximum.insecticide.resistance.heritability")}

  if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
  if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}

  if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
  if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}

  
  
  insecticide.resistance.heritability = runif(nsim, 
                                               min = minimum.insecticide.resistance.heritability, 
                                               max = maximum.insecticide.resistance.heritability)
  
  male.insecticide.exposure.proportion = runif(nsim,
                                               min = minimum.male.insecticide.exposure, 
                                               max = maximum.male.insecticide.exposure)
  
  female.insecticide.exposure.proportion= runif(nsim, 
                                                min = minimum.female.insecticide.exposure, 
                                                max = maximum.female.insecticide.exposure)
  #Breeders Equation
 
  resistance.selection.reponse = exposure.scaling.factor*((insecticide.resistance.heritability) * female.insecticide.exposure.proportion * (1 + male.insecticide.exposure.proportion)/2)
  
  return(resistance.selection.reponse) ##Return insecticide selection response as a vector
}

#This is equation 4 in the MS


