#' #' @title Make a matrix to hold the values for the degree of cross selection between each insecticide in the simulation.
#' #' 
#' #' @description This function is used to create a matrix to allow for the incorportation of cross selection in the model. Where it is assumed 
#' #' each insecticide could apply some cross selection on another insecticide. It is possible for cross selection to be both positive and negative
#' #' (antagonistic). All cross selection values between the same insecticides are set as 1. The cross selection values are taken from a uniform
#' #' distribution betweeen the min.cross.selection and the max.cross.selection.
#' #' 
#' #' @param number.of.insecticides This is the number of insecticides in the simulation.
#' #' @param min.cross.selection This is the minimum cross selection value available
#' #' @param max.cross.selection This is the maximum cross selection value



make_cross_selection_matrix = function(number.of.insecticides = 3,
                                  min.cross.selection = -1,
                                  max.cross.selection = 1){

  #Make the matrix based on the size of the number of insecticides
cs.matrix = matrix(nrow = number.of.insecticides,
                  ncol = number.of.insecticides,
                  data = runif((x=number.of.insecticides*number.of.insecticides),
                               min = min.cross.selection,
                               max = max.cross.selection)) #note this creates a matrix of random values.
                                                           #So it is possible for cross.selection from insecticide 1 to 2 to be positive
                                                                          #and cross.selection from insecticide 2 to 1 to be negative.
                                                            #Will have to think how biologically realistic this is.

for(i in 1:number.of.insecticides){ #set cross resistance for 1=0, 2=0 etc as 0. This would prevent any doubling events.
  cs.matrix[[i, i]] = 0
}

return(cs.matrix)
}

