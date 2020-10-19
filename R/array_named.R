#' @title create an array with named dimensions
#' 
#' @description 
#' an array specified by an number of dimensions with e.g. (name1=c('a','b','c), name2=c('x','y')) 
#' 
#' @param ... named dimensions to create in array e.g. (name1=c('a','b','c), name2=c('x','y')) 
#' @author Andy South
#' 
#' @examples 
#' array_named( sex = c('f','m') ) 
#' 
#' sexvar <- c('f','m')
#' array_named( sex = sexvar ) 
#' 
#' array_named( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), 
#'              niche1=c('0','a','A'), niche2=c('0','b','B') )
#' array_named( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), 
#'              locus2 = c('SS2','RS2','RR2'), 
#'              niche1 = c('0','a','A'), niche2 = c('0','a','A') )#' 
#' @return array
#' @export



array_named = function(...){
  array(NA, dim = lengths(list(...)), dimnames = list(...))
}

#Function courtesy of Andy South
#This function is called in the the create_starting_array function


