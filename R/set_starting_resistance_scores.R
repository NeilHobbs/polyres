#'@title Set the starting resistance scores for each insecticide in the intervention site and refugia
#'
#'@param sim.array = the array that holds the simulation data
#'@param starting.refugia.resistance.score = a vector, or single value,containing the starting resistance scores for each insecticide in the refugia.
#'@param starting.intervention.resistance.score = a vector, or single value,containing the starting resistance scores for each insecticide in the intervention site
#'@param number.of.insecticides = the number of insecticides in the simulation.

set_starting_resistance_scores = function(sim.array,
                                          starting.refugia.resistance.score,
                                          starting.intervention.resistance.score,
                                          number.of.insecticides){

if(length(starting.refugia.resistance.score) == 1){
  sim.array['refugia', , 1] = starting.refugia.resistance.score

}  else(

  #Set starting resistance intensities (fills in only the first row/generation). The other generations are set to NAs.
  for(i in 1:number.of.insecticides){
    sim.array['refugia', i , 1] = starting.refugia.resistance.score[i]
  })

if(length(starting.intervention.resistance.score) == 1){
  sim.array['treatment', , 1] = starting.intervention.resistance.score
}else(

  #treatment site starting resistance intensity (where the insecticide can be deployed)
  for(i in 1:number.of.insecticides){
    sim.array['treatment', i , 1] = starting.intervention.resistance.score[i]
  })

  return(sim.array)
}

# set_starting_resistance_scores(sim.array = sim.array,
#                                starting.refugia.resistance.score = c(1, 3, 7),
#                                starting.intervention.resistance.score = 5,
#                                number.of.insecticides = 3)
