# ##Mixtures...:
# 
# #Using this method would assume all insecticides are able to be incorporated with each other as a mixture.
#   #Which is fine I guess if the number of insecticides is small (3-4). But issue would be if number of
#     #available insecticides is large.
# 
# #Need to decide if we only allow a finite number of mixture combinations; or allow everything to be mixed with
#   #everything. 
# 
# #How to make a mixture...
#  #Which insecticides can be paired together
#  #Which insecticides are available
# 
# #mixtures cannot be the same insecticide:
# make_all_possible_mixtures = function(number.of.insecticides){
#   
#   insecticide.vec1 = rep(seq(1, number.of.insecticides, by = 1), times = number.of.insecticides)
#   insecticide.vec2 = sort(rep(seq(1, number.of.insecticides, by = 1), times = number.of.insecticides), decreasing = TRUE)
#   
#   mixture.df = data.frame(insecticide.vec1, insecticide.vec2)
#   
#   mixture.df = mixture.df%>%
#     dplyr::filter(insecticide.vec1 != insecticide.vec2)%>%
#     dplyr::filter(insecticide.vec1 < insecticide.vec2)
#   
# return(mixture.df)
#   
# }
# 
# 
# choose_mixture_combinations = function(number.of.mixtures,
#                                        potential.mixtures){
#   
#   mixtures.df = dplyr::sample_n(potential.mixtures, 
#                        size = number.of.mixtures,
#                        replace = FALSE)
#   
#   return(mixtures.df)
#   
# }
# 
# 
# choose_mixture_combinations(number.of.mixtures = 3,
#                             make_all_possible_mixtures(5))
# 
# 
# 
# 
# #Issue with this method is that it can give some odd pairings. So may not be the most suitable system.
# #Do we just say that M=1,2 ; M=3,4; M=5,6 etc. 
# #Having everything paired with everything makes no realistic sense. 
#  #But having sequential pairs means that only even number of insecticides can be compared (2, 4).
# 
# #Note: Mixtures in Sequence or Mixtures in Rotation.
# 
# 
# 
# 
# 
# 
#  