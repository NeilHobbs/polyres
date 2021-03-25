#Create the Parameter Space Dataframe for Running the Simulations

df = data.frame(lhs::randomLHS(5000, 6)) #5000 random samples of the 6 input parameters.

parameter.space = df%>%
  dplyr::rename(Heritability = X1)%>%
  dplyr::rename(`Male Insecticide Exposure` = X2)%>%
  dplyr::rename(`Female Insecticide Exposure` = X3)%>%
  dplyr::rename(`Fitness Cost` = X4)%>%
  dplyr::rename(`Intervention Coverage` = X5)%>%
  dplyr::rename(Dispersal = X6)%>%
  dplyr::mutate(Heritability = qunif(Heritability, 0.05, 0.3))%>%
  dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))%>% #
  dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))%>% #Defaults from Ian
  dplyr::mutate(`Fitness Cost` = qunif(`Fitness Cost`, 0.01, 0.2))%>%
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.1, 0.9))%>%
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))

  #Visually check suitable parameter space coverage - eg no patches with large gaps:
plot(parameter.space)


#Strategy effectiveness could be impacted by frequency of deployment decisions (how swiftly insecticide changes can be made)
#and the number of insecticides available.
parameter.space = rbind(parameter.space, parameter.space, parameter.space) #3x as 2, 3, 4 insecticides

`Number of Insecticides` = c(rep(2, 5000), rep(3, 5000), rep(4, 5000))
parameter.space = cbind(parameter.space, `Number of Insecticides`)


`Deployment Interval` = c(rep(5, 15000), rep(10, 15000), rep(20, 15000))#deployment decisions every 6 (IRS), 12 (IRS / LLIN), 24 (LLIN) months
parameter.space=cbind(parameter.space, `Deployment Interval`)

write.csv(parameter.space, ".//parameter.space.csv") #only has the randomly selected values
