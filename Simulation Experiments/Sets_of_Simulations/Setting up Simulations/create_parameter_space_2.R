#Create the Parameter Space Dataframe for Running the Simulations
library(dplyr)
library(lhs)
#Use only intervention.coverage from 0.5 to 0.9 as below 0.5 was giving predominantly draws. 

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
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.5, 0.9))%>%
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))

#Visually check suitable parameter space coverage - eg no patches with large gaps:
plot(parameter.space)

write.csv(parameter.space, ".//parameter.space.2.csv") #only has the randomly selected values
