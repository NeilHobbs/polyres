#Create the Parameter Space Dataframe for Running the Simulations
library(dplyr)
library(lhs)
#Use only intervention.coverage 1.
#Use dispersal rate = 0
#Use fitness cost = 0

df = data.frame(lhs::randomLHS(5000, 6)) #5000 random samples of the 3 variable input parameters.

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
  dplyr::mutate(`Fitness Cost` = 0)%>%
  dplyr::mutate(`Intervention Coverage` = 1)%>%
  dplyr::mutate(Dispersal = 0)

#Visually check suitable parameter space coverage - eg no patches with large gaps:
plot(parameter.space)

#write.csv(parameter.space, ".//parameter.space.3.csv") #only has the randomly selected values
