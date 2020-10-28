#Test_5; only tracking 1 insecticide, with 3 still deployed. 
#The Model is working correctly if:
#1. Resistance increases when the corresponding insecticide is deployed (but at a lower rate
#than when no cost was present (Test_1). The peaks should be lower.
#2. As there is no dispersal, refugia should see no change in resistance intensity
#3. As there is no cost; once insecticide withdrawn there should be no decline in resistance intensity.
#4. Resistance does not fall below 0.

Test_5 = run_simulation (number.of.insecticides = 1,
                         exposure.scaling.factor = 10,
                         nsim = 1000, #default
                         minimum.insecticide.resistance.hertitability = 0.1, #default 
                         maximum.insecticide.resistance.hertitability = 0.1, #default
                         minimum.male.insecticide.exposure = 0.5, #default
                         maximum.male.insecticide.exposure = 0.5, #default
                         minimum.female.insecticide.exposure = 0.7, #default
                         maximum.female.insecticide.exposure = 0.7, #default
                         resistance.cost = 0, #Cost associated with having IR
                         starting.treatment.site.intensity = 0, # default
                         starting.refugia.intensity = 0, #default
                         min.intervention.coverage = 0.5, #default
                         max.intervention.coverage = 0.5, #default
                         min.dispersal.rate = 1, #
                         max.dispersal.rate = 1, # 
                         maximum.generations = 500)

#Turning array into data frame format to allow for plotting.

#Obtain refugia intensities for each insecticide:
refugia1 = Test_5["refugia", 1, ] #Refugia, insecticide 1 length 500
refugia2 = Test_5["refugia", 2, ] # Subscript out of bounds
refugia3 = Test_5["refugia", 3, ] #Subscript out of bounds

#Do the same, but for treatment sites
treatment1 = Test_5["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2 = Test_5["treatment", 2, ] # subscript out of bounds
treatment3 = Test_5["treatment", 3, ] #subscript out of bounds

refugia = rep("refugia", times = 500) # length 500
treatment = rep("treatment", times = 500) # length 500



#Deployment strategy:
current.insecticide = c(rep("1", times = 100), rep("2", times = 100), 
                        rep("3", times = 100), rep("1", times=100), rep("2", times = 100))

deployed = rep(current.insecticide, times = 2) # make vector of deployed insecticide same length
deployed = as.factor(deployed)
generation = rep(seq(1, 500, by = 1), times = 2)
resistance.intensity = c(refugia1, treatment1)
location = c(refugia, treatment)
resistance.insecticide = rep(c(rep("1", times = 500)), times = 2)# lenth 1000
resistance.insecticide = as.factor(resistance.insecticide)

Test5_DF = data.frame(generation, location, resistance.intensity, resistance.insecticide, deployed)

Test5_DF = Test5_DF%>%
  mutate(deployed = as.character(deployed))%>%
  mutate(resistance.insecticide = as.character(resistance.insecticide))


##Now plot:
ggplot(Test5_DF, aes(x=generation, y = resistance.intensity)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("One Insecticide Tracked, Three Insecticides Sequentially Deployed") +
  facet_wrap(~location) #Would prefer to have on the same graph

