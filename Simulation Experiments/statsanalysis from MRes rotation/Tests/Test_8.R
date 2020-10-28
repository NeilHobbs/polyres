#Test 8: Changing starting resistance intensities
#This is testing without resistance cost, and without dispersal.
#All other parametes set as the default
#
#The Model is working correctly if:
#1. Resistance increases when the corresponding insecticide is deployed.
#2. As there is no dispersal, refugia should see no change in resistance intensity
#3. As there is no cost (and no dispersal); once insecticide withdrawn there should be 
    #no change in resistance intensity.
#4. Resistance does not fall below 0.






Test_8 = run_simulation (number.of.insecticides = 3,
                          exposure.scaling.factor = 10,
                          nsim = 1000, #default
                          minimum.insecticide.resistance.hertitability = 0.1, #default for testing 
                          maximum.insecticide.resistance.hertitability = 0.1, #default
                          minimum.male.insecticide.exposure = 0.5, #default
                          maximum.male.insecticide.exposure = 0.5, #default
                          minimum.female.insecticide.exposure = 0.7, #default
                          maximum.female.insecticide.exposure = 0.7, #default
                          resistance.cost = 0, #No cost associated with having IR
                          starting.treatment.site.intensity = 100, # default
                          starting.refugia.intensity = 200, #default
                          min.intervention.coverage = 0.5, #default
                          max.intervention.coverage = 0.5, #default
                          min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                          max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                          maximum.generations = 500)

#Turning array into data frame format to allow for plotting.

#Obtain refugia intensities for each insecticide:
refugia1 = Test_8["refugia", 1, ] #Refugia, insecticide 1 length 500
refugia2 = Test_8["refugia", 2, ] # Refugia, insecticide 2 length 500
refugia3 = Test_8["refugia", 3, ] #Refugia, insecticide 3 length 500

#Do the same, but for treatment sites
treatment1 = Test_8["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2 = Test_8["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3 = Test_8["treatment", 3, ] #treatment, insecticide 3 length 500

refugia = rep("refugia", times = 1500) # length 1500
treatment = rep("treatment", times = 1500) # length 1500



#Deployment strategy:
current.insecticide = c(rep("1", times = 100), rep("2", times = 100), 
                        rep("3", times = 100), rep("1", times=100), rep("2", times = 100))

deployed = rep(current.insecticide, times = 6) # make vector of deployed insecticide same length
deployed = as.factor(deployed)
generation = rep(seq(1, 500, by = 1), times = 6)
resistance.intensity = c(refugia1, refugia2, refugia3, treatment1, treatment2, treatment3)
location = c(refugia, treatment)
resistance.insecticide = rep(c(rep("1", times = 500), rep("2", times = 500), rep("3", times = 500)), times = 2) # lenth 3000
resistance.insecticide = as.factor(resistance.insecticide)

Test8_DF = data.frame(generation, location, resistance.intensity, resistance.insecticide, deployed)


##Now plot:
ggplot(Test8_DF, aes(x=generation, y = resistance.intensity)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("Changing Starting Intensities") +
  facet_wrap(~location) #Would prefer to have on the same graph






