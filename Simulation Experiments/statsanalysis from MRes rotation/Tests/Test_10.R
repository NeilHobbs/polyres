#Test 10: 100% Coverage, Changing Dispersal
#This is testing without resistance cost, and without dispersal.
#All other parametes set as the default
#
#The Model is working correctly if:
#1. Resistance increases when the corresponding insecticide is deployed.
#2. As there is no dispersal, refugia should see no change in resistance intensity
#3.As Coverage is 100%, then all areas are treated (no refugia) - all three runs should be identical
#4. Resistance does not fall below 0.



Test_10A = run_simulation (number.of.insecticides = 3,
                          exposure.scaling.factor = 10,
                          nsim = 1000, #default
                          minimum.insecticide.resistance.heritability = 0.1, #default for testing 
                          maximum.insecticide.resistance.heritability = 0.1, #default
                          minimum.male.insecticide.exposure = 0.5, #default
                          maximum.male.insecticide.exposure = 0.5, #default
                          minimum.female.insecticide.exposure =0.7, #default
                          maximum.female.insecticide.exposure = 0.7, #default
                          resistance.cost = 0, #No cost associated with having IR
                          starting.treatment.site.intensity = 0, # default
                          starting.refugia.intensity = 0, #default
                          min.intervention.coverage = 1, #default
                          max.intervention.coverage = 1, #default
                          min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                          max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                          maximum.generations = 500)
Test_10B = run_simulation (number.of.insecticides = 3,
                          exposure.scaling.factor = 10,
                          nsim = 1000, #default
                          minimum.insecticide.resistance.heritability = 0.1, #default for testing 
                          maximum.insecticide.resistance.heritability = 0.1, #default
                          minimum.male.insecticide.exposure = 0.5, #default
                          maximum.male.insecticide.exposure = 0.5, #default
                          minimum.female.insecticide.exposure = 0.7, #default
                          maximum.female.insecticide.exposure = 0.7, #default
                          resistance.cost = 0, #No cost associated with having IR
                          starting.treatment.site.intensity = 0, # default
                          starting.refugia.intensity = 0, #default
                          min.intervention.coverage = 1, #default
                          max.intervention.coverage = 1, #default
                          min.dispersal.rate = 0.5, #prevent migration [refugia should remain 0]
                          max.dispersal.rate = 0.5, # prevent migration [refugia should remain 0]
                          maximum.generations = 500)
Test_10C = run_simulation (number.of.insecticides = 3,
                          exposure.scaling.factor = 10,
                          nsim = 1000, #default
                          minimum.insecticide.resistance.heritability = 0.1, #default for testing 
                          maximum.insecticide.resistance.heritability = 0.1, #default
                          minimum.male.insecticide.exposure = 0.5, #default
                          maximum.male.insecticide.exposure = 0.5, #default
                          minimum.female.insecticide.exposure = 0.7, #default
                          maximum.female.insecticide.exposure = 0.7, #default
                          resistance.cost = 0, #No cost associated with having IR
                          starting.treatment.site.intensity = 0, # default
                          starting.refugia.intensity = 0, #default
                          min.intervention.coverage = 1, #default
                          max.intervention.coverage = 1, #default
                          min.dispersal.rate = 1, #prevent migration [refugia should remain 0]
                          max.dispersal.rate = 1, # prevent migration [refugia should remain 0]
                          maximum.generations = 500)
#Turning array into data frame format to allow for plotting.

#Only do for treatment sites : 7A
treatment1A = Test_10A["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2A = Test_10A["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3A = Test_10A["treatment", 3, ] #treatment, insecticide 3 length 500

refugia1A = Test_10A["refugia", 1, ] #refugia, insecticide 1 length 500
refugia2A = Test_10A["refugia", 2, ] # refugia, insecticide 2 length 500
refugia3A = Test_10A["refugia", 3, ] #refugia, insecticide 3 length 500

#7B
treatment1B = Test_10B["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2B = Test_10B["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3B = Test_10B["treatment", 3, ] #treatment, insecticide 3 length 500


refugia1B = Test_10B["refugia", 1, ] #refugia, insecticide 1 length 500
refugia2B = Test_10B["refugia", 2, ] # refugia, insecticide 2 length 500
refugia3B = Test_10B["refugia", 3, ] #refugia, insecticide 3 length 500
#7C
treatment1C = Test_10C["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2C = Test_10C["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3C = Test_10C["treatment", 3, ] #treatment, insecticide 3 length 500

refugia1C = Test_10C["refugia", 1, ] #refugia, insecticide 1 length 500
refugia2C = Test_10C["refugia", 2, ] # refugia, insecticide 2 length 500
refugia3C = Test_10C["refugia", 3, ] #refugia, insecticide 3 length 500

#Deployment strategy:
current.insecticide = c(rep("1", times = 100), rep("2", times = 100), 
                        rep("3", times = 100), rep("1", times=100), rep("2", times = 100))

deployed = rep(current.insecticide, times = 6) # make vector of deployed insecticide same length
deployed = as.factor(deployed)
generation = rep(seq(1, 500, by = 1), times = 6)
resistance.intensityA = c(treatment1A, treatment2A, treatment3A, refugia1A, refugia2A, refugia3A)
resistance.intensityB = c(treatment1B, treatment2B, treatment3B, refugia1B, refugia2B, refugia3B)
resistance.intensityC = c(treatment1C, treatment2C, treatment3C, refugia1C, refugia2C, refugia3C)
resistance.insecticide = rep(c(rep("1", times = 500), rep("2", times = 500), rep("3", times = 500)), times = 2) 
resistance.insecticide = as.factor(resistance.insecticide)
site = c(rep("treatment", times =1500), rep("refugia", times = 1500))



Test10A_DF = data.frame(generation, resistance.intensityA, resistance.insecticide, deployed, site)
Test10B_DF = data.frame(generation, resistance.intensityB, resistance.insecticide, deployed, site)
Test10C_DF = data.frame(generation, resistance.intensityC, resistance.insecticide, deployed, site)


##Now plot:
#Query whether working correctly
T10A =ggplot(Test10A_DF, aes(x=generation, y = resistance.intensityA)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  ylim(-5, 150) +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("A")+
  facet_wrap(~site)


T10B = ggplot(Test10B_DF, aes(x=generation, y = resistance.intensityB)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  labs(colour = "Insecticide")+
  ylim(-5, 150) +
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("B") +
  facet_wrap(~site)

T10C = ggplot(Test10C_DF, aes(x=generation, y = resistance.intensityC)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  ylim(-5, 150) +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("C")+
  facet_wrap(~site)

grid.arrange(T10A, T10B, T10C, nrow = 1, top = "Effect of 100% Coverage on Dispersal")



