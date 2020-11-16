#Test_6: Impact of female insecticide exposure
#The Model is working correctly if:
#1. Resistance increases when the corresponding insecticide is deployed.
#2. As there is no dispersal, refugia should see no change in resistance intensity
#3. As there is no cost (and no dispersal); once insecticide withdrawn there should be 
#no change in resistance intensity.
#4. Resistance does not fall below 0.
#5.Increase in resistance should be slower than in Test_1

Test_7A = run_simulation (number.of.insecticides = 3,
                          exposure.scaling.factor = 10,
                          nsim = 1000, #default
                          minimum.insecticide.resistance.heritability = 0.1, #default for testing 
                          maximum.insecticide.resistance.heritability = 0.1, #default
                          minimum.male.insecticide.exposure = 0.5, #default
                          maximum.male.insecticide.exposure = 0.5, #default
                          minimum.female.insecticide.exposure = 0, #default
                          maximum.female.insecticide.exposure = 0, #default
                          resistance.cost = 0, #No cost associated with having IR
                          starting.treatment.site.intensity = 0, # default
                          starting.refugia.intensity = 0, #default
                          min.intervention.coverage = 0.5, #default
                          max.intervention.coverage = 0.5, #default
                          min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                          max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                          maximum.generations = 500)
Test_7B = run_simulation (number.of.insecticides = 3,
                          exposure.scaling.factor = 10,
                          nsim = 1000, #default
                          minimum.insecticide.resistance.heritability = 0.1, #default for testing 
                          maximum.insecticide.resistance.heritability = 0.1, #default
                          minimum.male.insecticide.exposure = 0.5, #default
                          maximum.male.insecticide.exposure = 0.5, #default
                          minimum.female.insecticide.exposure = 0.5, #default
                          maximum.female.insecticide.exposure = 0.5, #default
                          resistance.cost = 0, #No cost associated with having IR
                          starting.treatment.site.intensity = 0, # default
                          starting.refugia.intensity = 0, #default
                          min.intervention.coverage = 0.5, #default
                          max.intervention.coverage = 0.5, #default
                          min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                          max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                          maximum.generations = 500)
Test_7C = run_simulation (number.of.insecticides = 3,
                          exposure.scaling.factor = 10,
                          nsim = 1000, #default
                          minimum.insecticide.resistance.heritability = 0.1, #default for testing 
                          maximum.insecticide.resistance.heritability = 0.1, #default
                          minimum.male.insecticide.exposure = 0.5, #default
                          maximum.male.insecticide.exposure = 0.5, #default
                          minimum.female.insecticide.exposure = 1, #default
                          maximum.female.insecticide.exposure = 1, #default
                          resistance.cost = 0, #No cost associated with having IR
                          starting.treatment.site.intensity = 0, # default
                          starting.refugia.intensity = 0, #default
                          min.intervention.coverage = 0.5, #default
                          max.intervention.coverage = 0.5, #default
                          min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                          max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                          maximum.generations = 500)
#Turning array into data frame format to allow for plotting.

#Only do for treatment sites : 7A
treatment1A = Test_7A["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2A = Test_7A["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3A = Test_7A["treatment", 3, ] #treatment, insecticide 3 length 500

#7B
treatment1B = Test_7B["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2B = Test_7B["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3B = Test_7B["treatment", 3, ] #treatment, insecticide 3 length 500

#7C
treatment1C = Test_7C["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2C = Test_7C["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3C = Test_7C["treatment", 3, ] #treatment, insecticide 3 length 500



#Deployment strategy:
current.insecticide = c(rep("1", times = 100), rep("2", times = 100), 
                        rep("3", times = 100), rep("1", times=100), rep("2", times = 100))

deployed = rep(current.insecticide, times = 3) # make vector of deployed insecticide same length
deployed = as.factor(deployed)
generation = rep(seq(1, 500, by = 1), times = 3)
resistance.intensityA = c(treatment1A, treatment2A, treatment3A)
resistance.intensityB = c(treatment1B, treatment2B, treatment3B)
resistance.intensityC = c(treatment1C, treatment2C, treatment3C)
resistance.insecticide = rep(c(rep("1", times = 500), rep("2", times = 500), rep("3", times = 500)), times = 1) 
resistance.insecticide = as.factor(resistance.insecticide)




Test7A_DF = data.frame(generation, resistance.intensityA, resistance.insecticide, deployed)
Test7B_DF = data.frame(generation, resistance.intensityB, resistance.insecticide, deployed)
Test7C_DF = data.frame(generation, resistance.intensityC, resistance.insecticide, deployed)


##Now plot:
#Query whether working correctly
T7A =ggplot(Test7A_DF, aes(x=generation, y = resistance.intensityA)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  ylim(-5, 160) +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("A") 


T7B = ggplot(Test7B_DF, aes(x=generation, y = resistance.intensityB)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  labs(colour = "Insecticide")+
  ylim(-5, 160) +
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("B") 

T7C = ggplot(Test7C_DF, aes(x=generation, y = resistance.intensityC)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  ylim(-5, 160) +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("C") 

grid.arrange(T7A, T7B, T7C, nrow = 1, top = "Effect of changing Female insecticide exposure")



