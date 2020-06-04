#Test_6: Excluding male insecticide exposure
#The Model is working correctly if:
#1. Resistance increases when the corresponding insecticide is deployed.
#2. As there is no dispersal, refugia should see no change in resistance intensity
#3. As there is no cost (and no dispersal); once insecticide withdrawn there should be 
#no change in resistance intensity.
#4. Resistance does not fall below 0.
#5.Increase in resistance should be slower than in Test_1






Test_6A = run_simulation (number.of.insecticides = 3,
                         exposure.scaling.factor = 10,
                         nsim = 1000, #default
                         minimum.insecticide.resistance.hertitability = 0.1, #default for testing 
                         maximum.insecticide.resistance.hertitability = 0.1, #default
                         minimum.male.insecticide.exposure = 0, #default
                         maximum.male.insecticide.exposure = 0, #default
                         minimum.female.insecticide.exposure = 0.7, #default
                         maximum.female.insecticide.exposure = 0.7, #default
                         resistance.cost = 0, #No cost associated with having IR
                         starting.treatment.site.intensity = 0, # default
                         starting.refugia.intensity = 0, #default
                         min.intervention.coverage = 0.5, #default
                         max.intervention.coverage = 0.5, #default
                         min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                         max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                         maximum.generations = 500)
Test_6B = run_simulation (number.of.insecticides = 3,
                         exposure.scaling.factor = 10,
                         nsim = 1000, #default
                         minimum.insecticide.resistance.hertitability = 0.1, #default for testing 
                         maximum.insecticide.resistance.hertitability = 0.1, #default
                         minimum.male.insecticide.exposure = 0.5, #default
                         maximum.male.insecticide.exposure = 0.5, #default
                         minimum.female.insecticide.exposure = 0.7, #default
                         maximum.female.insecticide.exposure = 0.7, #default
                         resistance.cost = 0, #No cost associated with having IR
                         starting.treatment.site.intensity = 0, # default
                         starting.refugia.intensity = 0, #default
                         min.intervention.coverage = 0.5, #default
                         max.intervention.coverage = 0.5, #default
                         min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                         max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                         maximum.generations = 500)
Test_6C = run_simulation (number.of.insecticides = 3,
                         exposure.scaling.factor = 10,
                         nsim = 1000, #default
                         minimum.insecticide.resistance.hertitability = 0.1, #default for testing 
                         maximum.insecticide.resistance.hertitability = 0.1, #default
                         minimum.male.insecticide.exposure = 1, #default
                         maximum.male.insecticide.exposure = 1, #default
                         minimum.female.insecticide.exposure = 0.7, #default
                         maximum.female.insecticide.exposure = 0.7, #default
                         resistance.cost = 0, #No cost associated with having IR
                         starting.treatment.site.intensity = 0, # default
                         starting.refugia.intensity = 0, #default
                         min.intervention.coverage = 0.5, #default
                         max.intervention.coverage = 0.5, #default
                         min.dispersal.rate = 0, #prevent migration [refugia should remain 0]
                         max.dispersal.rate = 0, # prevent migration [refugia should remain 0]
                         maximum.generations = 500)
#Turning array into data frame format to allow for plotting.

#Only do for treatment sites : 6A
treatment1A = Test_6A["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2A = Test_6A["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3A = Test_6A["treatment", 3, ] #treatment, insecticide 3 length 500

#6B
treatment1B = Test_6B["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2B = Test_6B["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3B = Test_6B["treatment", 3, ] #treatment, insecticide 3 length 500

#6C
treatment1C = Test_6C["treatment", 1, ] #treatment, insecticide 1 length 500
treatment2C = Test_6C["treatment", 2, ] # treatment, insecticide 2 length 500
treatment3C = Test_6C["treatment", 3, ] #treatment, insecticide 3 length 500



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




Test6A_DF = data.frame(generation, resistance.intensityA, resistance.insecticide, deployed)
Test6B_DF = data.frame(generation, resistance.intensityB, resistance.insecticide, deployed)
Test6C_DF = data.frame(generation, resistance.intensityC, resistance.insecticide, deployed)


##Now plot:
#Query whether working correctly
T6A =ggplot(Test6A_DF, aes(x=generation, y = resistance.intensityA)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  ylim(-5, 150) +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("A") 
 

T6B = ggplot(Test6B_DF, aes(x=generation, y = resistance.intensityB)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  labs(colour = "Insecticide")+
  ylim(-5, 150) +
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("B") 

T6C = ggplot(Test6C_DF, aes(x=generation, y = resistance.intensityC)) +
  geom_point(aes(colour = resistance.insecticide)) +
  geom_point(aes(y=-5, colour = deployed, x = generation)) +
  theme_classic() +
  ylim(-5, 150) +
  labs(colour = "Insecticide")+
  ylab("Insecticide Resistance Intensity") +
  xlab("Mosquito Generation") +
  ggtitle("C") 

grid.arrange(T6A, T6B, T6C, nrow = 1, top = "Effect of changing male insecticide exposure")



