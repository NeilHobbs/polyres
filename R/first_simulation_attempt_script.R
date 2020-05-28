#run simulation with default parameters, except resistance.cost which has no default and dispersal.
#Set Dispersal to Zero.

#Note: this is using the current run_simulation function that has a prespecified
#insecticide sequence. 
simulation1 = run_simulation(number.of.insecticides = 3, #same as number of insecticides in sequence.
                             exposure.scaling.factor = 10,
                             nsim = 1000,
                             minimum.insecticide.resistance.hertitability = 0.05,
                             maximum.insecticide.resistance.hertitability = 0.30,
                             minimum.male.insecticide.exposure = 0,
                             maximum.male.insecticide.exposure = 1,
                             minimum.female.insecticide.exposure = 0.4,
                             maximum.female.insecticide.exposure = 0.9,
                             resistance.cost = 0.1, #Medium resistance.cost (range 0.01 to 0.2)
                             starting.treatment.site.intensity = 0,
                             starting.refugia.intensity = 0,
                             min.intervention.coverage = 0.1,
                             max.intervention.coverage = 0.9,
                             min.dispersal.rate = 0, #No Dispersal
                             max.dispersal.rate = 0, #No Dispersal
                             maximum.generations = 500)#do not change (matches insecticide sequence)

#get intensities for insecticides based treatment area only
insecticide1T = simulation1[2,1,]
insecticide2T = simulation1[2,2,]
insecticide3T = simulation1[2,3,]

#add generation vector
generation = seq(1, 500, by = 1)

#what insecticide was used (this copied from sequence in run_simulation):
current.insecticide = c(rep(1, times = 100), rep(2, times = 100), 
                        rep(3, times = 100), rep(1, times=100), rep(2, times = 100))

#turn into a dataframe for plotting
sim1 = data.frame(insecticide1T, insecticide2T, insecticide3T, current.insecticide, generation)

#make the insecticide a variable
sim1 = pivot_longer(sim1, c("insecticide1T", "insecticide2T", "insecticide3T"), 
                    names_to = "Insecticide", values_to = "resistance.intensity")

#plot simulation results
ggplot(sim1, aes(x=generation, y=resistance.intensity, colour=Insecticide))+
  geom_line(size = 2) + 
  #geom_point(aes(x=generation, y=refugia_intensity), colour = "green") + #refugia is green
  ylab("Resistance Intensity")



