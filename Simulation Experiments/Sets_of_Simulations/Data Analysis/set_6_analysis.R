##set_6_analysis:::
#Read in the datasets
library(dplyr)
library(ggplot2)


mixtures.set.6 = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/mixtures.set.6.csv")
sequences.set.6 = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set.6.csv")

unique(mixtures.set.6$part.1.start)


#convert the peak PRS (to second part of mixture) to bioassay survival:
mixtures.set.6 = mixtures.set.6%>%
  dplyr::rowwise()%>%
  mutate(end.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                        mean.population.resistance = peak.resistance,
                                                        michaelis.menten.slope = 1, 
                                                        half.population.bioassay.survival.resistance = 900,
                                                        sd.population.resistance = 0, 
                                                        nsim = 1))%>%
  dplyr::rowwise()%>%
  mutate(start.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                        mean.population.resistance = part.1.start,
                                                        michaelis.menten.slope = 1, 
                                                        half.population.bioassay.survival.resistance = 900,
                                                        sd.population.resistance = 0, 
                                                        nsim = 1))

sequences.set.6 = sequences.set.6%>%
  dplyr::rowwise()%>%
  mutate(end.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                        mean.population.resistance = peak.resistance,
                                                        michaelis.menten.slope = 1, 
                                                        half.population.bioassay.survival.resistance = 900,
                                                        sd.population.resistance = 0, 
                                                        nsim = 1))
  



compare_against_single = function(start.prs){

mixtures.set.6.split = mixtures.set.6%>%
  dplyr::filter(part.1.start == start.prs)

diff = sequences.set.6$end.survival - mixtures.set.6.split$end.survival

return(diff)
}

diff.vals = c(diff_0 = compare_against_single(0),
              diff_47 = compare_against_single(47),
              diff_100 = compare_against_single(100),
              diff_225 = compare_against_single(225),
              diff_600 = compare_against_single(600),
              diff_900 = compare_against_single(900),
              diff_2100 = compare_against_single(2100),
              diff_8100 = compare_against_single(8100),
              diff_89100 = compare_against_single(89100))


mixtures.set.6$diff.vals = diff.vals

ggplot(mixtures.set.6, aes(x=start.survival*100,
                     y=diff.vals*100))+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Start Bioassay Survival, Mixture Part 1")+
  ylab("Difference in End Bioassay Survival")+
  theme_classic()


