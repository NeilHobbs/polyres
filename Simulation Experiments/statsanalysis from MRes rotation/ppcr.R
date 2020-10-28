library(lhs)
library(ppcor)
library(dplyr)
library(ggplot2)
#Make latin hypercube for sampling, that allows 2000 simulations total
df = data.frame(randomLHS(2000, 6))

#Rename columns; and change distributions to be correct
#for easier tracking of which variable is which.
#run and extract value for treatment and refugia at 100, 300, 500 gens
#uses the run_simulation_endpoint function in file called "modified runs" in the statsanalysis folder

df = df%>%
  rename(insecticide.resistance.hertiability = X1)%>%
  rename(male.insecticide.exposure = X2)%>%
  rename(female.insecticide.exposure = X3)%>%
  rename(resistance.cost = X4)%>%
  rename(intervention.coverage = X5)%>%
  rename(dispersal = X6)%>%
  mutate(insecticide.resistance.hertiability = qunif(insecticide.resistance.hertiability, 0.05, 0.3))%>%
  mutate(male.insecticide.exposure = qunif(male.insecticide.exposure, 0, 1))%>%
  mutate(female.insecticide.exposure = qunif(female.insecticide.exposure, 0.4, 0.9))%>%
  mutate(resistance.cost = qunif(resistance.cost, 0.01, 0.2))%>%
  mutate(intervention.coverage = qunif(intervention.coverage, 0.1, 0.9))%>%
  mutate(dispersal = qunif(dispersal, 0.1, 0.9))%>%
  rowwise()%>%
  mutate(T500 = (run_simulation_endpoint(number.of.insecticides = 3,
                                        exposure.scaling.factor = 10,
                                        nsim = 1, #only needs to be 1, as min and max values equal
                                        minimum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                        maximum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                        minimum.male.insecticide.exposure = male.insecticide.exposure,
                                        maximum.male.insecticide.exposure = male.insecticide.exposure,
                                        minimum.female.insecticide.exposure = female.insecticide.exposure,
                                        maximum.female.insecticide.exposure = female.insecticide.exposure,
                                        resistance.cost = resistance.cost,
                                        starting.treatment.site.intensity = 0,
                                        starting.refugia.intensity = 0,
                                        min.intervention.coverage = intervention.coverage,
                                        max.intervention.coverage = intervention.coverage,
                                        min.dispersal.rate = dispersal,
                                        max.dispersal.rate = dispersal,
                                        maximum.generations = 500)[1]))%>%
  mutate(T300 = (run_simulation_endpoint(number.of.insecticides = 3,
                                         exposure.scaling.factor = 10,
                                         nsim = 1, #only needs to be 1, as min and max values equal
                                         minimum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         maximum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         minimum.male.insecticide.exposure = male.insecticide.exposure,
                                         maximum.male.insecticide.exposure = male.insecticide.exposure,
                                         minimum.female.insecticide.exposure = female.insecticide.exposure,
                                         maximum.female.insecticide.exposure = female.insecticide.exposure,
                                         resistance.cost = resistance.cost,
                                         starting.treatment.site.intensity = 0,
                                         starting.refugia.intensity = 0,
                                         min.intervention.coverage = intervention.coverage,
                                         max.intervention.coverage = intervention.coverage,
                                         min.dispersal.rate = dispersal,
                                         max.dispersal.rate = dispersal,
                                         maximum.generations = 500)[2]))%>%
  mutate(T100 = (run_simulation_endpoint(number.of.insecticides = 3,
                                         exposure.scaling.factor = 10,
                                         nsim = 1, #only needs to be 1, as min and max values equal
                                         minimum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         maximum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         minimum.male.insecticide.exposure = male.insecticide.exposure,
                                         maximum.male.insecticide.exposure = male.insecticide.exposure,
                                         minimum.female.insecticide.exposure = female.insecticide.exposure,
                                         maximum.female.insecticide.exposure = female.insecticide.exposure,
                                         resistance.cost = resistance.cost,
                                         starting.treatment.site.intensity = 0,
                                         starting.refugia.intensity = 0,
                                         min.intervention.coverage = intervention.coverage,
                                         max.intervention.coverage = intervention.coverage,
                                         min.dispersal.rate = dispersal,
                                         max.dispersal.rate = dispersal,
                                         maximum.generations = 500)[3]))%>%
           mutate(R500 = (run_simulation_endpoint(number.of.insecticides = 3,
                                                  exposure.scaling.factor = 10,
                                                  nsim = 1, #only needs to be 1, as min and max values equal
                                                  minimum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                                  maximum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                                  minimum.male.insecticide.exposure = male.insecticide.exposure,
                                                  maximum.male.insecticide.exposure = male.insecticide.exposure,
                                                  minimum.female.insecticide.exposure = female.insecticide.exposure,
                                                  maximum.female.insecticide.exposure = female.insecticide.exposure,
                                                  resistance.cost = resistance.cost,
                                                  starting.treatment.site.intensity = 0,
                                                  starting.refugia.intensity = 0,
                                                  min.intervention.coverage = intervention.coverage,
                                                  max.intervention.coverage = intervention.coverage,
                                                  min.dispersal.rate = dispersal,
                                                  max.dispersal.rate = dispersal,
                                                  maximum.generations = 500)[4]))%>%
  mutate(R300 = (run_simulation_endpoint(number.of.insecticides = 3,
                                         exposure.scaling.factor = 10,
                                         nsim = 1, #only needs to be 1, as min and max values equal
                                         minimum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         maximum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         minimum.male.insecticide.exposure = male.insecticide.exposure,
                                         maximum.male.insecticide.exposure = male.insecticide.exposure,
                                         minimum.female.insecticide.exposure = female.insecticide.exposure,
                                         maximum.female.insecticide.exposure = female.insecticide.exposure,
                                         resistance.cost = resistance.cost,
                                         starting.treatment.site.intensity = 0,
                                         starting.refugia.intensity = 0,
                                         min.intervention.coverage = intervention.coverage,
                                         max.intervention.coverage = intervention.coverage,
                                         min.dispersal.rate = dispersal,
                                         max.dispersal.rate = dispersal,
                                         maximum.generations = 500)[5]))%>%
  mutate(R100 = (run_simulation_endpoint(number.of.insecticides = 3,
                                         exposure.scaling.factor = 10,
                                         nsim = 1, #only needs to be 1, as min and max values equal
                                         minimum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         maximum.insecticide.resistance.hertitability = insecticide.resistance.hertiability,
                                         minimum.male.insecticide.exposure = male.insecticide.exposure,
                                         maximum.male.insecticide.exposure = male.insecticide.exposure,
                                         minimum.female.insecticide.exposure = female.insecticide.exposure,
                                         maximum.female.insecticide.exposure = female.insecticide.exposure,
                                         resistance.cost = resistance.cost,
                                         starting.treatment.site.intensity = 0,
                                         starting.refugia.intensity = 0,
                                         min.intervention.coverage = intervention.coverage,
                                         max.intervention.coverage = intervention.coverage,
                                         min.dispersal.rate = dispersal,
                                         max.dispersal.rate = dispersal,
                                         maximum.generations = 500)[6]))

#Convert as numeric to be allow saving as a csv
df = df%>%
  mutate(T500 = as.numeric(T500))%>%
  mutate(T300 = as.numeric(T300))%>%
  mutate(T100 = as.numeric(T100))%>%
  mutate(R500 = as.numeric(R500))%>%
  mutate(R300 = as.numeric(R300))%>%
  mutate(R100 = as.numeric(R100))

#Allowed stop point
write.csv(df, "C:\\Users\\neilp\\OneDrive\\My Documents\\polyres\\statsanalysis\\SimParamSpace.csv", row.names = TRUE)


##Read in the saved csv file
df = read.csv(".\\SimParamSpace.csv")

#partial correlation needs each endpoint as the final column. Does not want to be correlating with earlier/later timepoints
dfT100 = df%>%
  select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
         "intervention.coverage", "dispersal", "female.insecticide.exposure", "T100")

dfT300 = df%>%
  select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
         "intervention.coverage", "dispersal", "female.insecticide.exposure", "T300")
dfT500 = df%>%
  select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
         "intervention.coverage", "dispersal", "female.insecticide.exposure", "T500")

dfR100 = df%>%
  select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
         "intervention.coverage", "dispersal", "female.insecticide.exposure", "R100")

dfR300 = df%>%
  select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
         "intervention.coverage", "dispersal", "female.insecticide.exposure", "R300")
dfR500 = df%>%
  select("insecticide.resistance.hertiability", "resistance.cost", "male.insecticide.exposure",
         "intervention.coverage", "dispersal", "female.insecticide.exposure", "R500")


##Run Partial correlation using pearson for treatment site and refugia at 100, 300, 500 gens
pcor.output.T100 = pcor(dfT100, method = "pearson")
pcor.output.T300 = pcor(dfT300, method = "pearson")
pcor.output.T500 = pcor(dfT500, method = "pearson")
pcor.output.R100 = pcor(dfR100, method = "pearson")
pcor.output.R300 = pcor(dfR300, method = "pearson")
pcor.output.R500 = pcor(dfR500, method = "pearson")

parameter = c("Heritability", "Fitness Cost", "Male Insecticide Exposure",
              "Intervention Coverage", "Dispersal", "Female Insecticide Exposure", NA)

#T100
estimate = pcor.output.T100$estimate[7,]
pvalue = pcor.output.T100$p.value[7,]
teststat= pcor.output.T100$statistic[7,]
T100.DF = data.frame(estimate, pvalue, teststat, parameter)
T100.DF = T100.DF[1:6,] #do not need the final row
T100.DF = T100.DF%>%
  mutate(Location = "Intervention Site")%>%
  mutate(Generation = "100")

#T300
estimate = pcor.output.T300$estimate[7,]
pvalue = pcor.output.T300$p.value[7,]
teststat = pcor.output.T300$statistic[7,]
T300.DF = data.frame(estimate, pvalue, teststat, parameter)
T300.DF = T300.DF[1:6,] #do not need the final row
T300.DF = T300.DF%>%
  mutate(Location = "Intervention Site")%>%
  mutate(Generation = "300")

#T500
estimate = pcor.output.T500$estimate[7,]
pvalue = pcor.output.T500$p.value[7,]
teststat = pcor.output.T500$statistic[7,]
T500.DF = data.frame(estimate, pvalue, teststat, parameter)
T500.DF = T500.DF[1:6,] #do not need the final row
T500.DF = T500.DF%>%
  mutate(Location = "Intervention Site")%>%
  mutate(Generation = "500")

#R100
estimate = pcor.output.R100$estimate[7,]
pvalue = pcor.output.R100$p.value[7,]
teststat = pcor.output.R100$statistic[7,]
R100.DF = data.frame(estimate, pvalue, teststat, parameter)
R100.DF = R100.DF[1:6,] #do not need the final row
R100.DF = R100.DF%>%
  mutate(Location = "Refugia")%>%
  mutate(Generation = "100")


#R300
estimate = pcor.output.R300$estimate[7,]
pvalue = pcor.output.R300$p.value[7,]
teststat = pcor.output.R300$statistic[7,]
R300.DF = data.frame(estimate, pvalue, teststat, parameter)
R300.DF = R300.DF[1:6,] #do not need the final row
R300.DF = R300.DF%>%
  mutate(Location = "Refugia")%>%
  mutate(Generation = "300")


#R500
estimate = pcor.output.R500$estimate[7,]
pvalue = pcor.output.R500$p.value[7,]
teststat = pcor.output.R500$statistic[7,]
R500.DF = data.frame(estimate, pvalue, teststat, parameter)
R500.DF = R500.DF[1:6,] #do not need the final row
R500.DF = R500.DF%>%
  mutate(Location = "Refugia")%>%
  mutate(Generation = "500")

final.df = bind_rows(T100.DF, T300.DF, T500.DF, R100.DF, R300.DF, R500.DF)

write.csv(final.df, "C:\\Users\\neilp\\OneDrive\\My Documents\\polyres\\statsanalysis\\pcoroutputdf.csv", row.names = TRUE)

#plot of correlation after 500 generations.
ggplot(final.df, aes(x=estimate, y=parameter, fill = Generation)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
   ylab("Parameter") +
    xlab("Correlation") +
    theme_classic()+
  facet_wrap(~Location)

print(pcor.df)

colnames(parameter.space)

##Get only the random parameter variables
parameter.space = df[,2:7]
parameter.space = parameter.space%>%
  rename(Heritability = insecticide.resistance.hertiability)%>%
  rename(`Male Exposure` = male.insecticide.exposure)%>%
  rename(`Female Exposure` = female.insecticide.exposure)%>%
  rename(`Intervention Coverage` = intervention.coverage)%>%
  rename(`Fitness Cost` = resistance.cost)%>%
  rename(Dispersal = dispersal)


plot(parameter.space)




