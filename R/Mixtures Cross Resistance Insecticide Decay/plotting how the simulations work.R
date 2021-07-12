library(ggplot2)
library(patchwork)
library(dplyr)
create_normal_distribution = function(vector.length,
                                      trait.mean,
                                      standard.deviation){
  
  normal.distribution = stats::qnorm(seq(from = 1/vector.length,
                                         to =1-1/vector.length,
                                         length.out = vector.length),
                                     mean=trait.mean, sd=standard.deviation)
  
  return(normal.distribution)
}

normal.distribution = create_normal_distribution(vector.length = 10000,
                                                 trait.mean = 50,
                                                 standard.deviation = 10)
relative.density = dnorm(normal.distribution,
                         mean = 50,
                         sd = 10)

range(normal.distribution)

df = data.frame(normal.distribution, relative.density)

population.plot = ggplot(df, aes(x=normal.distribution, y=relative.density))+
  geom_line(size = 5,
            col="black")+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             size = 2, 
             colour = "black")+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

population.plot


#Male population:
male.population.plot = ggplot(df, aes(x=normal.distribution, y=relative.density))+
  geom_line(size = 5,
            col="blue")+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             size = 2, 
             colour = "blue")+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

#Female population plot:
female.population.plot = ggplot(df, aes(x=normal.distribution, y=relative.density))+
  geom_line(size = 5,
            col = "blue")+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             size = 2,
             colour = "blue")+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

male.population.plot
female.population.plot



#Only a proportion of the mosquitoes will encounter the insecticide:
female.insecticide.exposed = ggplot(df, aes(x=normal.distribution, y=relative.density*0.7))+
  geom_line(colour = "red", size = 5)+
  geom_line(aes(x=normal.distribution, y=relative.density*0.3),
            colour="green", size = 5)+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             colour = "blue",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

female.insecticide.exposed



male.insecticide.exposed = ggplot(
  df, aes(x=normal.distribution, y=relative.density*0.7*0.3))+
  geom_line(colour = "red", size = 5)+
  geom_line(aes(x=normal.distribution, y=relative.density*(1-0.7*0.3)),
            colour="green", size = 5)+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             colour = "blue",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

male.insecticide.exposed



male.post.exposure = ggplot(df, aes(x=normal.distribution, y=relative.density*0.7*0.3))+
  geom_line(colour = "red", size = 1)+
  geom_line(aes(x=normal.distribution, y=relative.density*(1-0.7*0.3)),
            colour="green", size = 1)+
  geom_line(aes(x=normal.distribution+5, y=relative.density*0.85),
            colour = "orange", size = 5)+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             colour = "blue",
             size = 2)+
  geom_vline(xintercept = 55,
             linetype = "dashed",
             colour = "orange",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


male.post.exposure

female.post.exposure = ggplot(
  df, aes(x=normal.distribution, y=relative.density*0.7))+
  geom_line(colour = "red", size = 1)+
  geom_line(aes(x=normal.distribution, y=relative.density*0.3),
            colour="green", size =1)+
  geom_line(aes(x=normal.distribution+10, y=relative.density*0.5),
            colour = "orange", size = 5)+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             colour = "blue",
             size = 2)+
  geom_vline(xintercept = 60,
             linetype = "dashed",
             colour = "orange",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

female.post.exposure



next.gen.plot = ggplot(df, aes(x=normal.distribution, y=relative.density))+
  geom_line(size = 4,
            col="black",
            alpha = 0.7)+
  geom_vline(xintercept = 50,
             linetype = "dashed",
             size = 2, 
             colour = "black")+
  geom_line(aes(x=normal.distribution+5, y=relative.density),
            colour = "blue", size = 5)+
  geom_vline(xintercept = 55,
             linetype = "dashed",
             size = 2, 
             colour = "blue")+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  xlim(12, 100)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

next.gen.plot 

next.generations = ggplot(
  df, aes(x=normal.distribution, y=relative.density))+
  geom_line(colour = "#969696", size = 5)+
  geom_line(aes(x=normal.distribution+5, y=relative.density),
                colour = "#737373", size = 5)+
  geom_line(aes(x=normal.distribution+10, y=relative.density),
            colour = "#525252", size = 5)+
  geom_line(aes(x=normal.distribution+15, y=relative.density),
    colour = "#252525", size = 5)+
  geom_line(aes(x=normal.distribution+20, y=relative.density),
            colour = "black", size = 5)+
  geom_line(aes(x=normal.distribution+25, y=relative.density),
            colour = "#000000", size = 5)+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.04)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
next.generations




df=data.frame(bioassay.survival.values=seq(0, 0.6,
                                           by = 0.001))%>%
  dplyr::rowwise()%>%
  dplyr::mutate(resistance.values = bioassay_survival_to_resistance(
    maximum.bioassay.survival.proportion=1,
    michaelis.menten.slope=1, 
    half.population.bioassay.survival.resistance=900, 
    bioassay.survival=bioassay.survival.values, 
    estimate.precision=0.001, 
    sd.population.resistance=0,
    nsim=1,
    minimum.resistance.value=0, 
    maximum.resistance.value=15000)) ##plotting with sigma as 0.1 or 25 made no difference to plots

df=df%>%
  dplyr::filter(resistance.values <= 1000)


ggplot2::ggplot(df, aes(x=resistance.values, y = bioassay.survival.values)) +
  geom_point(colour = "#c994c7",
            size = 2.5) +
  geom_segment(aes(x = 900, y = 0.5, 
                   xend = 900, yend = 0),
                             size = 1,
               linetype = "dotted")+
  geom_segment(aes(x = 0, y = 0.5,
                   xend = 900, yend = 0.5),
               size = 1,
               linetype = "dotted")+
  geom_segment(aes(x = 100, y = 0.1, 
                   xend = 100, yend = 0),
               size = 1,
               linetype = "dotted")+
  geom_segment(aes(x = 0, y = 0.1,
                   xend = 100, yend = 0.1),
               size = 1,
               linetype = "dotted")+
  xlab("Polygenic Resistance Score") +
  ylab("Bioassay Survival Proportion") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.05))+
  theme_classic()+
  theme(axis.title.x = element_text(size= 20),
        axis.title.y = element_text(size = 20))


parameter.space = data.frame(lhs::randomLHS(100, 6)) #5000 random samples of the 6 input parameters.

parameter.space = parameter.space%>%
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
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.1, 0.9))%>%
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))%>%
  dplyr::mutate(Replicate = as.character(seq(1, 100, by = 1)))




herit1 = ggplot(parameter.space, aes(x=Heritability, y= `Male Insecticide Exposure`,
                            colour = Replicate))+
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3))+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
geom_point()+
  ylab("Male Exposure")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 10))

herit2 = ggplot(parameter.space, aes(x=Heritability, y= `Female Insecticide Exposure`,
                            colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  ylab("Female Exposure")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 10))


herit3 =ggplot(parameter.space, aes(x=Heritability, y= `Intervention Coverage`,
                            colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 10))

herit4 =ggplot(parameter.space, aes(x=Heritability, y= `Fitness Cost`,
                            colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3))+
  scale_y_continuous(breaks = c(0, 0.1, 0.2))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 10))

herit5= ggplot(parameter.space, aes(x=Heritability, y= `Dispersal`,
                            colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10))



male2 = ggplot(parameter.space, aes(x=`Male Insecticide Exposure`, y= `Female Insecticide Exposure`,
                                     colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

male3 =ggplot(parameter.space, aes(x=`Male Insecticide Exposure`, y= `Intervention Coverage`,
                                    colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

male4 =ggplot(parameter.space, aes(x=`Male Insecticide Exposure`, y= `Fitness Cost`,
                                    colour = Replicate))+
  scale_y_continuous(breaks = c(0, 0.1, 0.2))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

male5= ggplot(parameter.space, aes(x=`Male Insecticide Exposure`, y= `Dispersal`,
                                    colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  xlab("Male Exposure")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


female3 =ggplot(parameter.space, aes(x=`Female Insecticide Exposure`, y= `Intervention Coverage`,
                                   colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


female4 =ggplot(parameter.space, aes(x=`Female Insecticide Exposure`, y= `Fitness Cost`,
                                   colour = Replicate))+
  geom_point()+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  scale_x_continuous(breaks = c(0, 0.1, 0.2))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


female5= ggplot(parameter.space, aes(x=`Female Insecticide Exposure`, y= `Dispersal`,
                                   colour = Replicate))+
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  geom_point()+
  xlab("Female Exposure")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



cover4 =ggplot(parameter.space, aes(x=`Intervention Coverage`, y= `Fitness Cost`,
                                     colour = Replicate))+
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  scale_y_continuous(breaks = c(0, 0.1, 0.2))+
  geom_point()+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


cover5= ggplot(parameter.space, aes(x=`Intervention Coverage`, y= `Dispersal`,
                                     colour = Replicate))+
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  geom_point()+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



fit5= ggplot(parameter.space, aes(x=`Fitness Cost`, y= `Dispersal`,
                                    colour = Replicate))+
  geom_point()+
  scale_x_continuous(breaks = c(0, 0.1, 0.2))+
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


library(patchwork)

layout = "
A####
BG###
CHK##
DILN#
EJMOP
"

wrap_plots(A = herit1,
           B = herit2,
           C = herit3,
           D = herit4,
           E = herit5,
           G = male2,
           H = male3,
           I = male4,
           J = male5,
           K = female3,
           L = female4,
           M = female5,
           N = cover4,
           O = cover5,
           P = fit5,
           design = layout
           )


parameter.space2 = data.frame(lhs::randomLHS(5000, 6)) #5000 random samples of the 6 input parameters.

parameter.space2 = parameter.space2%>%
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
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.1, 0.9))%>%
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))


plot(parameter.space2)


#Run an example simulation::
A = run_simulation_intervention(number.of.insecticides = 2,
                                exposure.scaling.factor = 10,
                                nsim = 1000,
                                minimum.insecticide.resistance.heritability = 0.28,
                                maximum.insecticide.resistance.heritability = 0.28,
                                minimum.male.insecticide.exposure = 0.7,
                                maximum.male.insecticide.exposure = 0.7,
                                minimum.female.insecticide.exposure = 0.9,
                                maximum.female.insecticide.exposure = 0.9,
                                resistance.cost = 0.3,
                                starting.treatment.site.intensity = 0,
                                starting.refugia.intensity = 0,
                                min.intervention.coverage = 0.7,
                                max.intervention.coverage = 0.7,
                                min.dispersal.rate = 0.3,
                                max.dispersal.rate = 0.3,
                                maximum.generations = 500,
                                irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                half.population.bioassay.survival.resistance = 900,
                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                maximum.resistance.value = 25000)

B = get_simulation_dataframe(simulation.array = A,
                             number.of.insecticides = 2,
                             maximum.generations = 500)


B = B%>%
  dplyr::rowwise()%>%
  dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                    mean.population.resistance = resistance.intensity,
                                                                    michaelis.menten.slope = 1, 
                                                                    half.population.bioassay.survival.resistance = 900,
                                                                    sd.population.resistance = 0, 
                                                                    nsim = 1) * 100)#As a percentage)
#Make as separate dataframes: 1 for treatment, 1 for refugia
temp.df.treatment = B%>%
  dplyr::filter(site == "treatment")


treatment.plot = ggplot(data = temp.df.treatment, aes(x=time.in.generations, 
                                                      y = bioassay.survival, 
                                                      colour = insecticide.tracked,))+
  geom_point(aes(x=time.in.generations, y=10, #Make a line of the deployed insecticide at %
                 colour=insecticide.deployed), #colours should match the plots
                 alpha = 0.7,
             size = 1) +
  geom_point(aes(x=time.in.generations, #Line indicating the return threshold.
                 y=(5)), 
             colour="grey", 
             alpha = 0.3) +  #keep it fairly faint.
  geom_line(data =temp.df.treatment, aes(x=time.in.generations, 
                                         y=bioassay.survival, #aleady in %
                                         colour=insecticide.tracked),
            size = 3)+
  scale_colour_manual(values = c("#1f78b4", "#b2182b"))+
  scale_y_continuous(breaks = seq(1, 10, 1))+
  ylab("Bioassay Survival (%)")+
  xlab("Time in Mosquito Generations")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
                    

treatment.plot

#create a diagram to explain parameter space sampling::
parameter.vector = as.factor(c("Heritability", "Female Exposure", "Male Exposure", "Intervention Coverage", "Dispersal", "Fitness Cost"))
parameter.values1 = runif(6, 0, 1)
parameter.values2 = runif(6, 0, 1)
parameter.values3 = runif(6, 0, 1)

df = data.frame(parameter.vector, parameter.values1, parameter.values2, parameter.values3)

levels(df$parameter.vector) <- gsub(" ", "\n", levels(df$parameter.vector))

ggplot(df, aes(x= parameter.values, y = parameter.vector))+
  geom_hline(yintercept = "Heritability",
             size = 2 , colour = "lightblue")+
  geom_hline(yintercept = "Dispersal",
             size = 2 , colour = "lightblue")+
  geom_hline(yintercept = "Male\nExposure",
             size = 2 , colour = "lightblue")+
  geom_hline(yintercept = "Female\nExposure",
             size = 2 , colour = "lightblue")+
  geom_hline(yintercept = "Fitness\nCost",
             size=2, colour="lightblue")+
  geom_hline(yintercept = "Intervention\nCoverage",
             size = 2 , colour = "lightblue")+
  geom_point(aes(x=parameter.values1), 
             size = 10, colour = "black")+
  geom_point(aes(x=parameter.values1),
             size = 3, colour = "red")+
  geom_point(aes(x=parameter.values2), 
             size = 10, colour = "black")+
  geom_point(aes(x=parameter.values2),
             size = 3, colour = "green")+
  geom_point(aes(x=parameter.values3), 
             size = 10, colour = "black")+
  geom_point(aes(x=parameter.values3),
             size = 3, colour = "yellow")+
  ylab("Parameter")+
  xlab("Parameter Value")+
  theme_classic()+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15))
  



















