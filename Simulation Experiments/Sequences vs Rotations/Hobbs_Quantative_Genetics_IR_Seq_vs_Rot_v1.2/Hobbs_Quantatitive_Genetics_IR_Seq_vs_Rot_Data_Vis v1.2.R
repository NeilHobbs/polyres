#Hobbs_Quantatitive_Genetics_IR_Seq_vs_Rot_Data_Vis v1.2.R
########################################
###########Data Visualtions############# 
########################################

#Required Packages:
library(devtools)
load_all() #loads in polyres if opened the polyres R project first
library(ggplot2) #for plotting results
library(dplyr)


#Required Datasets:
#read in the random sample
example.dataframe = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/random_parameter_sample.csv")

#Has differences between Rot and Seq
all.sims.join = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/seq_rot_sims.csv")

#partial rank correlation
pcor.df.all = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/partial_rank_correlation.csv")

#Cross selection runs data::
sequence.rotation.cross.full.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/sequence.rotation.cross.full.df.csv")

#partial rank correlation of
pcor.cross.df = read.csv("Simulation Experiments/Sequences vs Rotations/Hobbs_Quantative_Genetics_IR_Seq_vs_Rot_v1.2/partial_rank_correlation_cross.csv")

#Make the Graphs
make_figure1_plot = function(){

seq.example.c0 = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                      exposure.scaling.factor = 10,
                                                                      nsim = 1,
                                                                      minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      resistance.cost = example.dataframe$resistance.cost,
                                                                      starting.treatment.site.intensity = 0,
                                                                      starting.refugia.intensity = 0,
                                                                      min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      min.dispersal.rate = example.dataframe$dispersal,
                                                                      max.dispersal.rate = example.dataframe$dispersal,
                                                                      maximum.generations = 500, #appoximately 50 years
                                                                      irm.strategy = "sequence", 
                                                                      half.population.bioassay.survival.resistance = 900, 
                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                      deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                      maximum.resistance.value = 25000 #have arbitrarily high just in case
) , 500, 2)

rot.example.c0 = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                      exposure.scaling.factor = 10,
                                                                      nsim = 1,
                                                                      minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                      minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                      minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                      resistance.cost = example.dataframe$resistance.cost,
                                                                      starting.treatment.site.intensity = 0,
                                                                      starting.refugia.intensity = 0,
                                                                      min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                      min.dispersal.rate = example.dataframe$dispersal,
                                                                      max.dispersal.rate = example.dataframe$dispersal,
                                                                      maximum.generations = 500, #appoximately 50 years
                                                                      irm.strategy = "rotation", 
                                                                      half.population.bioassay.survival.resistance = 900, 
                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                      deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                      maximum.resistance.value = 25000 #have arbitrarily high just in case
) , 500, 2)

seq.example.neg = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                                       exposure.scaling.factor = 10,
                                                                                       nsim = 1,
                                                                                       minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       resistance.cost = example.dataframe$resistance.cost,
                                                                                       starting.treatment.site.intensity = 0,
                                                                                       starting.refugia.intensity = 0,
                                                                                       min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       min.dispersal.rate = example.dataframe$dispersal,
                                                                                       max.dispersal.rate = example.dataframe$dispersal,
                                                                                       maximum.generations = 500, #appoximately 50 years
                                                                                       irm.strategy = "sequence", 
                                                                                       half.population.bioassay.survival.resistance = 900, 
                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                       deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                       maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                                       min.cross.selection = -0.3,
                                                                                       max.cross.selection = -0.3), 500, 2)

rot.example.neg = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                                       exposure.scaling.factor = 10,
                                                                                       nsim = 1,
                                                                                       minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       resistance.cost = example.dataframe$resistance.cost,
                                                                                       starting.treatment.site.intensity = 0,
                                                                                       starting.refugia.intensity = 0,
                                                                                       min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       min.dispersal.rate = example.dataframe$dispersal,
                                                                                       max.dispersal.rate = example.dataframe$dispersal,
                                                                                       maximum.generations = 500, #appoximately 50 years
                                                                                       irm.strategy = "rotation", 
                                                                                       half.population.bioassay.survival.resistance = 900, 
                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                       deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                       maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                                       min.cross.selection = -0.3,
                                                                                       max.cross.selection = -0.3), 500, 2)


seq.example.pos = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                                       exposure.scaling.factor = 10,
                                                                                       nsim = 1,
                                                                                       minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       resistance.cost = example.dataframe$resistance.cost,
                                                                                       starting.treatment.site.intensity = 0,
                                                                                       starting.refugia.intensity = 0,
                                                                                       min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       min.dispersal.rate = example.dataframe$dispersal,
                                                                                       max.dispersal.rate = example.dataframe$dispersal,
                                                                                       maximum.generations = 500, #appoximately 50 years
                                                                                       irm.strategy = "sequence", 
                                                                                       half.population.bioassay.survival.resistance = 900, 
                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                       deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                       maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                                       min.cross.selection = 0.3,
                                                                                       max.cross.selection = 0.3), 500, 2)

rot.example.pos = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = example.dataframe$sample.insecticides,
                                                                                       exposure.scaling.factor = 10,
                                                                                       nsim = 1,
                                                                                       minimum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       maximum.insecticide.resistance.heritability = example.dataframe$insecticide.resistance.hertiability,
                                                                                       minimum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       maximum.male.insecticide.exposure = example.dataframe$male.insecticide.exposure,
                                                                                       minimum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       maximum.female.insecticide.exposure = example.dataframe$female.insecticide.exposure,
                                                                                       resistance.cost = example.dataframe$resistance.cost,
                                                                                       starting.treatment.site.intensity = 0,
                                                                                       starting.refugia.intensity = 0,
                                                                                       min.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       max.intervention.coverage = example.dataframe$intervention.coverage,
                                                                                       min.dispersal.rate = example.dataframe$dispersal,
                                                                                       max.dispersal.rate = example.dataframe$dispersal,
                                                                                       maximum.generations = 500, #appoximately 50 years
                                                                                       irm.strategy = "rotation", 
                                                                                       half.population.bioassay.survival.resistance = 900, 
                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                       deployment.frequency = example.dataframe$sample.frequency, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                       maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                                       min.cross.selection = 0.3,
                                                                                       max.cross.selection = 0.3) , 500, 2)

rot.example.c0.plot = plot_simulation(simulation.dataframe = rot.example.c0, 
                                      half.population.bioassay.survival.resistance = 900, 
                                      withdrawal.threshold = 0.1,
                                      return.threshold = 0.05)

seq.example.c0.plot = plot_simulation(simulation.dataframe = seq.example.c0,
                                      half.population.bioassay.survival.resistance = 900, 
                                      withdrawal.threshold = 0.1,
                                      return.threshold = 0.05)

rot.example.neg.plot = plot_simulation(simulation.dataframe = rot.example.neg,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

seq.example.neg.plot = plot_simulation(simulation.dataframe = seq.example.neg,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

rot.example.pos.plot = plot_simulation(simulation.dataframe = rot.example.pos,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

seq.example.pos.plot = plot_simulation(simulation.dataframe = seq.example.pos,
                                       half.population.bioassay.survival.resistance = 900, 
                                       withdrawal.threshold = 0.1,
                                       return.threshold = 0.05)

#This is figure 1
figure.1 = gridExtra::grid.arrange(seq.example.c0.plot, rot.example.c0.plot,
                        seq.example.neg.plot, rot.example.neg.plot,
                        seq.example.pos.plot, rot.example.pos.plot,
                        ncol = 2, nrow = 3,
                        top = "Sequences                                                                                                                                       Rotations",
                        left = "Positive Cross Selection          Negative Cross Selection          No Cross Selection")

return(figure.1)
}
make_figure1_plot()

make_figure2_plot = function(){
replicate = seq(1, 45000, by = 1)

all.sims.join = data.frame(all.sims.join, replicate)

sim.not.equal.gens = all.sims.join%>%
  dplyr::filter(simulation.duration.sequence != simulation.duration.rotation)

#Creates a dataset for putting in labels for putting in the 
label.df = data.frame(
  text.label = c("Favours Sequences", "Favours Rotations"),
  label_x_coord = c(46000, 46000), #have the label fairly central.
  label_y_coord = c(200, -200)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq = data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(7500, 22500, 37500),
  label_y_coord = c(360, 360, 360)
)

#This is currently Figure 2
#Scatterplot of difference in duration (n=16815).
figure2 = ggplot(sim.not.equal.gens, aes(x = replicate.1, y=diff.duration)) +
  geom_point(aes(colour = as.factor(insecticides.in.sim)),
             alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
  geom_vline(xintercept = 15000, linetype = "dashed") +
  geom_vline(xintercept = 30000, linetype = "dashed")+
  geom_hline(yintercept = 0, size = 1)+
  xlab("Simulation Replicate Pair")+ 
  ylab("Difference in Simulation Duration (Generations)")+ 
  ylim(-360, 360)+#set as the maximum range
  geom_text(data = label.df, aes(label = text.label,
                                 x=label_x_coord,
                                 y=label_y_coord),
            angle = 270, size = 5)+
  geom_label(data = label.df.depfreq, aes(label = text.label,
                                          x=label_x_coord,
                                          y=label_y_coord),
             fill = "orchid")+
  theme_classic()+
  theme(legend.position = "none")

return(figure2)
}
make_figure2_plot()

make_figure3_plot = function(){
  
  replicate = seq(1, 45000, by = 1)
  
  all.sims.join = data.frame(all.sims.join, replicate)
  
  sim.equal.gens = all.sims.join%>%
    dplyr::filter(simulation.duration.sequence == simulation.duration.rotation)
  
  
  #Creates a dataset for putting in labels for putting in the 
  label.df1 = data.frame(
    text.label = c("Favours Rotations", "Favours Sequences"),
    label_x_coord = c(45500, 45500), #have the label fairly central.
    label_y_coord = c(2.5, -2.50)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq1 = data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(6, 6, 6)
  )
  
  
  #plot of the difference average bioassay survival to the deployed insecticide (n=15589)
  #this is figure 3.
 figure.3 =  ggplot(sim.equal.gens, aes(x = replicate.1, y= diff.av.survival*100))+
    geom_hline(yintercept = 0, size = 2)+
    geom_point(aes(colour=as.factor(insecticides.in.sim)),alpha = 0.3)+
    geom_vline(xintercept = 15000, linetype = "dashed") +
    geom_vline(xintercept = 30000, linetype = "dashed")+
    xlab("Simulation Replicate Pair")+ 
    ylab("Difference in Mean Bioassay Survival Percentage to Deployed Insecticide")+ 
    ylim(-6, 6)+#set as the maximum range
    geom_text(data = label.df1, aes(label = text.label,
                                    x=label_x_coord,
                                    y=label_y_coord),
              angle = 270, size = 5)+
    geom_label(data = label.df.depfreq1, aes(label = text.label,
                                             x=label_x_coord,
                                             y=label_y_coord),
               fill = "orchid")+
    theme_bw()+
    theme(legend.position = "none")
  
 return(figure.3)
}
make_figure3_plot()

make_figure4_plot = function(){
  replicate = seq(1, 45000, by = 1)
  
  all.sims.join = data.frame(all.sims.join, replicate)
  
  sim.equal.gens = all.sims.join%>%
    dplyr::filter(simulation.duration.sequence == simulation.duration.rotation)
  
  
  #Creates a dataset for putting in labels for putting in the 
  label.df3 = data.frame(
    text.label = c("Favours Sequences", "Favours Rotations"),
    label_x_coord = c(45500, 45500), #have the label fairly central.
    label_y_coord = c(-0.05, 0.05)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq3= data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(0.12, 0.12, 0.12)
  )
  
  #Plot of the difference in the peak bioassay survival
  #this is Figure 4
  figure.4 = ggplot(sim.equal.gens, aes(x = replicate.1, y= diff.peak.survival)) +
    geom_point(aes(colour = as.factor(insecticides.in.sim)), alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
    xlab("Simulation Replicate")+ 
    ylab("Difference in the Peak Bioassay Survival Percentage")+ 
    ylim(-0.12, 0.12)+#set as the maximum range
    geom_vline(xintercept = 15000, linetype = "dashed") +
    geom_vline(xintercept = 30000, linetype = "dashed")+
    xlab("Simulation Replicate Pair")+ 
    ylab("Difference in Peak Bioassay Survival Percentage")+ 
    geom_text(data = label.df3, aes(label = text.label,
                                    x=label_x_coord,
                                    y=label_y_coord),
              angle = 270, size = 5)+
    geom_label(data = label.df.depfreq3, aes(label = text.label,
                                             x=label_x_coord,
                                             y=label_y_coord),
               fill = "orchid")+
    theme_bw()+
    theme(legend.position = "none")
  
  return(figure.4)
}
make_figure4_plot()

make_figure5_plot = function(){
  replicate = seq(1, 45000, by = 1)
  
  all.sims.join = data.frame(all.sims.join, replicate)
  
  sim.equal.gens = all.sims.join%>%
    dplyr::filter(simulation.duration.sequence == simulation.duration.rotation)
  
  label.df4 = data.frame(
    text.label = c("Favours Rotations", "Favours Sequences"),
    label_x_coord = c(45500, 45500), #have the label fairly central.
    label_y_coord = c(85, -85)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq4= data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(170, 170, 170)
  )
  
  #Plot of the difference in the number of control failure generations
  #This is figure 5
 figure.5 = ggplot(sim.equal.gens, aes(x = replicate.1, y= diff.exceed.gens.deployed)) +
    geom_point(alpha = 0.3,
               aes(colour = as.factor(no.insecticides))
    )+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
    xlab("Simulation Replicate Pair")+ 
    ylab("Difference in the Number of Control Failure Generations")+ 
    ylim(-170, 170)+#set as the maximum range
    geom_vline(xintercept = 15000, linetype = "dashed") +
    geom_vline(xintercept = 30000, linetype = "dashed")+
    geom_text(data = label.df4, aes(label = text.label,
                                    x=label_x_coord,
                                    y=label_y_coord),
              angle = 270, size = 4)+
    geom_label(data = label.df.depfreq4, aes(label = text.label,
                                             x=label_x_coord,
                                             y=label_y_coord),
               fill = "orchid")+
    theme_bw()+
    theme(legend.position = "none")
 
 return(figure.5)
}
make_figure5_plot()

make_figure6_plot = function(){

pcor.df.all = pcor.df.all%>%
  dplyr::rename(`IRM Strategy` = IRM.Strategy)

#This is Figure 65
pals = c("#b8e186", "#7fbc41", "#4d9221", "#f1b6da", "#de77ae", "#c51b7d")


#https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
#Artem Sokolov
shift_legend3 = function(p) {
  pnls = cowplot::plot_to_gtable(p) %>% 
    gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% 
    purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

facet.labels.deployment.interval = c("5 Generation Deployment Interval",
                                     "10 Generation Deployment Interval",
                                     "20 Generation Deployment Interval")

#This is FIGURE 6
figure.6 = shift_legend3(ggplot(pcor.df.all, aes(x=estimate, y=parameter, xmin = lower.95.ci,
                                      xmax = upper.95.ci, fill = `IRM Strategy`)) +
                geom_bar(stat = "identity", position = position_dodge(), aes(x=estimate)) +
                geom_errorbarh(position = position_dodge())+
                scale_fill_manual(values = pals)+
                geom_vline(xintercept = 0) +
                xlim(-0.75, 0.75)+
                ylab("Parameter") +
                xlab("Correlation") +
                guides(fill=guide_legend(ncol=2)) +
                theme_bw()+
                facet_wrap(~deployment.interval,
                           ncol = 2))
return(figure.6)
}
make_figure6_plot()

make_figure7_plot = function(){

filter_cross_df = function(cr.val,
                           strat){
  filtered.cross.df = sequence.rotation.cross.full.df%>%
    dplyr::filter(cross.resistance == cr.val)%>%
    dplyr::filter(strategy == strat)%>%
    dplyr::filter(simulation.duration == 500)
  
  prop.completion = nrow(filtered.cross.df)/5000
  
  return(prop.completion)
}

count.max.dur = c(filter_cross_df(-0.3, "sequence"),
                  filter_cross_df(-0.2, "sequence"),
                  filter_cross_df(-0.1, "sequence"),
                  filter_cross_df(0, "sequence"),
                  filter_cross_df(0.1, "sequence"),
                  filter_cross_df(0.2, "sequence"),
                  filter_cross_df(0.3, "sequence"),
                  filter_cross_df(-0.3, "rotation"),
                  filter_cross_df(-0.2, "rotation"),
                  filter_cross_df(-0.1, "rotation"),
                  filter_cross_df(0, "rotation"),
                  filter_cross_df(0.1, "rotation"),
                  filter_cross_df(0.2, "rotation"),
                  filter_cross_df(0.3, "rotation"))

`IRM Strategy` = c(rep("sequence", 7), rep("rotation", 7))
cross.vals = rep(seq(-0.3, 0.3, 0.1), 2)

count.max.dur.df = data.frame(count.max.dur,
                              stat.vec,
                              cross.vals)


pals.1 = c("#4d9221", 
           "#c51b7d")

figure.x = ggplot(data=count.max.dur.df, aes(x=cross.vals, y=count.max.dur*100,
                                  colour = `IRM Strategy`))+
  geom_point(size = 3)+
  geom_line()+
  scale_colour_manual(values = pals.1)+
  scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  ylim(0, 100)+
  xlab("Cross Selection")+
  ylab("Simulations Reaching Maximum Duration (%)")+
  guides(fill=guide_legend(nrow=1)) +
    theme_bw()+
  theme(legend.position = "bottom")

return(figure.x)
}
make_figure7_plot()

make_figure8_plot = function(){
  
  #This is Figure 65
  pals = c("#b2182b",
           "#ef8a62",
           "#fddbc7",
           "#5aae61",
           "#d1e5f0",
           "#67a9cf",
           "#2166ac")
   
  pcor.cross.df = pcor.cross.df%>%
    dplyr::rename(`Cross Selection` = cross.selection)
  
  figure.y = ggplot(pcor.cross.df, aes(x=estimate, y=parameter, xmin = lower.95.ci,
                                       xmax = upper.95.ci, fill = `Cross Selection`)) +
    geom_bar(stat = "identity", position = position_dodge(), aes(x=estimate),
             colour = "black") +
    geom_errorbarh(position = position_dodge())+
    scale_fill_manual(values = rev(pals))+
    scale_y_discrete(labels = scales::wrap_format(10))+
    geom_vline(xintercept = 0) +
    xlim(-0.5, 0.8)+
    ylab("Parameter") +
    xlab("Correlation") +
    guides(fill=guide_legend(nrow=1)) +
    theme_bw()+
    theme(legend.position = "bottom")+
    facet_wrap(~strategy,
               ncol = 2)
  return(figure.y)
}
make_figure8_plot()



