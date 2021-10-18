#'@title Demonstrate the protective effect of mixtures.
#'
#'@description Allows the simple visual demonstration of how insecticides in mixtures are protective of one another
#'
#'@param male.insecticide.exposure = The proportion of males exposed to the insecticides as a proportion of female exposures
#'@param female.insecticide.exposure = The proportion of females exposed to the insecticides.
#'@param intervention.coverage = The coverage of the insecticides as a proportion
#'@param heritability = The heritability of the insecticide resistance traits
#'@param fitness.cost = The fitness cost associated with insecticide resistance
#'@param dispersal.rate = The proportion of individuals exchanging between the intervention site and refugia.
#'@param half.population.bioassay.survival.resistance = The polygenic resistance score which gives 50% bioassay survival
#'@param maximum.generations = The maximum number of generations of the simulations

demonstrate_protective_effect_of_mixtures = function(male.insecticide.exposure,
                                                     female.insecticide.exposure,
                                                     intervention.coverage,
                                                     heritability,
                                                     fitness.cost,
                                                     dispersal.rate,
                                                     half.population.bioassay.survival.resistance,
                                                     maximum.generations){
  
  #Start polygenic resistance score values corresponding to bioassay survival:
  #0% = 0 ; 5% = 47 ; 10% = 100, 25% = 300; 
  #50% = 900 ; ; 75% = 2700;
  #90% = 8100
  
  start.resistance.vec = c(0, 47, 100,
                           300, 900,
                           2700, 8100)
  sim.list = list()
  for(v in 1:length(start.resistance.vec)){
    temp = get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
                                                                                                       exposure.scaling.factor = 10,
                                                                                                       nsim = 1,
                                                                                                       minimum.insecticide.resistance.heritability = heritability,
                                                                                                       maximum.insecticide.resistance.heritability = heritability,
                                                                                                       minimum.male.insecticide.exposure = male.insecticide.exposure,
                                                                                                       maximum.male.insecticide.exposure = male.insecticide.exposure,
                                                                                                       minimum.female.insecticide.exposure = female.insecticide.exposure,
                                                                                                       maximum.female.insecticide.exposure = female.insecticide.exposure,
                                                                                                       resistance.cost = 0.1,
                                                                                                       starting.treatment.site.intensity = c(start.resistance.vec[v], 0),
                                                                                                       starting.refugia.intensity = c(start.resistance.vec[v], 0),
                                                                                                       min.intervention.coverage = intervention.coverage,
                                                                                                       max.intervention.coverage = intervention.coverage,
                                                                                                       min.dispersal.rate = dispersal.rate,
                                                                                                       max.dispersal.rate = dispersal.rate,
                                                                                                       maximum.generations = maximum.generations,
                                                                                                       irm.deployment.strategy = "mixtures", #single, mixtures
                                                                                                       mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
                                                                                                       irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
                                                                                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                       withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                                       return.threshold.value = 0.99, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                                       deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                                       maximum.resistance.value = 25000,
                                                                                                       conversion.factor = 0.48,
                                                                                                       intercept = 0.15,
                                                                                                       min.cross.selection = 0,
                                                                                                       max.cross.selection = 0), 
                                             maximum.generations = maximum.generations, number.of.insecticides = 2) 
    
    temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
      dplyr::filter(site == "treatment")
    
    temp_treatment$start.resistance = start.resistance.vec[v]
    
    sim.list[[v]] = temp_treatment
  }
  
  
  A  = do.call(rbind, sim.list)
  A = A%>%
    dplyr::filter(insecticide.tracked == 2)
  
  seq.run = get_simulation_dataframe(
    run_simulation_intervention_cross_selection(number.of.insecticides = 1,
                                                exposure.scaling.factor = 10,
                                                nsim = 1,
                                                minimum.insecticide.resistance.heritability = heritability,
                                                maximum.insecticide.resistance.heritability = heritability,
                                                minimum.male.insecticide.exposure = male.insecticide.exposure,
                                                maximum.male.insecticide.exposure = male.insecticide.exposure,
                                                minimum.female.insecticide.exposure = female.insecticide.exposure,
                                                maximum.female.insecticide.exposure = female.insecticide.exposure,
                                                resistance.cost = fitness.cost,
                                                starting.treatment.site.intensity = 0,
                                                starting.refugia.intensity = 0,
                                                min.intervention.coverage = intervention.coverage,
                                                max.intervention.coverage = intervention.coverage,
                                                min.dispersal.rate = dispersal.rate,
                                                max.dispersal.rate = dispersal.rate,
                                                maximum.generations = maximum.generations,
                                                irm.strategy = "sequence",
                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                return.threshold.value = 0.99, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                maximum.resistance.value = 25000,
                                                min.cross.selection = 0,
                                                max.cross.selection = 0), 
    maximum.generations = maximum.generations, 
    number.of.insecticides = 1)    
  
  seq.run = seq.run%>%
    dplyr::filter(site == "treatment")
  
  Parameters = c("Heritability",
                 "Female Insecticide Exposure",
                 "Male Insecticide Exposure",
                 "Dispersal Rate",
                 "Intervention Coverage",
                 "Fitness Cost")
  
  Values = c(heritability,
            female.insecticide.exposure,
            male.insecticide.exposure,
            dispersal.rate,
            intervention.coverage,
            fitness.cost)
  
 label.df = cbind(Parameters, Values)
  
  
 pals = c("#d4b9da",
   "#c994c7",
  "#df65b0",
   "#e7298a",
   "#ce1256",
   "#980043",
   "#67001f")
  
  demonstration.plot = ggplot(A, aes(x = time.in.generations,
                                     y= resistance.intensity,
                                     colour =as.factor(start.resistance)))+
    geom_line(size = 2)+
    scale_colour_manual(values = pals)+
    geom_line(data = seq.run,
              aes(x=time.in.generations,
                  y=resistance.intensity),
              colour = "#5aae61", size = 2)+
    xlab("Number of Generations")+
    ylab("Population Mean Polygenic Resistance Score (z)")+
    geom_hline(yintercept = 100, colour = "grey",
               size = 2)+
    theme_classic()+
    theme(legend.position = "none")+
    annotation_custom(gridExtra::tableGrob(label.df, rows=NULL), 
                      xmin=50, xmax=150, ymin=400, ymax=600)
 
  return(demonstration.plot)
}


