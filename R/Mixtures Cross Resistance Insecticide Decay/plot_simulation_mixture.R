plot_simulation_mixture = function(simulation.dataframe, #created from the get_simulation_dataframe() function
                           half.population.bioassay.survival.resistance, #must be same as used in the simulation
                           withdrawal.threshold, #must be the same as used in the simulation
                           return.threshold){ #must be the same as used in the simulation
  
  
  #Convert the insecticide resistance intensity into bioassay survival(%).
  #This is more intuitive and operationally relevant to visualise.
  simulation.dataframe = simulation.dataframe%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                      mean.population.resistance = resistance.intensity,
                                                                      michaelis.menten.slope = 1, 
                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                      sd.population.resistance = 0, 
                                                                      nsim = 1) * 100)#As a percentage)
  #Make as separate dataframes: 1 for treatment, 1 for refugia
  temp.df.treatment = simulation.dataframe%>%
    dplyr::filter(site == "treatment")
  
  temp.df.refugia = simulation.dataframe%>%
    dplyr::filter(site == "refugia")
  
 mixture.info = simulation.dataframe%>%
    dplyr::select("deployed.mixture.id", "deployed.mixture.part.1", "deployed.mixture.part.2")%>%
    dplyr::distinct()
 
   pals = c("#e41a1c","#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf")
  
  mixture.pals = pals[seq(1,max(simulation.dataframe$deployed.mixture.id),by=1)]    
   
  
  #Create a plot for the treatment site: this will have the deployment sequence and the threshold lines.
  treatment.plot = ggplot(data = temp.df.treatment, aes(x=time.in.generations, 
                                                        y = bioassay.survival))+
    geom_point(aes(x=time.in.generations, y=(withdrawal.threshold*100,
                                             colour = deployed.mixture.id), #Make a line of the deployed insecticide at %
                    alpha = 0.3)) +
    #scale_color_manual(values = mixture.pals)+
    geom_point(aes(x=time.in.generations, #Line indicating the return threshold.
                   y=(return.threshold * 100)), 
               colour="lightgrey", 
               alpha = 0.3) +  #keep it fairly faint.
    geom_line(data =temp.df.treatment, aes(x=time.in.generations, 
                                           y=bioassay.survival, #aleady in %
                                           colour=insecticide.tracked))+ #matches deployed colour
    scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$bioassay.survival) < (withdrawal.threshold*100),
                                            yes = (withdrawal.threshold*100), no = max(simulation.dataframe$bioassay.survival) + 1)),
                       breaks = c(0, (return.threshold*100), (withdrawal.threshold*100)))+ #make sure there are labels at the important bits
    ylab("Survival in Bioassay (%)") +
    xlab("Time in Generations") +
    ggtitle("Intervention Site")+ #May need  to be changed as we finalise on what the sites are called.
    theme_classic()+
    theme(legend.position = "none") #Colour label is irrelevant; insecticide number can be inferred from the order of deployment
  
  #Create a plot for the refugia. Does not need to have deployment/threshold lines.
  refugia.plot = ggplot(data = temp.df.refugia, aes(x=time.in.generations, 
                                                    y = bioassay.survival))+
    geom_line(data = temp.df.refugia, mapping = aes(x=time.in.generations, y=bioassay.survival, colour = insecticide.tracked)) +
    scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$bioassay.survival) < (withdrawal.threshold*100),
                                            yes = (withdrawal.threshold*100), no = max(simulation.dataframe$bioassay.survival) + 1)),
                       breaks = c(0, (return.threshold*100), (withdrawal.threshold*100)))+ #scale limits match Intervention Site
    ylab(" ") +
    xlab("Time in Generations") +
    ggtitle("Refugia")+
    theme_classic()+
    theme(legend.position = "none")
  

  insecticide.info = mixture.info%>%
    dplyr::select("deployed.mixture.part.1", "deployed.mixture.part.2")
  
  val = data.table::transpose(insecticide.info)
  
  nam = rep(c("Mixture Part 1", "Mixture Part 2"), times = max(simulation.dataframe$deployed.mixture.id))
  
  
  insecticide.info = data.frame(nam, val)
  

  mix.legend = cowplot::get_legend(ggplot(mixture.info, aes(x=deployed.mixture.id, y = "1",
                                                            fill = deployed.mixture.id))+
                                     geom_bar(stat="identity")+
                                     scale_fill_manual(values = mixture.pals))
  
  part.legend = cowplot::get_legend(ggplot(insecticide.info, aes(x=nam, y = "1",
                                                                 fill = as.factor(V1)))+
                                      geom_bar(stat="identity")+
                                      guides(fill=guide_legend(nrow=1)))
  
  
  
  legend.plot = gridExtra::grid.arrange(mix.legend, part.legend,
                                        nrow=1)
                                                            
  
  
  lay = rbind(c(1,1,1,2,2,2,
                1,1,1,2,2,2,
                NA,NA,3,3,NA,NA))
  
  
  return(gridExtra::grid.arrange(treatment.plot, refugia.plot,
                                 legend.plot, ncol=2))
}

