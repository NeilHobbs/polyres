#'@title An exampe of the impact of a decaying insecticide efficacy on the field survival of mosquitoes

example_insecticide_efficacy_field_survival_plot = function(){

  
  

  field.surv.vals_0 = convert_bioassay_survival_to_field_insecticide_decay(conversion.factor = 0.48,
                                                                           intercept = 0.15,
                                                                           bioassay.survival = 0,
                                                                           current.insecticide.efficacy = seq(1.2, 0, -0.001))
  
  field.surv.vals_10 = convert_bioassay_survival_to_field_insecticide_decay(conversion.factor = 0.48,
                                                                            intercept = 0.15,
                                                                            bioassay.survival = 0.1,
                                                                            current.insecticide.efficacy = seq(1.2, 0, -0.001))
  
  field.surv.vals_20 = convert_bioassay_survival_to_field_insecticide_decay(conversion.factor = 0.48,
                                                                            intercept = 0.15,
                                                                            bioassay.survival = 0.2,
                                                                            current.insecticide.efficacy = seq(1.2, 0, -0.001))
  
  
  field.surv.vals_50 = convert_bioassay_survival_to_field_insecticide_decay(conversion.factor = 0.48,
                                                                            intercept = 0.15,
                                                                            bioassay.survival = 0.5,
                                                                            current.insecticide.efficacy = seq(1.2, 0, -0.001))
  
  
  
  efficacy.vals = seq(1.2, 0, -0.001)
  
  df = data.frame(field.surv.vals_0,
                  field.surv.vals_10,
                  field.surv.vals_20,
                  field.surv.vals_50, efficacy.vals)
  
  example.plot = ggplot(df, aes(x=field.surv.vals_0,
                                y=field.surv.vals))+
    geom_line(aes(x=efficacy.vals, y = field.surv.vals_0),
               colour = "#ff7f00",
               alpha = 0.7,
              size = 2)+
    geom_line(aes(x=efficacy.vals, y = field.surv.vals_10),
               colour = "#6a3d9a",
               alpha = 0.7,
              size = 2)+
    geom_line(aes(x=efficacy.vals, y = field.surv.vals_20),
               colour = "#ffff33",
               alpha = 0.7,
              size = 2)+
    geom_line(aes(x=efficacy.vals, y = field.surv.vals_50),
               colour = "#377eb8",
               alpha = 0.7,
              size = 2)+
    ylab("Field survival of a fully \nsusceptible popopulation")+
    xlab(expression(Insecticide~Efficacy~omega[tau]^{"i"}))+
    scale_y_continuous(breaks = seq(0, 1, by = 0.1))+
    scale_x_continuous(breaks = seq(0, 1.2, by = 0.1))+
    theme_classic()+
    theme(axis.title.x = element_text(face="bold",
                                      size=14),
          axis.title.y = element_text(face="bold", 
                                      size=14))
  
  return(example.plot)
}

#example_insecticide_efficacy_field_survival_plot()


