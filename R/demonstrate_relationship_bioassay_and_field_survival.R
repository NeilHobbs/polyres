#'@title Demonstrate the relationship between the bioassay survival and field survival
#'
#'@description Plot a graph of the relationship between the bioassay survival and field survival. 
#'
#'@param intercept = The intercept obtained from a linear model 
#'@param conversion.factor = The regression coeffient obtained from a linear model
#'
#'@import ggplot2


demonstrate_relationship_bioassay_and_field_survival = function(intercept,
                                                                conversion.factor){
  
  bioassay.survival.values = seq(0, 1, by = 0.001)
  
 field.survival.values =  convert_bioassay_survival_to_field(bioassay.survival = bioassay.survival.values,
                                     intercept = intercept,
                                     conversion.factor = conversion.factor)
  
  
  df = data.frame(bioassay.survival.values, field.survival.values)
  
  example.plot = ggplot(df, aes(x=bioassay.survival.values*100,
                 field.survival.values*100))+
    geom_point(colour = "#1d91c0")+
    theme_classic()+
    xlab("Bioassay Survival (%)")+
    ylab("Field Survival (%)")
  
  return(example.plot)
}

# demonstrate_relationship_bioassay_and_field_survival(intercept = 0.15,
#                                                      conversion.factor = 0.48)
# 
