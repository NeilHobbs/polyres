#data obtained from Churcher et al. figure 2A
#dataset only uses LLINs
  #field mortality is experimental huts - just because mosquito entered hut and didn't die,
  #does not mean that they entered hut and encountered bednets.
  #Only considers female mosquitoes.
library(magrittr)

churcher.data = readxl::read_excel("Data/elife-16090-fig2-data2-v2.xlsx")

#Calculate mortality proportions and then convert to survival:::
churcher.data = churcher.data%>%
  dplyr::mutate(hut.mortality = Total_Dead_In_Hut_Trial/Total_Collected_In_Hut_Trial)%>%
  dplyr::mutate(bioassay.mortality = Total_Dead_In_Bioassay/Total_Tested_In_Bioassay)%>%
  dplyr::mutate(hut.survival = 1 - hut.mortality)%>%
  dplyr::mutate(bioassay.survival = 1 - bioassay.mortality)

plot(churcher.data$bioassay.survival, churcher.data$hut.survival)


#Generalised additive model to view the relationship (straight line / wiggly)
churcher.gam = mgcv::gam(hut.survival ~ s(bioassay.survival), 
                         data = churcher.data,
                         method = "REML")
#Generalised additive model suggests a linear relationship.
plot(churcher.gam)

churcher.lm = lm(hut.survival ~ bioassay.survival,
                 data = churcher.data)

par(mfrow = c(2, 2)) #make 4x4 grid
plot(churcher.lm)
par(mfrow = c(1, 1)) #return plotting space back to normal

#histogram of residuals
hist(churcher.lm$residuals)#not overly Normally distributed but small number of obs

summary(churcher.lm)
  #intercept is 0.33
  #scaling factor is 0.46

#Try with just Anopheles gambiae sl:
churcher.data.sl = churcher.data%>%
  dplyr::filter(Mosquito_Species == "An_gambiae_sl")

churcher.lm.sl = lm(formula = hut.survival ~ bioassay.survival,
                    data = churcher.data.sl)

summary(churcher.lm.sl)




#Try with only Deltamethrin and Anopheles sl. 
  #Rationale: Reduces confounding of species and insecticide differences
churcher.data.delta.sl = churcher.data%>%
  dplyr::filter(Insecticide_In_Bioassay == "Deltamethrin")%>%
  dplyr::filter(Mosquito_Species == "An_gambiae_sl")


churcher.gam.updated = mgcv::gam(hut.survival ~ s(bioassay.survival), 
                         data = churcher.data.delta.sl,
                         method = "REML")
#Generalised additive model suggests a broadly linear relationship.
plot(churcher.gam.updated)

summary(churcher.gam.updated)

churcher.lm.updated = lm(hut.survival ~ bioassay.survival,
                 data = churcher.data.delta.sl)

par(mfrow = c(2, 2)) #make 4x4 grid
plot(churcher.lm.updated)
par(mfrow = c(1, 1)) #return plotting space back to normal

#histogram of residuals
hist(churcher.lm.updated$residuals)#not overly Normally distributed

summary(churcher.lm.updated)
  #intercept 0.15
  #scaling factor 0.48



#A 15% base survival rate to an insecticide in the field seems plausible,
  #variation in insecticide contact/duration of contact; and individual physiological status of mosquitoes.
  #E.g. 0 resistance intensity = 15% survival in the field
convert_bioassay_survival_to_field = function(bioassay.survival,
                                              conversion.factor = 0.48,
                                              intercept = 0.15){
  
  field.survival = (conversion.factor * bioassay.survival) + intercept #values obtained from linear model.
  
  #stop field.survival being outside of 0-1 range
  field.survival = ifelse(field.survival > 1, yes = 1, no = field.survival)
  field.survival = ifelse(field.survival < 0, yes = 0, no = field.survival)
  
  return(field.survival)
}


bio_surv = seq(0,1, 0.01)
field_surv = c()

for(i in 1:101){
  
  field_surv[i] = convert_bioassay_survival_to_field(bio_surv[i])
  
}

plot(bio_surv, field_surv) #when resistance.intensity is 0; field survival still above 0 (useful for mixtures)

