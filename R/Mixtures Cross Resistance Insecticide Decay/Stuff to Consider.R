# ###Insecticide Decay####
# 
# months = seq(1, 10, by = 1)
# start.efficacy = 1 #Note:: This would be the efficacy against a fully susceptible population.
# decay.rate = 0.01
# 
# #Assuming a constant decay rate of the insecticide efficicacy (and therefore selection pressure)
# insecticide.potency = start.efficacy * exp(-decay.rate * months)
# 
# #Assuming decay starts after a set number of months (eg. 5)
# insecticide.efficacy = ifelse(months < 5, start.efficacy, start.efficacy*exp(-decay.rate * months))
# 
# insecticide.efficacy = start.potency * exp(-decay.rate * (months^2))
# 
# plot(x=months, y=insecticide.efficacy) +
# lines(x=months, y=insectinsecticide.efficacy)
# 
# 
# #This would then be used to scale the response_to_insecticide_selection
#   #so the final response would be insecticide.efficacy[generation]*response_to_insecticide_selection
# 
# 
# 
# ##Cross Resistance##
#   
#   #This would be a CRij such that CR to insecticide i from resistance j. So would need a matrix
#   #OR: A constant degree of cross resistance between all insecticides.
# 
# 
# 
# 
# 
# 
