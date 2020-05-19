# [polyres]
## Incorporating polygenic resistance into Insecticide Resistance Management

## Description
**polyres** enables the user to incorporate poygenic insecticide resistance (and the associated resistance intensity) into insecticide resistance models. This is achieved through converting bioassay survival (eg. from CDC Bottle Bioassays or WHO Cylinders) to a resistance intensity score. This is implemented through the Hill variation of the Michaelis-Menten equation. 

## Getting Started
###### Step 1: Deciding upon intitial scale of insecticide resistance intensity. 
The function ```calculate_half_population_survival``` allows for the calculation of a user defined resistance scale. 
The user should specify what resistance value is required at a particular cut-off point. For example, an intuitive scale would involve a ```desired.resistance```value of 100 corresponding to a ```desired.survival.proportion``` of 10% (0.1 as a proportion). Using these values the insecticide resistance intensity at which 50% of exposed mosquitoes would survive in a bioassay is calculated. 

```
calculate_half_population_survival(desired.resistance = 100,
                                  desired.survival.proportion = 0.1,
                                  maximum.bioassay.survival.proportion = 1,
                                  michaelis.menton.slope = 1, 
                                  estimate.precision = 0.01, 
                                  sd.population.resistance = 0,
                                  nsim = 1000,
                                  minimum.resistance.value = 0, 
                                  maximum.resistance.value = 5000)
```
Running the above code would return a value of 900 (Your value will depend on your . Which will be the *half.population.bioassay.survival.resistance* parameter that is used throughout the value.

###### Step 2: Obtain the scale and visualise
The functions ```plot_resistance_to_bioassay_survival``` and ```plot_bioassay_survival_to_resistance``` allow for the plotting and visualising of the relationship between insecticide resistance intensity and bioassay survival by using the ```resistance_to_bioassay_survival``` and ```bioassay_survival_to_resistance``` functions respectively.

An example of how to plot your calculated scale follows, using the ```half.population.bioassay.survival.resistance``` = 900, as calculated using the ```calculate_half_population_survival``` function. 

```plot_resistance_to_bioassay_survival(
                                maximum.bioassay.survival.proportion = 1, 
                                michaelis.menton.slope = 1, 
                                half.population.bioassay.survival.resistance = 900, 
                                nsim = 1000, 
                                minimum.resistance = 0, 
                                maximum.resistance = 10000, 
                                sd.population.resistance = 10
```

This will return a plot of the relationship between resistance intensity and bioassay survival.


##### Step 3: See impact of population standard deviation on the resistance intensity values, predicted using bioassay survival
The ```table_resistance_from_survival_and_sd``` function allows the production of a table to enable visualisation of how the standard deviation within the population influences the calculated resistance intensity based on the observed bioassay survival.
The function requires the input of a vector of standard deviation values ```sd.population.values``` and a vector of bioassay survival values ```bioassay.survival.values```. These vectors need to be the same length; and be in a form such that when combined to make a dataframe they produce sequence such that each standard deviation value is matched with each bioassay survival value. An example of how that could be achieved is given below. 
```
#Both vectors must be the same length. 
sd.vector = rep(c(0.1, 0.5, 1, 5, 20, 25), times = 5) # total length 30
survival.vector = c(rep(0.05, 6), rep(0.1, 6), rep(0.2, 6), rep(0.5, 6), rep(0.8, 6)) # total length 30
```
These vectors can then be passed into the ```table_resistance_from_survival_and_sd``` function as follows:
```
table_resistance_from_survival_and_sd(
  half.population.bioassay.survival.resistance = 900, 
  maximum.bioassay.survival.proportion = 1, 
  michaelis.menton.slope = 1, 
  bioassay.survival.values = survival.vector, 
  sd.population.values = sd.vector, 
  estimate.precision = 0.01, 
  nsim = 1000, 
  minimum.resistance.value = 0, 
  maximum.resistance.value = 10000
)
```
This will then return a table of standard deviation (left column), bioassay survival (top row) with the filled in values being the insecticide resistance values. 






