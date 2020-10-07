# [polyres]
## Incorporating polygenic resistance into Insecticide Resistance Management

## Description
**polyres** enables the user to incorporate poygenic insecticide resistance (and the associated resistance intensity) into insecticide resistance models. This is achieved through converting bioassay survival (eg. from CDC Bottle Bioassays or WHO Cylinders) to a resistance intensity score using the Hill variant of the Michaelis-Menten equation. This package is developed based around the *Anopheles gambiae* mosquito. 

## Getting Started

###### General Notes:
1. Functions in ```polyres``` are named with ```_``` between the names. Parameters are named with ```.``` between the names.
2. Unless otherwise stated, all survival and mortality values used are proportions.

###### Step 1: Deciding upon intitial scale of insecticide resistance intensity. 
The function ```calculate_half_population_survival``` allows for the calculation of a user defined resistance scale. 
The user should specify what resistance value is required at a particular cut-off point. For example, an intuitive scale would involve a ```desired.resistance```value of 100 corresponding to a ```desired.survival.proportion``` of 10% (0.1 as a proportion). Using these values the insecticide resistance intensity at which 50% of exposed mosquitoes would survive in a bioassay is calculated. 

```
calculate_half_population_survival(desired.resistance = 100,
                                  desired.survival.proportion = 0.1,
                                  maximum.bioassay.survival.proportion = 1,
                                  michaelis.menten.slope = 1, 
                                  estimate.precision = 0.01, 
                                  sd.population.resistance = 0,
                                  nsim = 1000,
                                  minimum.resistance.value = 0, 
                                  maximum.resistance.value = 5000)
```
Running the above code would return a value of 900 (Your value will depend on your ```desired.resistance``` and ```desired.survival.proportion``` combination . Which will be the *half.population.bioassay.survival.resistance* parameter that is used throughout the rest of the model and code.

##### Step 2: The ```bioassay_survival_to_resistance``` and ```resistance_to_bioassay_survival``` functions

The ```resistance_to_bioassay_survival``` converts the population resistance intensity into a corresponding survival in an insecticide bioassay. This function implements the Hill variant of the Michaelis-Menten equation with the distribution of individual level resistance intensity having a Normal distribution.

```
resistance_to_bioassay_survival = function(
                                           maximum.bioassay.survival.proportion = 1,
                                           mean.population.resistance = 100,
                                           michaelis.menten.slope = 1, 
                                           half.population.bioassay.survival.resistance = 900,
                                           sd.population.resistance = 5, 
                                           nsim = 1000
                                           )
```

The ```bioassay_survival_to_resistance``` function converts the proportion of individuals surviving in an insecticide bioassay (eg. CDC Bottle Bioassay or WHO Cylinder Bioassay) into what the corresponding population resistance intensity is. The target resistance value to be calculated needs to lie inbetween the ```minimum.resistance.value``` and the ```maximum.resistance.value```:

```
bioassay_survival_to_resistance(
                                maximum.bioassay.survival.proportion = 1,
                                michaelis.menten.slope = 1, 
                                half.population.bioassay.survival.resistance = 900, 
                                bioassay.survival = 0.1, 
                                estimate.precision = 0.01, 
                                sd.population.resistance = 5,
                                nsim = 1000,
                                minimum.resistance.value = 0, 
                                maximum.resistance.value  = 10000
                                )
```                                           

###### Step 3: Obtain the scale and visualise
The functions ```plot_resistance_to_bioassay_survival``` and ```plot_bioassay_survival_to_resistance``` allow for the plotting and visualising of the relationship between insecticide resistance intensity and bioassay survival by using the ```resistance_to_bioassay_survival``` and ```bioassay_survival_to_resistance``` functions respectively.

An example of how to plot your calculated scale follows, using the ```half.population.bioassay.survival.resistance``` = 900, as calculated using the ```calculate_half_population_survival``` function. 

```
plot_resistance_to_bioassay_survival(
                                maximum.bioassay.survival.proportion = 1, 
                                michaelis.menten.slope = 1, 
                                half.population.bioassay.survival.resistance = 900, 
                                nsim = 1000, 
                                minimum.resistance = 0, 
                                maximum.resistance = 10000, 
                                sd.population.resistance = 10
```

This will return a plot of the relationship between resistance intensity and bioassay survival.


##### Step 4: See impact of population standard deviation on the resistance intensity values, predicted using bioassay survival
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
                                       michaelis.menten.slope = 1, 
                                       bioassay.survival.values = survival.vector, 
                                       sd.population.values = sd.vector, 
                                       estimate.precision = 0.01,
                                       nsim = 1000, 
                                       minimum.resistance.value = 0, 
                                       maximum.resistance.value = 10000
                                       )                                      

```
This will then return a table of standard deviation (left column), bioassay survival (top row) with the filled in values being the insecticide resistance values. 

##### Step 5: Visualising the impact of an incorrect ```michaelis.menten.slope``` value
There are two functions that enable this, the ```plot_michaelis.menten_slope_resistance``` and the ```plot_michaelis.menten_slope_survival```. 

The ```plot_michaelis.menten_slope_resistance``` uses the ```bioassay_survival_to_resistance``` function:

```
michaelis.menton.values = rep(seq(0, 2, by = 0.1), 6)
st.dev.resistance = c(rep(0.1, 21), rep(0.5, 21), rep(1, 21), rep(5, 21), rep(20, 21), rep(25, 21))

plot_michaelis_menten_slope_resistance(maximum.bioassay.survival.proportion = 1,
                                       michaelis.menten.slope.values = michaelis.menten.values,
                                       half.population.bioassay.survival.resistance = 900,
                                       bioassay.survival = 0.5, 
                                       estimate.precision = 0.01, 
                                       sd.population.resistance.values = st.dev.resistance, 
                                       nsim = 1000, 
                                       minimum.resistance.value = 0,
                                       maximum.resistance.value = 10000)
 ```
                                       
The final returned plot is the Michaelis Menten Slope (x axis) and the associated resistance values (y axis). If using ```half.population.bioassay.survival.resistance = 900``` and ```bioassay.survival = 0.5``` then the only time the graph plots this correctly is when the Michaelis Menten slope value = 1. 


The ```plot_michaelis_menten_slope_survival``` uses the ```resistance_to_bioassay_survival``` function:

```

michaelis.menten.values = rep(seq(0, 2, by = 0.1), 6)
st.dev.values = c(rep(0.1, 21), rep(0.5, 21), rep(1, 21), rep(5, 21), rep(20, 21), rep(25, 21))

plot_michaelis_menten_slope_survival(
                                     michaelis.menten.slope.values = michaelis.menten.values, 
                                     sd.population.resistance.values = st.dev.values, 
                                     maximum.bioassay.survival = 1,
                                     half.population.bioassay.survival.resistance = 900, 
                                     mean.population.resistance = 900,
                                     nsim = 1000
                                     )
```
Alternatively, the impact of varying the ```michaelis.menten.slope``` could be visualised using a table: ```table_michaelis_menten_slope_survival```; with the implementation similar to the ```plot_michaelis_menten_slope_survival``` example.

##### Step 6: Tracking Changes in Resistance Intensity with the Breeder's Equation:
The function ```response_to_insecticide_selection``` function implements the Breeder's equation, modified to incorporate differential selection pressures on male and female mosquitoes (as only female *Anopheles gambiae* blood feed and so are more likely to come into contact with indoor based insecticides (Indoor Residual Spraying or Insecticide Treated Bednets). The exposures and heritabilities are assumed to have a uniform distribution.  

```
response_to_insecticide_selection(
 exposure.scaling.factor = 10, # Empirically set.
 nsim = 1000, 
 minimum.insecticide.resistance.hertitability = 0.05, 
 maximum.insecticide.resistance.hertitability = 0.3,
 minimum.male.insecticide.exposure = 0,
 maximum.male.insecticide.exposure = 1, 
 minimum.female.insecticide.exposure = 0.4, 
 maximum.female.insecticide.exposure = 0.9
 )
```
This function will return a vector of length of ```nsim``` of the change in the resistance intensity. The change in resistance intensity values can be plotted using either the ```plot_response_to_insecticide_selection``` or ```plot_log_response_to_insecticide_selection```. 

## Running Simulations

Insecticide resistance management simulations are run using the ```run_simulation_intervention``` function. This function is designed around giving a large amount of user flexibility to be able to test IRM strategies under multiple scenarios. This function returns a list of the simulation array holding the resistance intensities to the insecticides in both the refugia and the treatment site. 

Sequences: The ```irm.strategy = "sequence"``` runs the simulations using the sequence strategy for choosing and deploying insecticides. An insecticide is continually deployed until the ```withdrawal.threshold``` is reached, then the next insecticide is deployed. 

Rotations: The ```irm.strategy = "rotation"``` runs the simulations using the rotation stragegy for choosing and deploying insecticides. The insecticide deployed must change at each deployment interval. 

```
simulation.output = run_simulation_intervention(
                    number.of.insecticides = 3,
                    exposure.scaling.factor = 10,
                    nsim = 1,
                    minimum.insecticide.resistance.hertitability = 0.3,
                    maximum.insecticide.resistance.hertitability = 0.30,
                    minimum.male.insecticide.exposure = 0.9,
                    maximum.male.insecticide.exposure = 0.9,
                    minimum.female.insecticide.exposure = 0.9,
                    maximum.female.insecticide.exposure = 0.9,
                    resistance.cost = 0.2,
                    starting.treatment.site.intensity = 0,
                    starting.refugia.intensity = 0,
                    min.intervention.coverage = 0.8,
                    max.intervention.coverage = 0.8,
                    min.dispersal.rate = 0.9,
                    max.dispersal.rate = 0.9,
                    maximum.generations = 1000,
                    irm.strategy = "rotation", #will be "sequence" or "rotation" (plus mixture later on),
                    half.population.bioassay.survival.resistance = 900, #This is the resistance intensity that would give 50% survival in the bioassay,
                                                                         #and can be calculated with calculate_half_population_survival().
                    withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                    return.threshold.value = 0.1, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                    deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                    maximum.resistance.value = 25000 
```


## Getting the data from the simulations




















