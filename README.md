# [polyres]
## Incorporating polygenic resistance into Insecticide Resistance Management

## Description
**polyres** enables the user to incorporate poygenic insecticide resistance (and the associated resistance intensity) into insecticide resistance models. This is achieved through a quantitative genetics framework. Resistance intensity is converted to bioassay survival  using the Hill variant of the Michaelis-Menten equation. This package is developed primarily around the *Anopheles gambiae* mosquito, but is applicable to most bloodfeeding insects. 

## Getting Started

###### General Notes:
1. Functions in ```polyres``` are named with ```_``` between the names. Parameters and inputs are named with ```.``` between the names.
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

The ```bioassay_survival_to_resistance``` function converts the proportion of individuals surviving in an insecticide bioassay (eg. CDC Bottle Bioassay or WHO Cylinder Bioassay) into what the corresponding population resistance intensity is. The target resistance value to be calculated needs to lie inbetween the ```minimum.resistance.value``` and the ```maximum.resistance.value```:

                     

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

This will return a plot of the relationship between the Polygenic Resistance Score and bioassay survival.


## Step 4: Running Simulations

Depending upon the complexity of the scenario being looked at, different functions are available to allow for their running. 

In general there are 4 distinct simulation running methods.
1. ```run_simulation_intervention()```
2. ```run_simulation_intervention_special_cases()```
3. ```run_simulation_intervention_mixtures()```
4. ```run_simulation_intervention_mixtures_special_cases()```

Insecticide resistance management simulations are run currently using the ```run_simulation_intervention``` or ```run_simulation_intervention_special_cases``` functions. These function is designed around giving a large amount of user flexibility to be able to test IRM strategies under multiple scenarios. This function returns a list of the simulation array holding the polygenic resistance scores to the insecticides in both the refugia and the intervention site. The ```run_simulation_intervention_special_cases`` function allows for the incorporating of cross selection, insecticide decay and population suppression into the model. It should be noted that this function is slower than the ```run_simulation_intervention_test```; therefore if running simulations without the special cases it is recommended to use the ```run_simulation_intervention``` function to reduce computational time. The same can be said for the ```run_simulation_intervention_mixtures``` and ```run_simulation_mixtures_special_cases``` functions.

Sequences: The ```irm.strategy = "sequence"``` runs the simulations using the sequence strategy for choosing and deploying insecticides. An insecticide is continually deployed until the ```withdrawal.threshold``` is reached, then the next insecticide is deployed. 

Rotations: The ```irm.strategy = "rotation"``` runs the simulations using the rotation stragegy for choosing and deploying insecticides. The insecticide deployed must change at each deployment interval. It should be noted that if the input parameter ```number.of.insecticides = 1```, the simulation will only run for the duration of the ```deployment.frequency```. Therefore, comparing rotations and sequences when ```number.of.insecticides = 1``` is not recommended.  

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

Changing the minimum and maximum parameter to be different values from one another while keeping a small number of replicates ```nsim``` allows the model to run in a more stochastic fashion. If the minimum and maximum values values are the same, the model will run in a deterministic fashion. If the minimum and maximum parameter values are equal (for each set of parameter values), it is recommended to set ```nsim = 1``` to speed up the time of the simulations. It should be noted that the model is currently unable to run in a fully stochastic fashion, this is because while the values would change for each generation, they would also change for each insecticide (which is unexpected - and having dispersal different for each insecticide is not realistic). 

## Getting the data from the simulations

The simulation functions return an array of the resistance intensities in the sites, and a vector of the deployed insecticide. The ```get_simulation_dataframe``` function is used to convert this array and vector into a single usable dataframe; which can be used for plotting or additional analyses. 

```
simulation.output = run_simulaton_intervention_test()

simulation.dataframe = get_simulation_dataframe()
```
note if using running simulations with mixtures the ```get_simulation_dataframe_mixtures``` function should be used. 















