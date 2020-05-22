#' Function to plot a histogram of the response_to_insecticide_selection output.
#' Plot to give years to 10% Survival (assuming 10 generations per year)
#'
#' @import ggplot2
#' 
#' @param response.values As obtained from the response_to_insecticide_selection function

plot_log_response_to_insecticide_selection = function(response.values){
  
  response.values = 10/response.values
  
  response.values = log10(response.values)
  
  #Coerce into a dataframe to allow for plotting
  output=data.frame(response.values) 
  
  #Histogram of years
  selection.histogram =  ggplot(data = output, aes(x=response.values)) +
    geom_histogram(fill = "grey", colour = "black", binwidth = 0.1) +
    xlab("log(Years to 10% Survival)") +
    theme_classic()
  
  return(selection.histogram)
}




