mixedmodelnestednorm = function(mainframe){
  # Function description: implements beta regression model, mixed effects. 
  # Includes positivity rate as dependent variable.
  # includes maximum ambient temperature (averaged over county), log(population),
  # county, day of week, COVID prevalence
  
  # Inputs: 
  # mainframe - cleaned combined weather and covid data
  
  # Outputs:
  # list containing the following:
  # data - data used to train the model, equivalent to mainframe
  # model - trained beta regression model 
  
  datamodel = glmmTMB(pos ~ TMAX + (1 | cty/DOW) + logpop + prevscaled, 
                      data = mainframe, family = beta_family(link="logit"))
  
  return(list("data" = mainframe, "model" = datamodel))
}