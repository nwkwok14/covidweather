propNA = function(IsNa, area){
  ### function description: calculates percent NA for a variable of interest for 
  # unique areas
  
  ### Inputs:
  # IsNa - logicals representing whether variable of interest has NA value
  # area - corresponding areas
  
  ### Outputs:
  # NAcounties - dataframe with unique areas and corresponding % NA values
  
  areapos = data.frame(area, IsNa, NotNa=!IsNa)
  areasum = areapos %>% group_by(area) %>% summarise(propNa = sum(IsNa)/(sum(IsNa)+sum(NotNa)))
  NAcounties = dplyr::filter(areasum, areasum$propNa>0)
  
  return(NAcounties)}