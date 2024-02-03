secondfilter = function(mainframe, cutoffpop, cutoffprev, cutoffmisspos, cutoffmisstemp){
  
  ### Function description: filters data frame for cutoffs for population, 
  # prevalence, missing positivity values, and missing temperatures
  
  ### Inputs:
  # mainframe - combined weather and covid dataset
  # cutoffpop - cutoff for population (remove counties with population less than 
  # this threshold)
  # cutoffprev - cutoff for prevalence (remove counties with median prevalence less 
  # than this threshold)
  # cutoffmisspos - cutoff for missing positivity values (remove counties with 
  # higher than this threshold percent missing positivity values)
  # cutoffmisstemp - cutoff for missing temperature values (remove counties with 
  # higher than this threshold percent missing temperature values)
  
  ### Outputs:
  # mainframe - filtered combined weather and covid dataset
  # removecounties - list of counties removed
  
  # identify counties with positive NA rate for positivity
  IsNa1 = is.na(mainframe$positivity)
  area1 = mainframe$area
  posNAcounties = propNA(IsNa1, area1)
  
  # confirm that there are no missing positive_tests or total_tests
  if (sum(is.na(mainframe$positive_tests)) !=0 | sum(is.na(mainframe$total_tests)) !=0){
    print('there are missing positive or total tests')
  }
  
  # identify counties with positive NA rate for temp
  IsNa = is.na(mainframe$TMAX) | is.na(mainframe$TMIN)
  area2 = mainframe$area
  tempNAcounties = propNA(IsNa, area2)
  #print(paste(tempNAcounties[,1], "is the counties with NA temperatures.", sep = ' '))
  
  # identify counties with population > cutoff, median prevalence > cutoff
  mediancovid = mainframe %>% group_by(area) %>% summarise(medprev = median(prev), 
                                                           medpop = median(population))
  
  # remove counties with >cutoff percent missing data, population limit above, median prev above
  removecounties1 = dplyr::filter(posNAcounties, (posNAcounties$propNa > cutoffmisspos))
  removecounties2 = dplyr::filter(tempNAcounties, (tempNAcounties$propNa > cutoffmisstemp))
  removecounties3 = dplyr::filter(mediancovid, (mediancovid$medpop < cutoffpop))
  removecounties4 = dplyr::filter(mediancovid, (mediancovid$medprev < cutoffprev))
  removecountiesA = dplyr::union(removecounties1[,"area"], removecounties2[,"area"]) 
  removecountiesB = dplyr::union(removecounties3[,"area"], removecounties4[,"area"])
  removecounties = dplyr::union(removecountiesA, removecountiesB)
  #print(paste(removecounties1[,1], "is removed due to missing positivity values", sep = " "))
  #print(paste(removecounties2[,1], "is removed due to missing temperature values", sep = " "))
  #print(paste(removecounties3[,1], "is removed due to the population cutoff", sep = " "))
  #print(paste(removecounties4[,1], "is removed due to the prevalence cutoff", sep = " "))
  
  mainframelog = !(mainframe$area %in% removecounties$area)
  
  mainframe2 = dplyr::filter(mainframe, mainframelog)
  
  return(list("mainframe" = mainframe2, "removecounties" = removecounties))
}