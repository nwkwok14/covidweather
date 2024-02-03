cleancovidtendayprev = function(covidfilename, firstdate, seconddate){
  # Function description: 
  # reads COVID data file
  # downselects variables to date, area, population, total tests, positive tests, cases
  # removes out of state data
  # add prevalence (as 10 day cumulative sum), positivity rate to dataset
  # omits Sutter county, which has no temperature data
  
  # Inputs: 
  # covidfilename: file name in string format 
  # firstdate: first date of interest
  # seconddate: second date of interest
  
  # outputs:
  # coviddata2 in dataframe format 
  
  # read file
  coviddata0 = fread(covidfilename)
  
  # select variables date, area, population, total tests, positive tests, cases
  coviddata = dplyr::select(coviddata0, 'date', 'area', 'population', 'total_tests', 'positive_tests', 'cases')
  
  # remove out of state data
  coviddata = dplyr::filter(coviddata, as.logical(coviddata[,'area'] != 'Out of state'))
  coviddata = dplyr::filter(coviddata, as.logical(coviddata[,'area'] != 'California'))
  coviddata = dplyr::filter(coviddata, as.logical(coviddata[,'area'] != 'Unknown'))
  
  # calculate "true" case number as total positive tests of prior 10 days
  # truncates 10 days from beginning of total data
  coviddata2 = coviddata %>% group_by(area) %>% mutate(truecases = lag(positive_tests, n=1) +
    lag(positive_tests, n=2) + lag(positive_tests, n=3) + lag(positive_tests, n=4)
    + lag(positive_tests, n=5) + lag(positive_tests, n=6) + lag(positive_tests, n=7)
    + lag(positive_tests, n=8) + lag(positive_tests, n=9) + lag(positive_tests, n=10))
  coviddata2 = dplyr::ungroup(coviddata2)
  coviddata3 = dplyr::filter(coviddata2, !is.na(coviddata2$truecases))
  
  # select date range
  coviddata2 = trimdate(coviddata3, firstdate, seconddate)
  
  # add prevalence, positivity rate to dataset
  # use approximation instead that people recover in 10 days, or are hospitalized where they can't infect others
  # see calculation above 
  prev = (coviddata2[,"truecases"]/coviddata2[,"population"])
  names(prev) = 'prev'
  # prev = coviddata2[,"cases"]/coviddata2[,"population"]*prevfactor # cases = positive tests + deaths
  positivity = coviddata2[,"positive_tests"]/coviddata2[,"total_tests"]
  names(positivity)='positivity'
  Newdata = data.frame(coviddata2[,"date"], prev, positivity)
  coviddata3 = dplyr::mutate(coviddata2, Newdata)
  
  # sort by area
  coviddata4 = arrange(coviddata3, area)
  coviddata4 = data.frame(coviddata4)
  
  # output list of counties
  # remove sutter county, which has no temp data
  countylist = unique(coviddata4[,'area'])
  omitcounties = c('Sutter') # Sutter had no temp data
  #listofcounties = c('San Francisco', 'Los Angeles', 'Fresno')
  coviddata4 = dplyr::filter(coviddata4, as.logical(!(coviddata4$area %in% omitcounties)))
  
  return(list("covid" = coviddata4))
}
