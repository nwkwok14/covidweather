combinecovidweather = function(finalcovid, finalweather, date1, date2){
  ### inputs
  # finalcovid - cleaned covid dataset
  # finalweather - cleaned weather dataset
  # date1 - first date of date range
  # date2 - last date of date range
  
  ### outputs
  # mainframe - combined weather and covid dataset, aligned by date and county
  
  # handle if counties are asymmetric
  if (length(unique(finalcovid$area)) > length(unique(finalweather$area))){
    finalcovid = dplyr::filter(finalcovid, (finalcovid$area %in% finalweather$area))
  } else if (length(unique(finalweather$area)) > length(unique(finalcovid$area))){
    finalweather = dplyr::filter(finalweather, (finalweather$area %in% finalcovid$area))
  }
  
  # downselect covid dataset to relevant values
  # set date class to Date
  Dates = as.Date(finalcovid$date)
  finalcovid = dplyr::select(finalcovid, -date)
  finalcovid = dplyr::mutate(finalcovid, date = Dates)
  
  # combine data into one data frame
  # find counties with a lot of missing data
  counts = count(finalweather, area)
  lacksweather = counts[1][counts[2]<count(finalcovid, area)[2]]
  lacksweathercount = counts[2][counts[2]<count(finalcovid, area)[2]]
  lackscovid = counts[1][counts[2]>count(finalcovid, area)[2]]
  lackscovidcount = counts[2][counts[2]>count(finalcovid, area)[2]]
  # deletes counties, then appends with filled out county with NA values for missing dates
  if (length(lacksweather)>0) {
    for (i in lacksweather){
      imbalancedcounty = dplyr::filter(finalweather, finalweather$area==i)
      finalweather = dplyr::filter(finalweather, finalweather$area!=i)
      date = as.Date(seq(date1, date2, by = 'days'))
      days = data.frame(date, area = rep(i, length(date)), TMAX = rep(NA, length(date)), TMIN = rep(NA, length(date))) 
      extradays = dplyr::filter(days, as.logical(!(days$date %in% imbalancedcounty$date)))
      balancedcounty = dplyr::arrange(dplyr::bind_rows(imbalancedcounty, extradays), date)
      finalweather= dplyr::bind_rows(finalweather, balancedcounty)
    }}
  if (length(lackscovid)>0) {
    for (i in lackscovid){
      imbalancedcounty = dplyr::filter(finalcovid, finalcovid$area==i)
      finalcovid = dplyr::filter(finalcovid, finalcovid$area!=i)
      date = as.Date(seq(date1, date2, by = 'days'))
      days = data.frame(date, area = rep(i, length(date)), prev = rep(NA, length(date)), pos = rep(NA, length(date))) 
      extradays = dplyr::filter(days, as.logical(!(days$date %in% imbalancedcounty$date)))
      balancedcounty = dplyr::arrange(dplyr::bind_rows(imbalancedcounty, extradays), date)
      finalcovid= dplyr::bind_rows(finalcovid, balancedcounty)
    }}
  
  by2 = join_by(date, area)
  mainframe = dplyr::full_join(finalcovid, finalweather, by2)
  return(mainframe)
} 