loadtemp = function(listofcounties, maxdate1, maxdate2){
  ### function description - loads county specific CSV files with weather data, 
  # and creates single data frame
  
  ### inputs
  # listofcounties - list of counties from COVID dataset
  # maxdate1 - first date of desired weather dataset
  # maxdate2 - last date of desired weather dataset
  
  ### outputs
  # weather - data frame with date, county, station with longitude and latitude, 
  # maximum and minimum daily temperatures
  
  weather <- data.frame(date = as.Date("1970-01-01"), TMAX = 0, TMIN = 0, county = 'notcounty')
  
  for (countyi in listofcounties) {
    # countyi = listofcounties[1]  
    pathname = paste(dataroot, countyi, '.csv', sep = '') 
    weathersubset = fread(pathname)
    
    # trim weather data
    weathersubset = dplyr::mutate(weathersubset, date = as.Date(weathersubset$DATE))
    weathersubset = dplyr::select(weathersubset,"STATION","LATITUDE","LONGITUDE", "date", "TMAX","TMIN")
    
    # trim to date range
    weathersubset = trimdate(weathersubset, maxdate1, maxdate2)
    
    # remove dates with no weather data
    notemplogical = is.na(weathersubset$TMAX) & is.na(weathersubset$TMIN)
    weathersubset = dplyr::filter(weathersubset, as.logical(!notemplogical))
    
    # remove dates with no latitude or longitude
    nogeo = is.na(weathersubset$LATITUDE) | is.na(weathersubset$LONGITUDE)
    weathersubset = dplyr::filter(weathersubset, as.logical(!nogeo))
    
    # add county name
    area = countyi[rep(each=lengths(weathersubset[,"date"]), 1)]
    weathersubset = dplyr::mutate(weathersubset, area)
    
    if (dim(weathersubset)[1]==0) {
      next
    } else {
      weather = dplyr::bind_rows(weather, weathersubset)}
  }
  # remove placeholder row
  weather = dplyr::slice(weather, 2:length(weather$date))
  
  # sort counties by area
  weather = arrange(weather, area)
  
  return(weather)
}
