loadstation = function(weather){
  ### function description: for stations in weather dataset, retrieves zipc codes 
  # of note, all zip codes in california use 9----
  
  ### inputs
  # weather - raw weather dataset
  
  ### outputs
  # statzips - data frame with station, longitude, latitude, county, zipcode
  
  stations = dplyr::distinct(weather, STATION, LATITUDE, LONGITUDE, .keep_all = TRUE)
  addresses = reverse_geocode(stations, LATITUDE, LONGITUDE,
                              address = 'address', return_input = FALSE, full_results = FALSE, 
                              limit = NULL, unique_only = FALSE)
  
  # remove first 7 characters to remove street number
  addresses$shorter <--substr(addresses$address, 7, nchar(addresses$address))
  zips = str_extract_all(addresses$shorter, "9\\d{4}", simplify = TRUE)
  statzips = data.frame(stations$STATION, stations$LATITUDE, stations$LONGITUDE, stations$area, zips)
  return(statzips)
}