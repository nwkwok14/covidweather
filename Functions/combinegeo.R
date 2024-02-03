combinegeo =  function(CAzzonly, zctapopulations, zipcodes){

  ### function description
  # creates mapping of station to associated population by zip code
  
  ### inputs
  # CAzzonly - mapping of ZCTA to zip codes for CA
  # zctapopulations - mapping of ZCTA to population
  # zipcodes - mapping of stations to zip codes 
  
  ### outputs
  # zipzctapop - mapping of stations to zip code associated populations
  
  # remove blank zipcodes
  notblank = !(zipcodes$X1 == '')
  zipcodes = data.frame(station = zipcodes$stations.STATION[notblank],
                        lat = zipcodes$stations.LATITUDE[notblank],
                        long = zipcodes$stations.LONGITUDE[notblank],
                        area = zipcodes$stations.area[notblank],
                        zips = as.integer(zipcodes$X1[notblank]),
                        X2 = as.integer(zipcodes$X2[notblank]))
  
  # resolve doubled up 5 digit codes and address number
  loclogs = (zipcodes["X2"] != "")
  newvalues = (zipcodes$X2[loclogs])
  oldvalues = zipcodes$zips[loclogs]
  for (i in 1:length(newvalues)){
    zipcodes["zips"][zipcodes["zips"]==oldvalues[i]]<-newvalues[i]}
  
  zipcodes = dplyr::select(zipcodes, zips, station, lat, long, area)
  
  # combine zipcodes and CAzzonly
  # sorting zipcodes, CAzzonly presorted by zip code (and similarly zcta)
  sortedzips = arrange(zipcodes, zips)
  
  selectzctas = data.frame(zips = CAzzonly$ZIP_CODE[CAzzonly$ZIP_CODE %in% sortedzips$zips], 
                           zcta = CAzzonly$ZCTA[CAzzonly$ZIP_CODE %in% sortedzips$zips]) 
  zzca = dplyr::left_join(sortedzips, selectzctas)
  # remove any NA rows for zcta
  zzca = dplyr::filter(zzca, !is.na(zcta))
  
  # find populations corresponding to zctas
  # resorting by zcta
  sortedzzca = arrange(zzca, zcta)
  sortedzzca = dplyr::select(sortedzzca, zcta, zips, station, lat, long, area)
  # zctapopulations is also presorted by zcta
  relevantpop = data.frame(zcta = as.integer(zctapopulations$GEOID[zctapopulations$GEOID %in% zzca$zcta]),
                           pop = as.numeric(zctapopulations$estimate[zctapopulations$GEOID %in% zzca$zcta]),
                           moe = as.numeric(zctapopulations$moe[zctapopulations$GEOID %in% zzca$zcta]))
  zipzctapop = dplyr::left_join(sortedzzca, relevantpop)
  zipzctapop = dplyr::filter(zipzctapop, !is.na(pop) & pop!=0)
  return(zipzctapop)
}
