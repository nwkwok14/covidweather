cleantemp = function(weather, combinedgeo, AreaPop){
  # Function description
  # downselects variables to date, tmax, tmin, adds county
  # combines stations per county using mean temperature using station populations
  # calculate average weather for all counties
  
  # Inputs: 
  # weather - has date tmax tmin county (NA) station lat long area
  # combinedgeo - zcta zips station lat long area pop moe; population is derived from zcta
  
  # outputs:
  # temps - data frame with date, area, maximum and minimum daily temperatures 
  # at the county level
  
  # add county population  
  xweather = dplyr::arrange(weather, area)
  xAreaPop = dplyr::arrange(AreaPop, area)
  xcounts = dplyr::count(xweather, area)
  xpopvector = rep(xAreaPop$population, times = xcounts$n)
  yweather = dplyr::mutate(xweather, countypop = xpopvector)
  
  # downsample weather data to stations with zip code, zcta, pop
  lessweather = dplyr::filter(yweather, (yweather$STATION %in% combinedgeo$station))
  
  # downsample variables to date tmax tmin area station pop
  betterweather = dplyr::select(lessweather, date, area, STATION, TMAX, TMIN, countypop)
  bettergeo = dplyr::select(combinedgeo, station, pop)
  
  # sort by station
  sortedweather = dplyr::arrange(betterweather, STATION)
  sortedgeo = dplyr::arrange(bettergeo, station)
  
  # add population to the weather dataset
  counts = dplyr::count(sortedweather, STATION)
  popvector = rep(sortedgeo$pop, times = counts$n)
  newweather = dplyr::mutate(sortedweather, pop = popvector)
  
  # sort by area, date, station
  # fundamentally, the limitation is that for dates with different numbers of stations represented, 
  # the change in temperature may represent changes in the number of stations rather than 
  # the actual average temperature.
  # calculate average temperature across stations using station populations
  finalweather = dplyr::arrange(newweather, area, date, STATION)
  totalpop = finalweather %>% group_by(area, date) %>% mutate(totalpop = sum(pop))
  totalpop = dplyr::mutate(totalpop, wTMAX = TMAX*pop/countypop, wTMIN = TMIN*pop/countypop, 
                    factor = (countypop - totalpop)/countypop)
  totalpop = dplyr::mutate(totalpop, fTMAX = TMAX*factor, fTMIN = TMIN*factor)
  
  maxp = totalpop %>% summarise(pTMAX = sum(wTMAX))
  minp = totalpop %>% summarise(pTMIN = sum(wTMIN))
  meansmax = totalpop %>% summarise(Tmeanmax = mean(fTMAX))
  meansmin = totalpop %>% summarise(Tmeanmin = mean(fTMIN))
  
  maxp = dplyr::ungroup(dplyr::ungroup(maxp))
  meansmax = dplyr::ungroup(dplyr::ungroup(meansmax))
  maxtemps = dplyr::mutate(meansmax, TMAX = meansmax$Tmeanmax + maxp$pTMAX)
    
  minp = dplyr::ungroup(dplyr::ungroup(minp))
  meansmin = dplyr::ungroup(dplyr::ungroup(meansmin))
  mintemps = dplyr::mutate(meansmin, TMIN = meansmin$Tmeanmin + minp$pTMIN)
  
  by1 <- join_by(area, date)
  temps = full_join(maxtemps, mintemps, by1)
  temps = dplyr::select(temps, area, date, TMAX, TMIN)
  temps = dplyr::arrange(temps, area)
  
  return(temps)}