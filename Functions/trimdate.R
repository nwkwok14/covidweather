trimdate = function(data, firstdate, seconddate){
  ### Function description: truncates data to date range desired

  ### input variables: 
  # data (data frame) - data frame to trim
  # firstdate - first date of interest 
  # seconddate - last date of interest
  
  ### outputs: 
  # trimmed (data frame) - data frame trimmed to date range between firstdate 
  # and seconddate
  
  firstdatecolumn = rep(firstdate, length(data$date))
  lastdatecolumn = rep(seconddate, length(data$date))
  logicals = (data$date>=firstdatecolumn & data$date<=lastdatecolumn)
  trimmed = dplyr::filter(data,logicals)
  return(trimmed)
}