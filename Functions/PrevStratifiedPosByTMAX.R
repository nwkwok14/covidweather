PrevStratifiedPosByTMAX = function(countydata){
  
  ### function description: for a given county, plots temperature vs positivity 
  # rate, stratified by prevalence (3 quantiles)
  
  ### inputs:
  # countydata - combined weather/covid data for single county 
  
  ### outputs: 
  # plot2 - plot of temperatures vs positivity rate, stratified by three 
  # quantiles of prevalence
  
  prev2 = countydata$prev
  quant = split_quantile(prev2, 3)
  quantileddata = dplyr::mutate(countydata, quant)
  quant1 = dplyr::filter(quantileddata, quant==1)
  quant2 = dplyr::filter(quantileddata, quant==2)
  quant3 = dplyr::filter(quantileddata, quant==3)
  
  plotcomp = list(
    xlim(35, 115),
    ylim(0, 0.25),
    scale_x_continuous(limits = c(35, 115), breaks=seq(35,115,10))
    )
  
  plotcomp2 = list(
    ggtitle(paste(countydata$area[1], 'County', sep = ' ')), 
    theme(plot.title = element_text(hjust = 0.5)))
  
  plot1 <- ggplot(quantileddata, aes(x=TMAX, y=pos, color = quant)) + 
    geom_point() + plotcomp
  plot2 <- plot1 + theme_classic() + plotcomp2 
  
  return(plot2)  
}