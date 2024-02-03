loadfxn = function(functionroot){
    # function description: loads all custom functions from folder
  
    ### inputs:
    # functionroot - file path to function folder
  
    ### outputs: none
  
    source(paste(functionroot, "cleancovidtendayprev.R", sep = '')) # incorporates 10 day average of new positive tests as prevalence
    source(paste(functionroot, "cleantemp.R", sep = ''))
    source(paste(functionroot, "trimdate.R", sep = ''))
    source(paste(functionroot, 'propNA.R', sep = ''))
    source(paste(functionroot, 'secondfilter.R', sep = ''))
    source(paste(functionroot, 'loadtemp.R', sep = ''))
    source(paste(functionroot, 'loadstation.R', sep = ''))
    source(paste(functionroot, 'combinegeo.R', sep = ''))
    source(paste(functionroot, 'mixedmodelnestednorm.R', sep = ''))
    source(paste(functionroot, 'combinecovidweather.R', sep = ''))
    source(paste(functionroot, 'PrevStratifiedPosByTMAX.R', sep = ''))
}