# Beta Regression Mixed Effects Model
# Independent Variables: maximum daily ambient temperature (averaged for each county),
# COVID prevalence (scaled), county, day of week, log(population)
# Dependent Variable: COVID-19 test positivity rate

# Written by Nicholas Kwok, MD 
# 01/29/2024
# Cedars-Sinai Medical Center

# run code using rscript (tempCOVIDmixedmodel.R)

#############################################

# define file paths
#fileroot1 = 'C:\\Users\\music\\Desktop\\COVIDweatherproject\\'
fileroot2 = 'C:\\Users\\nwkwok\\Desktop\\COVIDweatherproject\\'

functionroot = paste(fileroot2, 'Functions\\', sep = '')
dataroot = paste(fileroot2, 'Data\\', sep = '')

#############################################

# load fxns and packages
source(paste(functionroot, "loadpckg.R", sep = ''))
source(paste(functionroot, "loadfxn.R", sep = ''))

# loads packages
if (TRUE) {
  loadpckg()
}

# loads functions 
if (TRUE) {
  loadfxn(functionroot)  
}

# set maximum dates to load data (limited by COVID dataset)
maxdate1 = as.Date('2020-07-01')
maxdate2 = as.Date('2021-01-30')

#############################################

covidfilename = paste(dataroot, "covid19cases_test", ".csv", sep = '')

# load and clean COVID data
if (FALSE){
  covidlist = cleancovidtendayprev(covidfilename, maxdate1, maxdate2)
  save(covidlist, file = 'covidlistmod.rds')}
load(paste(dataroot, 'covidlistmod.rds', sep = ''))
coviddata = covidlist[[1]]
    
# define data frame with areas and populations
AreaPop = dplyr::distinct(dplyr::select(coviddata, area, population))
sortedAreaPop = dplyr::arrange(AreaPop, population)
listofcounties = AreaPop$area

##########################

# load temperature files, truncated to correct time frame
if (FALSE) {
  weather = loadtemp(listofcounties, maxdate1, maxdate2)
  save(weather, file = 'weather.rds')}
load(paste(dataroot, 'weather.rds', sep = ''))

############################################

# load station data for weather
if (FALSE) {
  zipcodes = loadstation(weather)
  save(zipcodes, file = 'zipcodes.rds')}
load(paste(dataroot, 'zipcodes.rds', sep = ''))

# set up zip code to zcta table
if (FALSE) {
  zippathname = paste(dataroot, 'ZIPCodetoZCTACrosswalk2021UDS', '.csv', sep = '') 
  zipzcta = fread(zippathname)
  CAzipzcta = dplyr::filter(zipzcta, zipzcta$STATE=='CA')
  CAzzonly = dplyr::select(CAzipzcta, "ZIP_CODE", "ZCTA")
  save(CAzzonly, file = 'CAzipcodeszcta.rds')}
load(paste(dataroot, "CAzipcodeszcta.rds", sep = ''))

# retrieve populations per zipcode for ACS 2019 5 year survey (2014-2019)
if (FALSE){
  possiblevariables = load_variables(2021,dataset = c("acs5"), cache = FALSE)
  census_api_key("c38b33c92f3f00a174defd61764a780b9e02ae39")
  zctapop = get_acs(geography = 'zcta', variables = "B01003_001", year = 2019, state = "CA", survey = 'acs5' )
  zctapopulations = dplyr::select(zctapop, "GEOID", "estimate", "moe")
  save(zctapopulations, file = 'zctapopulations.rds')
}
load(paste(dataroot, 'zctapopulations.rds', sep = ''))

if (FALSE){
  combinedgeo = combinegeo(CAzzonly, zctapopulations, zipcodes)
  save(combinedgeo, file = 'combinedgeo.rds')}
load(paste(dataroot, 'combinedgeo.rds', sep = ''))

#############################################################

# count weather stations
countweather = dplyr::filter(weather, (weather$STATION %in% combinedgeo$station))
distinctstationareas = dplyr::distinct(dplyr::select(weather, area, STATION))

#  clean temp data
# removes Del Norte (no stations with associated zipcode)
if (FALSE){
  hashweather = cleantemp(weather, combinedgeo, AreaPop)
  save(hashweather, file = 'cleanedweather.rds')
}
load(paste(dataroot, 'cleanedweather.rds', sep = ''))

############################################ 

# define dates of interest
date1 = as.Date('2020-08-01')
date2 = as.Date('2020-12-11')
ndays = length(seq(date1, date2, '1 day'))

newcovid = trimdate(coviddata,date1,date2)
newweather = trimdate(hashweather, date1, date2)

# combine datacovid and weather data
mainframe = combinecovidweather(newcovid, newweather, date1, date2)

# add total tests/population, DOW, county encoding, pos, negative tests
# to handle y>0 and y < 1, uses (y*(n-1)+0.5)/n
# to handle zero total_tests or negative_tests or covid cases, use bias +1
totaldays = length(mainframe$total_tests)
testavail = mainframe$total_tests/mainframe$population
DOW = as.factor(wday(mainframe$date, week_start=3)) # week starts on wednesday; 8/1/2020 was a saturday
pos = (mainframe$positivity*(totaldays-1)+0.5)/totaldays
logpop = log(mainframe$population)
mainframe = dplyr::mutate(mainframe, testavail, DOW, logpop, pos)
mainframe = dplyr::arrange(mainframe, population, date)
# county converted to factor
# county in order by population for encoding
sortedcty = dplyr::distinct(mainframe, area)
cty = factor(mainframe$area, levels = sortedcty$area)
mainframe = dplyr:: mutate(mainframe, cty) 

##############################################################

# removes counties with population below cutoff, prevalence below cutoff (in %),
# and missing data more than cutoff fraction
cutoffpop = 25000 # 25,000 people in population; no cutoff is 0; # cuts Alpine, Sierra, Modoc
cutoffprev = 0 # 0.001% in median prevalence; no cutoff is 0
cutoffmisspos = 1 # 1% missing data; no cutoff is 1
cutoffmisstemp = 1 # 1 % missing data; no cutoff is 1
# missing positivity data is actually due to total tests = 0 unless error msg prints
datalist = secondfilter(mainframe, cutoffpop, cutoffprev, cutoffmisspos, cutoffmisstemp)
mainframe = datalist[[1]]
removecounties = datalist[[2]]

# remove days with tests less than 1st quartile of values
mainframe = mainframe %>% filter(total_tests>=123) # using 1st quartile

############################################################

# drop NA rows instead of filling
mainframe = mainframe %>% drop_na(TMAX)

# if fewer than 30% days remaining, remove counties
checkcount = count(mainframe, area)
removecty = checkcount %>% filter(checkcount$n < ndays*0.5)
themainframe = mainframe %>% filter(!(mainframe$area %in% removecty$area))

###############################################################

# for convergence, scale prevalence
prevscaled = themainframe$prev *1000
themainframe2 = dplyr::mutate(themainframe, prevscaled)

###############################################################

# plot temperature vs positivity graphs for three representative counties

AreaPopUpdated = arrange(dplyr::distinct(dplyr::select(themainframe2, area, population)), population)
Firstcounty = AreaPopUpdated$area[1]
Lastcounty = AreaPopUpdated$area[length(AreaPopUpdated$area)]
# # if even number of counties, may default to NA or empty
Middlecounty = AreaPopUpdated$area[AreaPopUpdated$population == median(AreaPopUpdated$population)]

# # example graphs
# LA = dplyr::filter(themainframe2, area == "Los Angeles")
# Lassen = dplyr::filter(themainframe2, area=='Lassen')
# Placer = dplyr::filter(themainframe2, area=='Placer')

# p1 = PrevStratifiedPosByTMAX(LA)
# p1 = p1 + scale_color_discrete(guide = 'none') + theme (axis.title = element_blank())
# p2 = PrevStratifiedPosByTMAX(Placer)
# p2 = p2 + scale_color_discrete(guide = 'none') +  theme (axis.title.y = element_blank()) 
# p2 = p2 + xlab('Maximum Ambient Temperature (\u00B0F)')
# p3 = PrevStratifiedPosByTMAX(Lassen)
# p3 = p3 + scale_color_discrete(guide = 'none') + theme (axis.title.x = element_blank())
# p3 = p3 + ylab('Positivity Rate')
# p4 = p3 + p2 + p1
# p4 = p4 + scale_color_discrete(name='Prevalence', 
#                      labels = c('Quantile 1', 'Quantile 2', 'Quantile 3'))

####################################################

# model variable summary stats
newareacounts = count(themainframe2, area)
# summary(newareacounts)
sdn = sd(newareacounts$n)
# summary(themainframe2)
sdTMAX = sd(themainframe2$TMAX)
sdpop = sd(themainframe2$population)
sdtt = sd(themainframe2$total_tests)
sdps = sd(themainframe2$prevscaled)
sdpos = sd(themainframe2$pos)

# calculate counts for stations (from above)
logincludedareas = (distinctstationareas$area %in% distinct(themainframe2, area)$area)
stationareasincluded = dplyr::filter(distinctstationareas, logincludedareas)
# summary(count(stationareasincluded, area))
stdevstations = sd(count(stationareasincluded, area)$n)

###################################################

# apply GLMMTMB model: beta regression model with link function logit, phi constant
# variables: pos, prev, TMAX, cty, DOW 
# GLMM TMB uses MLE for estimation
modellist = mixedmodelnestednorm(themainframe2)
modelleddata = modellist[[1]]
themodel = modellist[[2]]
summary(themodel)
check_collinearity(themodel)
# wald CI, expected to be asymmetric
modelci = confint(themodel) 
modelexpci = exp(confint(themodel))

### evaluate the model
res = residuals(themodel, type='pearson')
modelleddata = modelleddata %>% mutate(res)
# ggplot(data.frame(eta=predict(themodel,type="link"),res),
#        aes(x=eta,y=res)) + geom_point() + theme_bw()
# simOutput = simulateResiduals(fittedModel = themodel, plot=T)
# plotResiduals(simOutput, form = modelleddata$TMAX)
# plotResiduals(simOutput, form = modelleddata$prevscaled)
# plotResiduals(simOutput, form = modelleddata$logpop)
# testUniformity(simOutput)
# testOutliers(simOutput)
# testDispersion(simOutput)
# diagnose(themodel)

##############################

# try random data as input temperatures, and rerun model
set.seed(3)
TMAXr = rnorm(n=length(themainframe2$TMAX), mean = mean(themainframe2$TMAX),
              sd = sd(themainframe2$TMAX))
randomtemp = dplyr::transmute(themainframe2, date, area, pos, DOW, 
                              cty, prevscaled, logpop, 
                              TMAX = TMAXr)
modellistrand = mixedmodelnestednorm(randomtemp)
modelleddatarand = modellistrand[[1]]
summary(modellistrand[[2]])
check_collinearity(modellistrand[[2]])
modelcirand = confint(modellistrand[[2]])
modelexpcirand = exp(confint(modellistrand[[2]]))