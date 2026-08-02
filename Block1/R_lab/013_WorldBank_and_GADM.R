#### World Bank's World Development Indicators (WDI) ####
#
## World Bank Data website
## http://data.worldbank.org/data-catalog/world-development-indicators 
## http://data.worldbank.org/ 
#
## Country codes:
## http://wits.worldbank.org/WITS/WITS/WITSHELP/Content/Codes/Country_Codes.htm
#
## Indicators' (variable) codes:
## http://data.worldbank.org/indicator/all
## ... Search the web page for the indicator, then use "Metadata" tab
## or
## Use the WDIsearch() function of the {WDI} package
#  
## Error codes:
## http://data.worldbank.org/node/211
#
#
#
#
#
rm(list=ls())
#
library(tidyverse)
library(WDI) # install.packages("WDI")
library(ggplot2) # install.packages("ggplot2")
library(RColorBrewer)
library(countrycode) # install.packages("countrycode")
#
#
#
#
### Search World Bank Data for GDP per capita in the USA, Canada & Mexico, constant prices
#
#
# Step 1: find suitable variable
#
?WDIsearch
gdp.datasets <- WDIsearch('gdp')
dim(gdp.datasets)
View(gdp.datasets)
#
# WDIsearch uses grep syntax {type ?grep into R concole} and is NOT case-sensitive
#
?regex # Regular Expressions as used in R
# ".*"
# We shall use the ".*" metacharacter combination:
# The "." matches/substitutes any character, "*" allows the preceding character 
# (dot) to be repeated 0 or more times
# So, if we want to search for something like: 
# "GDP per capita in constant prices, we may use e.g. the following:
#
WDIsearch('gdp.*capita.*constant')
# 
# Say, we decide to use:
# "NY.GDP.PCAP.PP.KD" "GDP per capita, PPP (constant 2005 international $)"
#
#
# Step 2: check the country code, if necessary:
#
?countrycode::codelist
str(codelist,list.len=15)
?countrycode
countrycode("USA",'iso3c', 'genc2c')
countrycode("Canada", "country.name", 'genc2c')
countrycode("Mexico", "country.name", 'genc2c')
countrycode("Guatemala", "country.name", 'genc2c')
#
#
# Step 3: retrieve the data from WB's WDI
#
?WDI 
# We create a data.frame "dat" for subsequent analysis.
# .. year chosen ad-hoc
dat <- WDI(indicator='NY.GDP.PCAP.KD', 
           country=c('MX','CA','US','GT'), 
           start=1960, end=2025)
head(dat)
tail(dat)
#
# Plot the data using ggplot2:
ggplot(dat, aes(x = year, y = NY.GDP.PCAP.KD, color=country)) + 
  geom_line(lwd=1) + 
  xlab('Year') + 
  ylab('GDP per capita, PPP (constant 2015 constant US $)')
#
#
world <- spData::world |> 
  dplyr::filter(iso_a2 %in% c('MX','CA','US','GT'))

dat2020 <- dat |> 
  filter(year == 2020)

world <- world |> 
  left_join(dat2020, by = c("iso_a2" = "iso2c"))
# country-level infomap
world |> 
  ggplot()+
  geom_sf(aes(fill=NY.GDP.PCAP.KD))+
  ggtitle("2020 GDP per capita (constant prices, USD, 2015)")+ 
  scale_fill_gradientn('GDP \nper capita', colours=brewer.pal(8, "Greens"))+
  theme_minimal()

#
#####################################################################
# Example 2 
# WDI database, Fertility rates & the use of metadata
rm(list=ls())
#
#
# Use the WDIsearch function to get a list of fertility rate indicators
indicatorMetaData <- WDIsearch("Fertility rate", field="name", short=FALSE)
# This serves for data source documentation & reproducibility
View(indicatorMetaData) 
#
#
# Define a list of countries for which to download the data
countries <- c("United States", "Britain", "Sweden", "Japan")
#
# Convert the country names to iso2c format used in the World Bank data
iso2cNames <- countrycode(countries, "country.name", "iso2c")
#
# Download data for each country, for selected fertility rate indicators, 
wdiData <- WDI(country=iso2cNames, "SP.DYN.TFRT.IN", 
               start=1990, end=2023)
View(wdiData)
#
#
# Create trend charts for the selected indicator
plot1 <- ggplot(wdiData, aes(x=year, y=SP.DYN.TFRT.IN, color=country)) +
              geom_line(lwd=1) +
              ylab('Fertility') +
              ggtitle("Fertility rate")
plot1 # shows the graph
#
#
#
#
#
################################################################################
#
#
## Geographic administrative units available from:  gadm.org
##
library(geodata)
rm(list=ls())
# 
# Malta 
#
Codes <- geodata::country_codes()
Codes %>% filter(NAME == "Malta")
malta <- geodata::gadm("MLT",level=2,path=getwd()) 
# download and save map into working directory
# geodata::gadm("MLT",level=2,path=getwd()) 
# malta <- readRDS("gadm/gadm41_MLT_2_pk.rds")
plot(malta[,1])
class(malta)
malta_sf <- sf::st_as_sf(malta)
plot(malta_sf[,1])
