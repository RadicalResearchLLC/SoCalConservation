#30x30 processing
#Created by Mike McCarthy
#First created July 2024
#Modified November 2024
#Created for SoCal Earth - Redford Conservancy

#30x30 definition
#Areas under government ownership or control, primarily designated to protect species or habitats
#Areas under perpetual easements that protect species and their habitats
#Areas with species and habitat protection designations that have gone through a formal rulemaking or other
#enforceable decision making process not subject to simple reversal

library(sf)
library(leaflet)
library(tidyverse)
library(tigris)

'%ni%' <- Negate('%in%') ## not in operator
options(tigris_use_cache= T)
#Directories
wd <- getwd()
data_dir <- str_c(wd, '/data')
#don't use spherical geometry for joins
sf_use_s2(FALSE)

#Counties in SoCal
SoCal_counties <- c('Los Angeles', 'Orange', 'Riverside', 'San Bernardino',
                    'San Diego', 'Imperial', 'Ventura')

### Import jurisdictions and create summary list
#County data from tigris 
CA_counties <- counties(state = 'CA', cb = T, year = 2023) |> 
  filter(NAME %in% c(SoCal_counties)) |> 
  select(NAME, ALAND) |> 
  mutate(NAME = str_c(NAME, ' County')) |> 
  st_make_valid() |> 
  st_transform(crs = 4326)

str(CA_counties)
st_area(CA_counties)

#Places boundaries from tigris 2023
#Cities and census designated places (and two towns?)
jurisdictions <- places(state = 'CA', cb = T, year = 2023) |> 
  st_transform(crs = 4326) |> 
  st_filter(CA_counties , .predicate = st_covered_by) |> 
  select(NAME, ALAND)
  
## create SoCal
SoCal_summary <- CA_counties |> 
  summarize(ALAND = sum(ALAND)) |> 
  mutate(NAME = 'Southern California') 

st_is_valid(SoCal_summary)
st_is_valid(jurisdictions)
st_is_valid(CA_counties)
# create full Jurisdiction list - all in NAD83
jurisFinal2 <- bind_rows(jurisdictions, CA_counties, SoCal_summary) 

st_is_valid(jurisFinal2)
rm(ls = jurisdictions, SoCal_summary)

#30x30 terrestrial data from here - https://www.californianature.ca.gov/pages/open-data
#2024 terrestrial dataset acquired November 2024
#Check for update biannually
conservationAreas <- sf::st_read(dsn = str_c(data_dir, '/30x30_Conserved_Areas_2024')) |> 
  filter(CA_County_ %in% SoCal_counties) |> # filter down to 
  st_make_valid() |> #make sure polygons are valid
  st_zm() |> # remove z dimensions 
  select(UNIT_NAME, Acres, geometry)

## Group polygons by conservation name
conserve1 <- conservationAreas |> 
  select(UNIT_NAME, Acres, geometry) |> 
  group_by(UNIT_NAME) |> 
  summarize() 

## Simplify polygons to 10 meter tolerance to conserve space
conserve_simple <- conserve1 |> 
  st_simplify(dTolerance = 10, preserveTopology = FALSE) |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

# Check how much smaller it is
round(c(object.size(conservationAreas), object.size(conserve_simple)) / 1024)

st_is_valid(conserve_simple)
rm(ls = conservationAreas, conserve1)
gc()

# import biodiversity dataset
# data from 30x30 open data
# https://www.californianature.ca.gov/pages/open-data downloaded November 2024
biodiversity <- sf::st_read(dsn = str_c(data_dir, '/Species_Biodiversity_-_ACE')) |> 
  filter(County %in% str_to_upper(SoCal_counties))

gc()
# simplify it by only keeping three columns and reduce polygon resolution
bio_simpler <- biodiversity |> 
  select(Name, SpBioRnkEc) |> 
  group_by(Name, SpBioRnkEc) |> 
  summarize(.groups = 'drop') |> 
  st_simplify(dTolerance = .0003, preserveTopology = FALSE) |> 
  st_make_valid() |> 
  st_transform(crs = 4326) 

# keep high biodiversity for filtering
high_bio <- bio_simpler |> 
  filter(SpBioRnkEc >= 4) |> 
  select(geometry) |> 
  summarize()

round(c(object.size(biodiversity), 
        #object.size(bio_simple),
        object.size(bio_simpler)) /1024) 

st_is_valid(bio_simpler)
st_is_valid(high_bio)
rm(ls = biodiversity)

setwd(str_c(wd, '/SoCalConservation'))

save.image('.RData')
setwd(wd)      
save.image('.RData')
