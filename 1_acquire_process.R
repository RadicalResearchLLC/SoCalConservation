#30x30 processing
#Created by Mike McCarthy
#First created July 2024
#Modified January 2025
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
rm(ls = jurisdictions)

gc()
object.size(jurisFinal2)
#30x30 terrestrial data from here - https://www.californianature.ca.gov/pages/open-data
#2024 terrestrial dataset acquired November 2024
#Check for update biannually
conservationAreas <- sf::st_read(dsn = str_c(data_dir, '/30x30_Conserved_Areas_2024')) |> 
  filter(CA_County_ %in% SoCal_counties) |> # filter down to 
  st_make_valid() |> #make sure polygons are valid
  st_zm() |> # remove z dimensions 
  select(UNIT_NAME, Acres, geometry)

object.size(conservationAreas)

## Group polygons by conservation name
conserve1 <- conservationAreas |> 
  select(UNIT_NAME, Acres, geometry) |> 
  group_by(UNIT_NAME) |> 
  summarize(count = n(), Acres = round(sum(Acres), 1)) #|> 
 
mega_conserve <- conserve1 |> 
  select(geometry) |> 
  summarize() |> 
  st_simplify(dTolerance = 30) |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

object.size(mega_conserve)

## Simplify polygons to 100 meter tolerance to conserve space
conserve_LT3acre <- conserve1 |> 
  filter(Acres < 3) |> 
  st_simplify(dTolerance = 3, preserveTopology = FALSE) |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

conserve_3_to_100acres <- conserve1 |> 
  filter(Acres >= 3 & Acres < 100) |> 
  st_simplify(dTolerance = 10, preserveTopology = FALSE) |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

conserve_100_2000 <-  conserve1 |> 
  filter(Acres >= 100 & Acres < 2000) |> 
  st_simplify(dTolerance = 20, preserveTopology = FALSE) |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

conserve_GT2000 <- conserve1 |> 
  filter(Acres >= 2000) |> 
  st_simplify(dTolerance = 30, preserveTopology = FALSE) |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

conserve_simple <- bind_rows(conserve_3_to_100acres, conserve_LT3acre, conserve_100_2000, conserve_GT2000)

# Check how much smaller it is
round(c(object.size(conservationAreas), object.size(conserve_simple)) / 1024)

st_is_valid(conserve_simple)
rm(ls = conservationAreas, conserve_3_to_100acres, conserve_LT3acre, conserve_GT2000, conserve_100_2000)
gc()

str(conserve_simple)

# import biodiversity dataset
# data from 30x30 open data
# https://www.californianature.ca.gov/pages/open-data downloaded November 2024
biodiversity <- sf::st_read(dsn = str_c(data_dir, '/Species_Biodiversity_-_ACE')) |> 
  filter(County %in% str_to_upper(SoCal_counties)) |> 
  st_transform(crs = 3310)

gc()
# simplify it by only keeping three columns and reduce polygon resolution
bio_simpler <- biodiversity |> 
  select(SpBioRnkEc) |> 
  group_by(SpBioRnkEc) |> 
  summarize(.groups = 'drop') |>
  st_simplify(dTolerance = 30, preserveTopology = FALSE) |> 
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

### Perform calculations to speed up processing
st_is_valid(mega_conserve)

conserveArea1 <- jurisFinal2 |> 
  st_intersection(mega_conserve)

conserveArea2 <- as.numeric(st_area(conserveArea1))
conserveArea1$areaconserve <- conserveArea2

conserveArea <- conserveArea1 |> 
  st_set_geometry(value = NULL) |> 
  select(NAME, areaconserve)

jurisArea <- as.numeric(st_area(jurisFinal2))
jurisFinal2$area <- jurisArea

## USE ALAND - probably including area that is not land
JurisTbl1 <- jurisFinal2 |> 
  st_set_geometry(value = NULL) |> 
  mutate(diff = 100*(ALAND-area)/area) |> 
  left_join(conserveArea) |> 
  mutate(conservePct = 
           ifelse(is.na(areaconserve), 0,
                    round(100*areaconserve/ALAND, 1))
  ) |> 
  select(NAME, conservePct)

## high biodiversity land by jurisdiction

highBio <- jurisFinal2 |> 
  st_intersection(high_bio) |> 
  select(NAME)

bio_area_juris <- as.numeric(st_area(highBio))

highBio$highBioArea <- bio_area_juris

highBio2 <- highBio |> 
  st_set_geometry(value= NULL)

conserved_bio <- st_intersection(mega_conserve, high_bio)

juris_highConsvd_bio <- st_intersection(jurisFinal2, conserved_bio) 
highConsv_bio_area <- as.numeric(st_area(juris_highConsvd_bio))
juris_highConsvd_bio$bioConsvdArea <- highConsv_bio_area

jurisBio2 <- juris_highConsvd_bio |> 
  st_set_geometry(value = NULL)

jurisTbl2 <- jurisFinal2 |> 
  st_set_geometry(value = NULL) |> 
  left_join(highBio2) |> 
  left_join(jurisBio2) |> 
  mutate(highBioPct = 
           ifelse(is.na(highBioArea), 0,
             ifelse(is.na(bioConsvdArea), 0,
               round(100*bioConsvdArea/highBioArea, 1)))
  ) |> 
  select(NAME, highBioPct)
  
###

bufferJuris1Mile <- jurisFinal2 |> 
  st_transform(crs = 3310) |> 
  st_buffer(dist = 1609.3) |> 
  st_transform(crs = 4326) |> 
  st_intersection(SoCal_summary) |> 
  select(NAME)

bufferJurisArea <- as.numeric(st_area(bufferJuris1Mile))
bufferJuris1Mile$bufferJurisArea <- bufferJurisArea
bufferJuris1Mile2 <- bufferJuris1Mile |> 
  select(NAME, bufferJurisArea) |> 
  st_set_geometry(value = NULL)

consvd_1Mile <- st_intersection(bufferJuris1Mile, mega_conserve)
consvdJurisArea <- as.numeric(st_area(consvd_1Mile))
consvd_1Mile$consvdArea <- consvdJurisArea
consvd_1Mile2<- consvd_1Mile |> 
  select(NAME, consvdArea) |> 
  st_set_geometry(value = NULL)

jurisTbl3 <- jurisFinal2 |> 
  st_set_geometry(value = NULL) |> 
  left_join(bufferJuris1Mile2) |> 
  left_join(consvd_1Mile2) |> 
  mutate(AccessPct = 
           ifelse(is.na(bufferJurisArea), 0,
                  ifelse(is.na(consvdArea), 0,
                         round(100*consvdArea/bufferJurisArea, 1)))
  ) |> 
  select(NAME, AccessPct)

jurisTbl <- JurisTbl1 |> 
  full_join(jurisTbl2) |> 
  full_join(jurisTbl3)

rm(ls = JurisTbl, JurisTbl1, jurisTbl2, jurisTbl3)
rm(bufferJuris, bufferJuris1Mile2,
   conserveArea, conserveArea1,
   consvd_1Mile, consvd_1Mile2,
   highBio, highBio2, juris_highConsvd_bio, jurisBio2)
rm(ls = conserve1)

object.size(conserve1)
object.size(conserve_simple)
gc()

setwd(str_c(wd, '/SoCalConservation'))

save.image('.RData')
setwd(wd)      
save.image('.RData')
