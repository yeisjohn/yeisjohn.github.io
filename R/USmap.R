# Map of my research across US
rm(list=ls())
require(sf); require(tidyverse); require(tmap); require(sp);require(RColorBrewer)

usshp <- st_read("R/data/acs_2012_2016_county_us_B27001.shp",
               stringsAsFactors = FALSE) %>%
  rename(uninsured_2012 = un_2012,
         uninsured_2016 = un_2016,
         uninsured_diff = unnsrd_) %>%
  mutate(STFIPS = stringr::str_sub(GEOID, 1, 2))
states <- gsub(' ', '', sapply(strsplit(usshp$NAME,','), function(x) x[2]))
usunion <- st_union(usshp)

usshp$Project <- NA
usshp$inference <- NA


# Working lands for wildlife ----------------------------------------------
wlfw <- st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/WFLW/WLFW.gdb', layer='SiteInfo')
wlfw <- st_transform(wlfw, st_crs(usshp))
wlfwintersect <- st_intersects(usshp,wlfw, dist=0)

usshp$Project[which(sapply(wlfwintersect, length) > 0)] <- 'WLFW'


# Texas energy development ------------------------------------------------
TXProject_counts <- st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/TX', layer='OPJV_TXOil_SiteCoords')
TXProject_counts <- st_transform(TXProject_counts, st_crs(usshp))
TXintersect <- st_intersects(usshp,TXProject_counts, dist=0)

usshp$Project[which(sapply(TXintersect, length) > 0)] <- 'TX Energy Development'


# CREP --------------------------------------------------------------------
CREParea <- st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/CREP', layer='StudyArea_Shape')
CREPcounts <- read.csv('R/data/CREP/Target_Species_Observations_testdist_2016.csv')
coordinates(CREPcounts) <- ~ Longitude + Latitude
proj4string(CREPcounts) <- st_crs(CREParea)$proj4string
CREPcounts <- st_as_sf(CREPcounts, 'POINTS')
plot(CREPcounts$geometry)
CREPcounts <-  st_transform(CREPcounts, st_crs(usshp))

CREPintersect <- st_intersects(usshp,CREPcounts, dist=0)

usshp$Project[which(sapply(CREPintersect, length) > 0)] <- 'CREP'


# FSACIP ------------------------------------------------------------------
FSACIPstates <-  st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/FSACIP', layer='FSACIPStates')
FSACIPcounts <- read.csv('R/data/FSACIP/SpringVisits_20190520.csv')
coordinates(FSACIPcounts) <- ~Longitude + Latitude
proj4string(FSACIPcounts) <- st_crs(CREParea)$proj4string # probably same projection, doesn't really matter
FSACIPcounts <- st_as_sf(FSACIPcounts, 'POINTS')
FSACIPcounts <-  st_transform(FSACIPcounts, st_crs(usshp))
plot(FSACIPcounts$geometry)
FSACIPintersect <- st_intersects(usshp,FSACIPcounts, dist=0)

usshp$Project[which(sapply(FSACIPintersect, length) > 0)] <- 'FSA-CIP'

# all CIP ------------------------------------------------------------------
CIPcounts <- read.csv('R/data/CIP/AllSpringVisits_20200512.csv')
coordinates(CIPcounts) <- ~long + lat
proj4string(CIPcounts) <- st_crs(CREParea)$proj4string # probably same projection, doesn't really matter
CIPcounts <- st_as_sf(CIPcounts, 'POINTS')
CIPcounts <-  st_transform(CIPcounts, st_crs(usshp))
plot(CIPcounts$geometry)
CIPintersect <- st_intersects(usshp,CIPcounts, dist=0)

usshp$Project[which(sapply(CIPintersect, length) > 0)] <- 'CIP'

# DiLane  ------------------------------------------------------------------
DiLaneCounts <-  st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/DiLane', layer='DiLaneCC_fall2016')
DiLaneCounts <- st_transform(DiLaneCounts, st_crs(usshp))
DiLaneIntersect <- st_intersects(usshp,DiLaneCounts, dist=0)

usshp$Project[which(sapply(DiLaneIntersect, length) > 0)] <- 'DiLane WMA'

# cpal <- brewer.pal(name='BuPu', n=9)
map <- tm_shape(usshp) +
  tm_fill("Project", palette= '-BuPu')
tmap_save(map, 'R/output/YeiserProjectMap.png')



