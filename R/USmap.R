# Map of my research across US
rm(list=ls())
require(sf); require(tidyverse); require(tmap); require(tmaptools) ;require(sp); require(dplyr); require(RColorBrewer)

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

usshp$Project[which(sapply(wlfwintersect, length) > 0)] <- 'Working Lands For Wildlife: population-level bobwhite response'


# Texas energy development ------------------------------------------------
TXProject_counts <- st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/TX', layer='OPJV_TXOil_SiteCoords')
TXProject_counts <- st_transform(TXProject_counts, st_crs(usshp))
TXintersect <- st_intersects(usshp,TXProject_counts, dist=0)

usshp$Project[which(sapply(TXintersect, length) > 0)] <- 'TX energy development influence on grassland birds'


# CREP --------------------------------------------------------------------
CREParea <- st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/CREP', layer='StudyArea_Shape')
CREPcounts <- read.csv('R/data/CREP/Target_Species_Observations_testdist_2016.csv')
coordinates(CREPcounts) <- ~ Longitude + Latitude
proj4string(CREPcounts) <- st_crs(CREParea)$proj4string
CREPcounts <- st_as_sf(CREPcounts, 'POINTS')
plot(CREPcounts$geometry)
CREPcounts <-  st_transform(CREPcounts, st_crs(usshp))

CREPintersect <- st_intersects(usshp,CREPcounts, dist=0)

usshp$Project[which(sapply(CREPintersect, length) > 0)] <- 'KY Conservation Reserve Enhancement Program and grassland birds'


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

usshp$Project[which(sapply(CIPintersect, length) > 0)] <- 'NBCI Coordinated Implementation Program'

# DiLane  ------------------------------------------------------------------
DiLaneCounts <-  st_read(dsn='/Users/john/Google Drive/yeisjohn.github.io/R/data/DiLane', layer='DiLaneCC_fall2016')
DiLaneCounts <- st_transform(DiLaneCounts, st_crs(usshp))
DiLaneIntersect <- st_intersects(usshp,DiLaneCounts, dist=0)

usshp$Project[which(sapply(DiLaneIntersect, length) > 0)] <- 'DiLane WMA: quantifying bobwhite harvest'

# Tall Timbers ------------------------------------------------------------------
usshp$Project[which(usshp$NAME == 'Leon County, Florida')]  <- 'Tall Timbers: bobwhite population model'

# Fort Gordon ------------------------------------------------------------------
# Goose pond - indiana harvest study
usshp$Project[which(usshp$NAME == 'Greene County, Indiana')]  <- 'Goose Pond WMA: population-level impacts of harvest on bobwhite'

# SW GA ------------------------------------------------------------------
usshp$Project[which(usshp$NAME%in%c('Calhoun County, Georgia','Thomas County, Georgia', 'Decatur County, Georgia'))] <- 'SW GA WMAs: building adaptive management plans'

# KY - herbicide and burning to reduce rank nwsg ------------------------------------------------------------------
usshp$Project[which(usshp$NAME%in%c('Union County, Kentucky',
                                    'Spencer County, Kentucky',
                                    'Anderson County, Kentucky',
                                    'Nelson County, Kentucky',
                                    'Taylor County, Kentucky',
                                    'Adair County, Kentucky',
                                    'Mercer County, Kentucky'))] <- 'Managing native grass for bobwhite using fire and herbicide'

# KY salamanders ------------------------------------------------------------------
usshp$Project[which(usshp$NAME%in%c('Fayette County, Kentucky'))] <- 'Habitat associations of stream-dwelling salamanders'

# KY mine reclaimation ------------------------------------------------------------------
usshp$Project[which(usshp$NAME%in%c('Perry County, Kentucky',
                                    'Knot County, Kentucky',
                                    'Breathitt County, Kentucky'))] <- 'Reclaiming coal mines with wildlife-friendly seed mixes'
# KY -- dog trials
usshp$Project[which(usshp$NAME%in%c('Muhlenberg County, Kentucky',
                                    'Ohio County, Kentucky'))] <- 'Hunt success: pen-reared vs wild bobwhite'

# KY -- targeted approach to bobwhite conservation
usshp$Project[which(usshp$NAME%in%c('Livingston County, Kentucky',
                                    'Hart County, Kentucky',
                                    'Madison County, Kentucky'))] <- 'A focused approach for bobwhite restoration in KY'



# cpal <- brewer.pal(name='BuPu', n=9)

statemap <- usshp %>%
  aggregate(by=list(states), FUN=mean)


display.brewer.all()
get_brewer_pal(palette='Spectral',n=13)

map <- tm_shape(usshp) +
  tm_fill("Project", palette= 'Spectral', textNA = "", colorNA='gray75') +
  tm_layout(legend.outside = TRUE, legend.outside.position='bottom', legend.text.size = 2.5, legend.title.size=3) +
  tm_shape(statemap) +
  tm_borders(col = "black", alpha=0.25)
map

tmap_save(map, 'R/output/YeiserProjectMap.png')
