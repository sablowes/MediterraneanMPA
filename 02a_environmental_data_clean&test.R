##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# this code loads environmental data and tests whether it is different between fished and protected areas

library(raster)

# run code to load fish sample data (we need this for the geographic locations)
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/01a_data_join_and_clean.R')

# temperature and productivity data for each location from Bio-ORACLE
##	now load the files for the temperature and chlorophyll (productivity) data
sst <- list.files('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/data/SST', pattern='asc', full.names=T)
chloro <- list.files('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/data/chlorophyll', pattern='asc', full.names=T)
other <- list.files('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/data/other', pattern = 'asc', full.names = T)
# and the impacts data from Micheli et al 2013: downloaded from: https://www.nceas.ucsb.edu/globalmarine/mediterranean (1. model) 07/01/2019
impacts <- list.files('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/data/', pattern = 'tif', full.names = T)


##	create 'stack' of the different layers
sst_stack <- raster::stack(sst)
chloro_stack <- raster::stack(chloro)
other_stack <- raster::stack(other)

# check CRS
micheli_impacts <- raster('~/Dropbox/1current/mediterranean_mpa/data/Micheli_impacts_model.tif')
crs(micheli_impacts)

# reproject micheli to be compatible with the other layers
micheli_impacts_rp <- 
  projectRaster(micheli_impacts, 
                crs = "+proj=longlat +lat_0=38 +lon_0=18 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")


##	extract the variables at the occurrence locations
sst_at_coords <- raster::extract(sst_stack, unique_coords2[,c('lon', 'lat')])
chloro_at_coords <- raster::extract(chloro_stack, unique_coords2[,c('lon', 'lat')])
other_at_coords <- raster::extract(other_stack, unique_coords2[,c('lon', 'lat')])
impacts_at_coords <- raster::extract(micheli_impacts_rp, unique_coords2[,c('lon', 'lat')])


# get values for cells with NA, use nearest non-NA value
# locations that are missing
outsidePts <- unique_coords2[is.na(impacts_at_coords), c('lon', 'lat')]
# nearest location (within 750m) with a value
nearest <- seegSDM::nearestLand(outsidePts, micheli_impacts_rp, 750)
# impact at the nearby locations
missing_impacts <- raster::extract(micheli_impacts_rp, nearest)
# put the missing values back into original vector of impacts
impacts_at_coords[is.na(impacts_at_coords)] <- missing_impacts

##	 combine variables and coordinates
mpa_covariates <- cbind.data.frame(unique_coords2, sst_at_coords, chloro_at_coords, other_at_coords, impacts_at_coords) %>% 
  as_tibble()

# get rugosity (a measure of habitat heterogeneity along each transect) from the orginal data
rug <- mpa3_reduced %>% 
  distinct(site, alt_enforcement, lat, lon, rug) %>% 
  group_by(site, alt_enforcement, lat, lon) %>% 
  summarise(rug_mu = mean(rug)) %>% 
  ungroup()

# missing rug at some locations, use mean of locations at the same site
# rug %>% 
#   filter(is.na(rug_mu))

dragonera_rug <- rug %>% 
  filter(site=='DRAGONERA' &!is.na(rug_mu)) %>% 
  summarise(rug_mu = mean(rug_mu))
  
medes_rug <- rug %>% 
  filter(site=='MEDES' &!is.na(rug_mu)) %>% 
  summarise(rug_mu = mean(rug_mu))

# put these rug values back into the dataframe
rug$rug_mu[which(rug$site=='DRAGONERA' & is.na(rug$rug_mu))] <- dragonera_rug$rug_mu
rug$rug_mu[which(rug$site=='MEDES' & is.na(rug$rug_mu))] <- medes_rug$rug_mu

mpa_covariates <- left_join(mpa_covariates,
                            rug %>% dplyr::select(-site), 
                            by = c('alt_enforcement', 'lat', 'lon'))

# recombine with the dataframe
mpa3_reduced <- inner_join(mpa3_reduced, mpa_covariates, 
                           by = c('lat', 'lon', 'alt_enforcement'))


# PERMANOVA for determining whether there are systematic differences
# between fished and protected sites in the 'confounding' covariates

# function to mean centre and standardise with sd of a vector
stand.fn <- function(x){
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

mpa_covariates <- mpa_covariates %>% 
  unite(coord, c(lat, lon), remove = F) 

mpa_complete <- mpa_covariates %>% 
  mutate(complete = complete.cases(mpa_covariates)) %>% 
  filter(complete == TRUE) %>% 
  # standardise covariates (different scales cause problems when using euclidian distances)
  mutate_at(.vars = vars(sstmax:rug_mu), .funs = stand.fn)
         

covariates_only <- mpa_complete %>% 
  dplyr::select(-lat, -lon, -coord, -alt_enforcement, - complete) 

euclid <- vegan::vegdist(covariates_only, method = 'euclidean')

mpa_confounders <- vegan::adonis(euclid ~ alt_enforcement, data = mpa_complete, method = 'euclidian')


# test for homogeneity of dispersions (variances)
dispersion <- vegan::betadisper(euclid, group = mpa_complete$alt_enforcement )
vegan::permutest(dispersion)

# plot(dispersion, ellipse = TRUE, hull = FALSE)


nmds <- vegan::metaMDS(euclid, distance = 'euclid')
nmds.df <- tibble(
  nmds1 = nmds$points[,1],
  nmds2 = nmds$points[,2],
  mpa_status = mpa_complete$alt_enforcement
)

# ggplot(nmds.df) +
#   geom_point(aes(x = nmds1, y = nmds2, colour = mpa_status)) +
#   stat_ellipse(aes(x = nmds1, y = nmds2, colour = mpa_status)) +
#   theme_bw()