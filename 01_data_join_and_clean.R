##==============================================================================================================
##	code for JAE MS: Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance.
##==============================================================================================================
rm(list=ls())
library(tidyverse)
##==============================================================================================================
mpa <- read_csv('~/Dropbox/1current/Mediterranean_MPA/data_raw_and_results/mediterranean_mpa.csv')

str(mpa)	
# label = site.label (unique ID for a combination of site/sub.site/transect)
# data are arranged hierarchically: site/sub.site/transect
# plus information on the mpa e.g., total.mpa.ha, size.notake, yr.creation, age.reserve.yr, etc.

##	reduce to columns needed for inspection & analysis
names(mpa)
mpa2 <- mpa[,c('site', 'sub.site', 'transect', 'label', 'season', 'protection', 'enforcement', 'total.mpa.ha', 
               'size.notake', 'yr.creation', 'age.reserve.yr', 'rug', 'lat', 'lon', 'depth', 'species', 'sp.n')]			
##==============================================================================================================
##	combine with trait data (only want vulnerabilty for the sensitivity to fishing measure at this stage)
##	first, load trait data (downloaded from fishbase 21st August 2017)
load('~/Dropbox/1current/Mediterranean_MPA/data_raw_and_results/mpa_fishbase_traits.Rdata')
##	reduce to names and vulnerability
traits <- traits[,c('sciname','Vulnerability')]				
names(traits)[1] <- 'species'

##	change species name in traits to match species names in 
traits <- traits %>%
  separate(species, into=c('Genus', 'Species')) %>% 
  unite(species, Genus, Species, sep='.')

##	combine with original data
mpa3 <- as_tibble(inner_join(mpa2, traits, by='species'))

##	fix up enforcement levels for plotting (NA==fished==enforcement level 0)
mpa3$enforcement <- ifelse(is.na(mpa3$enforcement), 0, mpa3$enforcement)
##	alternate reinforcement coding
mpa3$alt_enforcement <- ifelse((mpa3$enforcement==0|mpa3$enforcement==1), 'fished', 'protected')
##==============================================================================================================
##	reduce spatial extent for a more balanced design
##  (this removes fished sites in the eastern mediterranean and around the boot of italy)
mpa3_reduced <- mpa3 %>% filter(lon<10)

##	check number of unique coords
unique_coords2 <- mpa3_reduced %>% group_by(alt_enforcement) %>%
  distinct(lon, lat)

##	check number of protected and fished sites in the reduced data
unique_sites <- mpa3_reduced %>% group_by(alt_enforcement) %>%
  summarise(n_sites = n_distinct(site),
            n_subSites = n_distinct(sub.site),
            n_transect = n_distinct(label),
            n_locations = n_distinct(lon, lat))


# add the sensitivity groups based on quantiles of the vulnerability score
mpa3_reduced$v10_90 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .1, .9, 1))[2], 'low', 'high')	
mpa3_reduced$v20_80 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .2, .8, 1))[2], 'low', 'high')	
mpa3_reduced$v30_70 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .3, .7, 1))[2], 'low', 'high')	
mpa3_reduced$v40_60 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .4, .6, 1))[2], 'low', 'high')	
mpa3_reduced$v50_50 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .5, .5, 1))[2], 'low', 'high')	
mpa3_reduced$v60_40 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .4, .6, 1))[3], 'low', 'high')	
mpa3_reduced$v70_30 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .3, .7, 1))[3], 'low', 'high')	
mpa3_reduced$v80_20 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .2, .8, 1))[3], 'low', 'high')	
mpa3_reduced$v90_10 <- ifelse(mpa3_reduced$Vulnerability < quantile(traits$Vulnerability, probs=c(0, .1, .9, 1))[3], 'low', 'high')	

