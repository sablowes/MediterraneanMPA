##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# code to calculate alpha-scale metrics (three protection classes for supplement)
# metrics calculated for all species combined, and separately for fishes with high and low
# sensitivity to exploitation

# clean data and combine with traits
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/02b_environmental_data_clean&test_3classes.R')

##--------first calculate alpha scale metrics----------------
# proceed wtih alpha-analysis that includes these
alpha_summary <- mpa3_reduced %>% 
  # first collate the transects at each unique location
  group_by(site, alt_enforcement, lat, lon, species) %>% 
  summarise(abundance = sum(sp.n)) %>% 
  ungroup() %>% 
  # calculate metrics for each location
  group_by(site, alt_enforcement, lat, lon) %>% 
  summarise(
    S = n_distinct(species),
    N = sum(abundance),
    S_PIE = mobr::calc_PIE(abundance, ENS = T),
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson')) %>% 
  ungroup() %>% 
  # rejoin with the covariates
  inner_join(mpa_covariates, by = c('lat', 'lon', 'alt_enforcement')) %>% 
  # standard covariates
  mutate_at(.vars = vars(sstmax:rug_mu), .funs = stand.fn) %>% 
  # add the minimum number of indiviuals for calculating ibr
  mutate(minN = min(N))

# check are these data balanced? No, 1 extra site and two extra locations for protected areas
# this will need to be accounted for when estimating gamma-diversity
alpha_summary %>% 
  group_by(alt_enforcement) %>% 
  summarise(N_sites = n_distinct(site),
            N_locations = n_distinct(lon, lat))


##------individual based rarefaction before fitting models-----------
alpha_data <- mpa3_reduced %>% 
  # first collate the transects at each unique location
  group_by(site, alt_enforcement, lat, lon, species) %>% 
  summarise(abundance = sum(sp.n)) %>% 
  ungroup() %>% 
  # add target number of indiduals
  inner_join(alpha_summary %>% ungroup() %>% 
               dplyr::select(site, alt_enforcement, lat, lon, minN),
             by = c('site', 'alt_enforcement', 'lat', 'lon')) %>% 
  # next for calculating Sn
  group_by(site, alt_enforcement, lat, lon) %>% 
  nest(species, abundance, minN) %>% 
  ungroup() %>% 
  mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$abundance, method = 'indiv', effort = unique(.x$minN)))) %>% 
  unnest(Sn)

# put Sn into the alpha_summary df
alpha_summary <- inner_join(alpha_summary %>% ungroup(), 
                            alpha_data %>% dplyr::select(site, alt_enforcement, lat, lon, Sn),
                            by = c('site', 'alt_enforcement', 'lat', 'lon'))

##---------analysis of fishes with high and low sensitivity to fishing----------
# put the sensitivity data back in:
alpha_summary_sensitivity <- mpa3_reduced %>% 
  # first collate the transects at each unique location
  group_by(site, alt_enforcement, lat, lon, species) %>% 
  summarise(abundance = sum(sp.n)) %>% 
  ungroup() %>% 
  # put sensitivity data back in
  inner_join(mpa3_reduced %>% dplyr::distinct(species, v70_30),
             by = c('species')) %>% 
  # calculate metrics for each sensitivity at eadch location
  group_by(site, alt_enforcement, v70_30, lat, lon) %>% 
  summarise(
    S = n_distinct(species),
    N = sum(abundance),
    S_PIE = mobr::calc_PIE(abundance, ENS = T),
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson')) %>% 
  ungroup() %>% 
  # rejoin with the covariates
  inner_join(mpa_covariates, by = c('lat', 'lon', 'alt_enforcement')) %>% 
  # standard covariates
  mutate_at(.vars = vars(sstmax:rug_mu), .funs = stand.fn)

# add the minimum number of indiviuals for calculating ibr (want at least 5 individuals)
minN <- alpha_summary_sensitivity %>% 
  filter(N >=5) %>% 
  group_by(v70_30) %>% 
  mutate(minN = min(N)) %>% 
  ungroup()

alpha_summary_sensitivity <- left_join(alpha_summary_sensitivity,
                                       minN %>% distinct(v70_30, minN),
                                       by = 'v70_30')
# check are these data balanced? No, 1 extra site and two extra locations for protected areas
# this will need to be accounted for when estimating gamma-diversity
alpha_summary_sensitivity %>% 
  group_by(alt_enforcement) %>% 
  summarise(N_sites = n_distinct(site),
            N_locations = n_distinct(lon, lat))


##------individual based rarefaction before fitting models-----------
alpha_data_sensitivity <- mpa3_reduced %>% 
  # first collate the transects at each unique location
  group_by(site, alt_enforcement, lat, lon, species) %>% 
  summarise(abundance = sum(sp.n)) %>% 
  ungroup() %>% 
  # put sensitivity data back in
  inner_join(mpa3_reduced %>% dplyr::distinct(species, v70_30),
             by = c('species')) %>% 
  # add target number of indiduals
  inner_join(alpha_summary_sensitivity %>% ungroup() %>% 
               dplyr::select(site, alt_enforcement, v70_30, lat, lon, minN),
             by = c('site', 'alt_enforcement', 'v70_30', 'lat', 'lon')) %>% 
  # next for calculating Sn
  group_by(site, alt_enforcement, v70_30, lat, lon) %>% 
  nest(species, abundance, minN) %>% 
  ungroup() %>% 
  mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$abundance, method = 'indiv', effort = unique(.x$minN)))) %>% 
  unnest(Sn)

# put Sn into the alpha_summary_sensitivity df
alpha_summary_sensitivity <- inner_join(alpha_summary_sensitivity %>% ungroup(), 
                                        alpha_data_sensitivity %>% dplyr::select(site, alt_enforcement, v70_30, 
                                                                                 lat, lon, Sn),
                                        by = c('site', 'alt_enforcement', 'v70_30', 'lat', 'lon'))


# save the two dataframes needed for fitting the models
# save(alpha_summary, alpha_summary_sensitivity, 
#      file = '~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/data_results/alpha_scale_revisionData_3classes.Rdata')
