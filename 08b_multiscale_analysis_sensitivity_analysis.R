##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# this code runs the multiscale analysis to examine whether results in the main text for fishes with high and 
# low sensitivity to exploitation are sensitive to the percentiles used to determine sensitivity

# run code to load fish sample data (we need this for the geographic locations)
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/01a_data_join_and_clean.R')

library(mobr)


##==============================================================================================================
##  v10_90 (bottom 10% of vulnerability scores are designated as having low sensitivity)
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v10_90 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v10_90 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v10_90 <- comm_alt_enfcmt_v10_90[,c('site', 'alt_enforcement', 'v10_90', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v10_90)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v10_90$alt_enforcement <- factor(env_alt_enfcmt_v10_90$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v10_90 <- data.matrix(comm_alt_enfcmt_v10_90[,-c(1:5)])

##	make comm object required by mobr: need to create one for each vulnerability class
alt_enfcmt_v10_90_low <- make_mob_in(abund_alt_enfcmt_v10_90[which(env_alt_enfcmt_v10_90$v=='low'),],
 		env_alt_enfcmt_v10_90[which(env_alt_enfcmt_v10_90$v=='low'),], latlong = TRUE)
alt_enfcmt_v10_90_high <- make_mob_in(abund_alt_enfcmt_v10_90[which(env_alt_enfcmt_v10_90$v=='high'),],
 		env_alt_enfcmt_v10_90[which(env_alt_enfcmt_v10_90$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v10_90[which(env_alt_enfcmt_v10_90$v=='low'),])
plot_N(abund_alt_enfcmt_v10_90[which(env_alt_enfcmt_v10_90$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v10_90_low_mobrTest <- get_delta_stats(alt_enfcmt_v10_90_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v10_90_low_mobrTest,  trt_group='protected', ref_group='fished', same_scale=F)
##	high vulnerability 
alt_enfcmt_v10_90_high_mobrTest <- get_delta_stats(alt_enfcmt_v10_90_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v10_90_high_mobrTest,  trt_group='protected', ref_group='fished', same_scale=F)
##==============================================================================================================

##==============================================================================================================
##  v20_80
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v20_80 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v20_80 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v20_80 <- comm_alt_enfcmt_v20_80[,c('site', 'alt_enforcement', 'v20_80', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v20_80)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v20_80$alt_enforcement <- factor(env_alt_enfcmt_v20_80$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v20_80 <- data.matrix(comm_alt_enfcmt_v20_80[,-c(1:5)])

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v20_80_low <- make_mob_in(abund_alt_enfcmt_v20_80[which(env_alt_enfcmt_v20_80$v=='low'),],
 		env_alt_enfcmt_v20_80[which(env_alt_enfcmt_v20_80$v=='low'),], latlong = TRUE)
alt_enfcmt_v20_80_high <- make_mob_in(abund_alt_enfcmt_v20_80[which(env_alt_enfcmt_v20_80$v=='high'),],
 		env_alt_enfcmt_v20_80[which(env_alt_enfcmt_v20_80$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v20_80[which(env_alt_enfcmt_v20_80$v=='low'),])
plot_N(abund_alt_enfcmt_v20_80[which(env_alt_enfcmt_v20_80$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v20_80_low_mobrTest <- get_delta_stats(alt_enfcmt_v20_80_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v20_80_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability (two tests only: SAD, N [aggregation for later...])
alt_enfcmt_v20_80_high_mobrTest <- get_delta_stats(alt_enfcmt_v20_80_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v20_80_high_mobrTest,  'protected', 'fished', same_scale=F)
##==============================================================================================================

##==============================================================================================================
##  v30_70
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v30_70 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v30_70 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v30_70 <- comm_alt_enfcmt_v30_70[,c('site', 'alt_enforcement', 'v30_70', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v30_70)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v30_70$alt_enforcement <- factor(env_alt_enfcmt_v30_70$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v30_70 <- data.matrix(comm_alt_enfcmt_v30_70[,-c(1:5)])

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v30_70_low <- make_mob_in(abund_alt_enfcmt_v30_70[which(env_alt_enfcmt_v30_70$v=='low'),],
 		env_alt_enfcmt_v30_70[which(env_alt_enfcmt_v30_70$v=='low'),], latlong = TRUE)
alt_enfcmt_v30_70_high <- make_mob_in(abund_alt_enfcmt_v30_70[which(env_alt_enfcmt_v30_70$v=='high'),],
 		env_alt_enfcmt_v30_70[which(env_alt_enfcmt_v30_70$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v30_70[which(env_alt_enfcmt_v30_70$v=='low'),])
plot_N(abund_alt_enfcmt_v30_70[which(env_alt_enfcmt_v30_70$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v30_70_low_mobrTest <- get_delta_stats(alt_enfcmt_v30_70_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v30_70_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability
alt_enfcmt_v30_70_high_mobrTest <- get_delta_stats(alt_enfcmt_v30_70_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v30_70_high_mobrTest,  'protected', 'fished', same_scale=F)
##==============================================================================================================

##==============================================================================================================
##  v40_60
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v40_60 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v40_60 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v40_60 <- comm_alt_enfcmt_v40_60[,c('site', 'alt_enforcement', 'v40_60', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v40_60)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v40_60$alt_enforcement <- factor(env_alt_enfcmt_v40_60$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v40_60 <- data.matrix(comm_alt_enfcmt_v40_60[,-c(1:5)])

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v40_60_low <- make_mob_in(abund_alt_enfcmt_v40_60[which(env_alt_enfcmt_v40_60$v=='low'),],
 		env_alt_enfcmt_v40_60[which(env_alt_enfcmt_v40_60$v=='low'),], latlong = TRUE)
alt_enfcmt_v40_60_high <- make_mob_in(abund_alt_enfcmt_v40_60[which(env_alt_enfcmt_v40_60$v=='high'),],
 		env_alt_enfcmt_v40_60[which(env_alt_enfcmt_v40_60$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v40_60[which(env_alt_enfcmt_v40_60$v=='low'),])
plot_N(abund_alt_enfcmt_v40_60[which(env_alt_enfcmt_v40_60$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v40_60_low_mobrTest <- get_delta_stats(alt_enfcmt_v40_60_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v40_60_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability
alt_enfcmt_v40_60_high_mobrTest <- get_delta_stats(alt_enfcmt_v40_60_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v40_60_high_mobrTest,  'protected', 'fished', same_scale=F)
##==============================================================================================================

##==============================================================================================================
##  v50_50
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v50_50 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v50_50 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v50_50 <- comm_alt_enfcmt_v50_50[,c('site', 'alt_enforcement', 'v50_50', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v50_50)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v50_50$alt_enforcement <- factor(env_alt_enfcmt_v50_50$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v50_50 <- data.matrix(comm_alt_enfcmt_v50_50[,-c(1:5)])

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v50_50_low <- make_mob_in(abund_alt_enfcmt_v50_50[which(env_alt_enfcmt_v50_50$v=='low'),],
 		env_alt_enfcmt_v50_50[which(env_alt_enfcmt_v50_50$v=='low'),], latlong = TRUE)
alt_enfcmt_v50_50_high <- make_mob_in(abund_alt_enfcmt_v50_50[which(env_alt_enfcmt_v50_50$v=='high'),],
 		env_alt_enfcmt_v50_50[which(env_alt_enfcmt_v50_50$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v50_50[which(env_alt_enfcmt_v50_50$v=='low'),])
plot_N(abund_alt_enfcmt_v50_50[which(env_alt_enfcmt_v50_50$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v50_50_low_mobrTest <- get_delta_stats(alt_enfcmt_v50_50_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v50_50_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability (two tests only: SAD, N [aggregation for later...])
alt_enfcmt_v50_50_high_mobrTest <- get_delta_stats(alt_enfcmt_v50_50_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v50_50_high_mobrTest,  'protected', 'fished', same_scale=T)
##==============================================================================================================

##==============================================================================================================
##  v60_40
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v60_40 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v60_40 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v60_40 <- comm_alt_enfcmt_v60_40[,c('site', 'alt_enforcement', 'v60_40', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v60_40)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v60_40$alt_enforcement <- factor(env_alt_enfcmt_v60_40$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v60_40 <- data.matrix(comm_alt_enfcmt_v60_40[,-c(1:5)])

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v60_40_low <- make_mob_in(abund_alt_enfcmt_v60_40[which(env_alt_enfcmt_v60_40$v=='low'),],
 		env_alt_enfcmt_v60_40[which(env_alt_enfcmt_v60_40$v=='low'),], latlong = TRUE)
alt_enfcmt_v60_40_high <- make_mob_in(abund_alt_enfcmt_v60_40[which(env_alt_enfcmt_v60_40$v=='high'),],
 		env_alt_enfcmt_v60_40[which(env_alt_enfcmt_v60_40$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v60_40[which(env_alt_enfcmt_v60_40$v=='low'),])
plot_N(abund_alt_enfcmt_v60_40[which(env_alt_enfcmt_v60_40$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v60_40_low_mobrTest <- get_delta_stats(alt_enfcmt_v60_40_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)

##	high vulnerability (two tests only: SAD, N [aggregation for later...])
alt_enfcmt_v60_40_high_mobrTest <- get_delta_stats(alt_enfcmt_v60_40_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v60_40_high_mobrTest,  trt_group='protected', ref_group='fished', same_scale=F)
##==============================================================================================================

##==============================================================================================================
##  v80_20
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v80_20 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v80_20 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v80_20 <- comm_alt_enfcmt_v80_20[,c('site', 'alt_enforcement', 'v80_20', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v80_20)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v80_20$alt_enforcement <- factor(env_alt_enfcmt_v80_20$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v80_20 <- data.matrix(comm_alt_enfcmt_v80_20[,-c(1:5)])

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v80_20_low <- make_mob_in(abund_alt_enfcmt_v80_20[which(env_alt_enfcmt_v80_20$v=='low'),],
 		env_alt_enfcmt_v80_20[which(env_alt_enfcmt_v80_20$v=='low'),], latlong = TRUE)
alt_enfcmt_v80_20_high <- make_mob_in(abund_alt_enfcmt_v80_20[which(env_alt_enfcmt_v80_20$v=='high'),],
 		env_alt_enfcmt_v80_20[which(env_alt_enfcmt_v80_20$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v80_20[which(env_alt_enfcmt_v80_20$v=='low'),])
plot_N(abund_alt_enfcmt_v80_20[which(env_alt_enfcmt_v80_20$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v80_20_low_mobrTest <- get_delta_stats(alt_enfcmt_v80_20_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v80_20_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability 
alt_enfcmt_v80_20_high_mobrTest <- get_delta_stats(alt_enfcmt_v80_20_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v80_20_high_mobrTest,  trt_group='protected', ref_group='fished', same_scale=F)
##==============================================================================================================

##==============================================================================================================
##  v90_10
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v90_10 <- reshape::cast(mpa3_reduced, site + alt_enforcement + v90_10 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')

##	reserve and location data
env_alt_enfcmt_v90_10 <- comm_alt_enfcmt_v90_10[,c('site', 'alt_enforcement', 'v90_10', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v90_10)[c(4,5)] <- c('x', 'y')
env_alt_enfcmt_v90_10$alt_enforcement <- factor(env_alt_enfcmt_v90_10$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v90_10 <- data.matrix(comm_alt_enfcmt_v90_10[,-c(1:5)])

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v90_10_low <- make_mob_in(abund_alt_enfcmt_v90_10[which(env_alt_enfcmt_v90_10$v=='low'),],
 		env_alt_enfcmt_v90_10[which(env_alt_enfcmt_v90_10$v=='low'),], latlong = TRUE)
alt_enfcmt_v90_10_high <- make_mob_in(abund_alt_enfcmt_v90_10[which(env_alt_enfcmt_v90_10$v=='high'),],
 		env_alt_enfcmt_v90_10[which(env_alt_enfcmt_v90_10$v=='high'),], latlong = TRUE)

##	check assumption of linear increase in N with sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v90_10[which(env_alt_enfcmt_v90_10$v=='low'),])
plot_N(abund_alt_enfcmt_v90_10[which(env_alt_enfcmt_v90_10$v=='high'),])

##	mobr multiscale analysis
alt_enfcmt_v90_10_low_mobrTest <- get_delta_stats(alt_enfcmt_v90_10_low, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v90_10_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability 
alt_enfcmt_v90_10_high_mobrTest <- get_delta_stats(alt_enfcmt_v90_10_high, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_v90_10_high_mobrTest,  trt_group='protected', ref_group='fished', same_scale=F)
##==============================================================================================================

# save(alt_enfcmt, alt_enfcmt_mobrTest, 
#   alt_enfcmt_v10_90_high, alt_enfcmt_v10_90_high_mobrTest, 
#   alt_enfcmt_v10_90_low, alt_enfcmt_v10_90_low_mobrTest, 
#   alt_enfcmt_v20_80_high, alt_enfcmt_v20_80_high_mobrTest, 
#   alt_enfcmt_v20_80_low, alt_enfcmt_v20_80_low_mobrTest, 
#   alt_enfcmt_v30_70_high, alt_enfcmt_v30_70_high_mobrTest, 
#   alt_enfcmt_v30_70_low, alt_enfcmt_v30_70_low_mobrTest, 
#   alt_enfcmt_v40_60_high, alt_enfcmt_v40_60_high_mobrTest, 
#   alt_enfcmt_v40_60_low, alt_enfcmt_v40_60_low_mobrTest, 
#   alt_enfcmt_v50_50_high, alt_enfcmt_v50_50_high_mobrTest, 
#   alt_enfcmt_v50_50_low, alt_enfcmt_v50_50_low_mobrTest, 
#   alt_enfcmt_v60_40_high, alt_enfcmt_v60_40_high_mobrTest, 
#   alt_enfcmt_v60_40_low, alt_enfcmt_v60_40_low_mobrTest, 
#   alt_enfcmt_v70_30_high, alt_enfcmt_v70_30_high_mobrTest, 
#   alt_enfcmt_v70_30_low, alt_enfcmt_v70_30_low_mobrTest, 
#   alt_enfcmt_v80_20_high, alt_enfcmt_v80_20_high_mobrTest, 
#   alt_enfcmt_v80_20_low, alt_enfcmt_v80_20_low_mobrTest, 
#   alt_enfcmt_v90_10_high, alt_enfcmt_v90_10_high_mobrTest, 
#   alt_enfcmt_v90_10_low, alt_enfcmt_v90_10_low_mobrTest,
#   file = 'mobr_multiscale_sensitivity.Rdata')