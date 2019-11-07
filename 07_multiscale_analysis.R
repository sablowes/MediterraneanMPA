# mobr analysis: 
source('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/code/01_balance_samples.R')

##	first without vulnerability...
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt <- reshape::cast(mpa3_reduced, site + alt_enforcement + lat + lon ~ species, fun.aggregate=sum, value='sp.n')
head(comm_alt_enfcmt)
str(comm_alt_enfcmt)

##	reserve and location data
env_alt_enfcmt <- comm_alt_enfcmt[,c('site', 'alt_enforcement', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt)[c(3,4)] <- c('x', 'y')
str(env_alt_enfcmt)
env_alt_enfcmt$alt_enforcement <- factor(env_alt_enfcmt$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt <- data.matrix(comm_alt_enfcmt[,-c(1:4)])
head(abund_alt_enfcmt)

##	make comm object required by mobr: need to create one for each vulnerability class
alt_enfcmt <- make_mob_in(abund_alt_enfcmt, env_alt_enfcmt)

##	assumptions (too good to be true?!)
plot_N(comm_alt_enfcmt)

##	perform tests of component changes
alt_enfcmt_stats <- get_mob_stats(alt_enfcmt, group_var='alt_enforcement', boot_groups = T)
plot(alt_enfcmt_stats)

##	continuous analysis of rarefaction curves...
alt_enfcmt_mobrTest <- get_delta_stats(alt_enfcmt, group_var='alt_enforcement', ref_group='fished', n_perm=200)
plot(alt_enfcmt_mobrTest, trt_group='protected', ref_group='fished')
stack_effects(alt_enfcmt_mobrTest, trt_group='protected')

##---------------now for fishes with differing vulnerability to fishing----------------------
##  v70_30
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v70_30 <- cast(mpa3_reduced, site + alt_enforcement + v70_30 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')
head(comm_alt_enfcmt_v70_30)
str(comm_alt_enfcmt_v70_30)

##	reserve and location data
env_alt_enfcmt_v70_30 <- comm_alt_enfcmt_v70_30[,c('site', 'alt_enforcement', 'v70_30', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v70_30)[c(4,5)] <- c('x', 'y')
str(env_alt_enfcmt_v70_30)
env_alt_enfcmt_v70_30$alt_enforcement <- factor(env_alt_enfcmt_v70_30$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v70_30 <- data.matrix(comm_alt_enfcmt_v70_30[,-c(1:5)])
head(abund_alt_enfcmt_v70_30)

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v70_30_low <- make_mob_in(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='low'),],
                                     env_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='low'),])
alt_enfcmt_v70_30_high <- make_mob_in(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='high'),],
                                      env_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='high'),])

##	assumptions (too good to be true?!)
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='low'),])
plot_N(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='high'),])

##	perform tests of component changes
stats_v70_30_low <- get_mob_stats(alt_enfcmt_v70_30_low, group_var='alt_enforcement', 
                                  boot_groups = T)
##	visual output 
plot(stats_v70_30_low, multi_panel=T)

##	stats for the highly vulnerable fishes (90 percentile)
stats_v70_30_high <- get_mob_stats(alt_enfcmt_v70_30_high, group_var='alt_enforcement',
                                   boot_groups = T)
##	visual output 
plot(stats_v70_30_high)

##	permutation tests...
alt_enfcmt_v70_30_low_mobrTest <- get_delta_stats(alt_enfcmt_v70_30_low, group_var='alt_enforcement', ref_group='fished', nperm=200)

##	my results have NAs in them...not sure why at this stage?
#source('/Users/jc155893/Desktop/current/mediterranean_mpa/code/plot_mob_out_HACK.R')
plot(alt_enfcmt_v70_30_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability 
alt_enfcmt_v70_30_high_mobrTest <- get_delta_stats(alt_enfcmt_v70_30_high, group_var='alt_enforcement', ref_group='fished', nper=200)
plot(alt_enfcmt_v70_30_high_mobrTest,  trt_group='protected', ref_group='fished', same_scale=F)



##	now want to look at the mobr results all on one figure
source('~/Dropbox/1current/R_random/functions/mobr_stack_hack.R')
source('~/Dropbox/1current/R_random/functions/mobr_stack_hack_low.R')
load('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/data_results/mobr_output.Rdata')

png('mobr_allEffects_allFishes.png', width=200, height=200, units='mm', res=300)
par(mar=c(5,7,6,4), cex.lab=2, cex.axis=1.5)
stack_hack(alt_enfcmt_mobrTest, trt_group='protected', legend=T, plot=T, uncertainty = T)
dev.off()

png('mobr_allEffects_high.png', width=200, height=200, units='mm', res=300)
par(mar=c(5,7,6,4), cex.lab=2, cex.axis=1.5)
stack_hack(alt_enfcmt_v70_30_high_mobrTest, trt_group='protected', legend=T, plot=T, uncertainty = T)
dev.off()

png('mobr_allEffects_low.png', width=200, height=200, units='mm', res=300)
par(mar=c(5,7,6,4), cex.lab=2, cex.axis=1.5)
stack_hack(alt_enfcmt_v70_30_low_mobrTest, trt_group='protected', legend=T, plot=T, uncertainty = T)
dev.off()

# pdf('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/figs/allEffects_3panel.pdf', width=290, height = 100)
par(mfrow=c(1,3), mar = c(5, 7, 7, 0.5), cex = 0.65)
stack_hack(alt_enfcmt_mobrTest, trt_group='protected', 
           legend=T, plot=T, uncertainty = T, 
           ylab = 'Contribution to species richness\ngains in MPAs',
           main_title = 'a. All fishes')
stack_hack(alt_enfcmt_v70_30_high_mobrTest, trt_group='protected', 
           plot=T, uncertainty = T, 
           xlab = 'Number of sites', xlab_top = 'Number of individuals',
           main_title = 'b. Fishes with high sensitivity')
stack_hack_low(alt_enfcmt_v70_30_low_mobrTest, trt_group='protected',  plot=T, 
           uncertainty = T,
           main_title = 'c. Fishes with low sensitivity')
# dev.off()

low_mobr <- stack_hack(alt_enfcmt_v70_30_low_mobrTest, plot = F, trt_group = 'protected') %>% 
  as_tibble()
mobr::overlap_effects(alt_enfcmt_v70_30_low_mobrTest, trt_group = 'protected', rescale = 'density_stat')
overlap_effects(inv_mob_out, trt_group = 'invaded', rescale = 'density_stat')
