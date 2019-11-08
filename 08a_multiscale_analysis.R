##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# this code runs the multiscale analysis for all fish combined, and for fishes with high and low sensitivity to exploitation
# presented in the main text

# NB: to just plot the results load the 'mobr_multiscale_sensitivity.Rdata' file in the data_ folder of github 
# (code for plot at end of script)

# run code to load fish sample data (we need this for the geographic locations)
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/01a_data_join_and_clean.R')

library(mobr)
##	first without vulnerability...
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt <- reshape::cast(mpa3_reduced, site + alt_enforcement + lat + lon ~ species, fun.aggregate=sum, value='sp.n')
# head(comm_alt_enfcmt)
# str(comm_alt_enfcmt)

##	reserve and location data
env_alt_enfcmt <- comm_alt_enfcmt[,c('site', 'alt_enforcement', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt)[c(3,4)] <- c('x', 'y')
# str(env_alt_enfcmt)
env_alt_enfcmt$alt_enforcement <- factor(env_alt_enfcmt$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt <- data.matrix(comm_alt_enfcmt[,-c(1:4)])
# head(abund_alt_enfcmt)

##	make comm object required by mobr: need to create one for each vulnerability class
alt_enfcmt <- make_mob_in(abund_alt_enfcmt, env_alt_enfcmt)

##	check assumption of linear increase in abundance with increased sampling effort
plot_N(comm_alt_enfcmt)

##	continuous analysis of rarefaction curves...
alt_enfcmt_mobrTest <- get_delta_stats(alt_enfcmt, group_var='alt_enforcement', ref_group='fished', n_perm=200)
# plot(alt_enfcmt_mobrTest, trt_group='protected', ref_group='fished')
# stack_effects(alt_enfcmt_mobrTest, trt_group='protected')

##---------------now for fishes with differing vulnerability to fishing----------------------
##  v70_30
##	want site x species matrix for mobr (with vulnerability this time)
comm_alt_enfcmt_v70_30 <- cast(mpa3_reduced, site + alt_enforcement + v70_30 + lat + lon ~ species, fun.aggregate=sum, value='sp.n')
# head(comm_alt_enfcmt_v70_30)
# str(comm_alt_enfcmt_v70_30)

##	reserve and location data
env_alt_enfcmt_v70_30 <- comm_alt_enfcmt_v70_30[,c('site', 'alt_enforcement', 'v70_30', 'lon', 'lat')]
##	need coords to be named x and y
names(env_alt_enfcmt_v70_30)[c(4,5)] <- c('x', 'y')
# str(env_alt_enfcmt_v70_30)
env_alt_enfcmt_v70_30$alt_enforcement <- factor(env_alt_enfcmt_v70_30$alt_enforcement, levels=c('fished','protected'))

##	separate community (i.e., abundances) from reserve and location data
abund_alt_enfcmt_v70_30 <- data.matrix(comm_alt_enfcmt_v70_30[,-c(1:5)])
# head(abund_alt_enfcmt_v70_30)

##	make comm object required by mobr: need to create 3 of these, one for each vulnerability class
alt_enfcmt_v70_30_low <- make_mob_in(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='low'),],
                                     env_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='low'),])
alt_enfcmt_v70_30_high <- make_mob_in(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='high'),],
                                      env_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='high'),])

##	check assumption of linear increase in abundance with increased sampling effort
par(mfrow=c(1,2))
plot_N(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='low'),])
plot_N(abund_alt_enfcmt_v70_30[which(env_alt_enfcmt_v70_30$v=='high'),])


##	permutation tests...
alt_enfcmt_v70_30_low_mobrTest <- get_delta_stats(alt_enfcmt_v70_30_low, group_var='alt_enforcement', ref_group='fished', nperm=200)

# plot(alt_enfcmt_v70_30_low_mobrTest,  'protected', 'fished', same_scale=F)

##	high vulnerability 
alt_enfcmt_v70_30_high_mobrTest <- get_delta_stats(alt_enfcmt_v70_30_high, group_var='alt_enforcement', ref_group='fished', nper=200)
# plot(alt_enfcmt_v70_30_high_mobrTest,  trt_group='protected', ref_group='fished', same_scale=F)



##--code to plot results as 'stacked plot'
##	now want to look at the mobr results all on one figure

# these files are modifications to the mobr 'overlap_effects' function
# for plotting the uncertainty; colour-blind friendly colour scheme; multi-panel layout and labelling (custom for this MS)
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/mobr_stack_plot.R')
# I needed to slightly modify the code further for the low sensitivity fishes: the inbalanced sampling design caused some problems for the N-effect
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/mobr_stack_plot_low.R')

load('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/data_mobr_results/mobr_multiscale_sensitivity.Rdata')

pdf('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision3_to_press/figs/Fig5.pdf', width=14, height = 6)
# png('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision3_to_press/figs/Fig5.png', width=290, height = 100, res = 600, units = 'mm')
par(mfrow=c(1,3), mar = c(5, 7, 7, 0.05), cex = 0.65,
    oma = c(.1,.1,.1,1))
stack_plot(alt_enfcmt_mobrTest, trt_group='protected', 
           legend=T, plot=T, uncertainty = T, 
           ylab = 'Contribution to species richness\ngains in MPAs',
           main_title = 'a. All fishes')
stack_plot(alt_enfcmt_v70_30_high_mobrTest, trt_group='protected', 
           plot=T, uncertainty = T, 
           xlab = 'Number of sites', xlab_top = 'Number of individuals',
           main_title = 'b. Fishes with high sensitivity')
stack_plot_low(alt_enfcmt_v70_30_low_mobrTest, trt_group='protected',  plot=T, 
           uncertainty = T,
           main_title = 'c. Fishes with low sensitivity')
dev.off()
