##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# code to fit models to alpha-scale metrics (two protection classes), 
# repeat analyses for fishes with high and low sensitivity to exploitation

source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/03a_alpha_scale_wrangle.R')

#  models include the potential confounders to get the marginal reserve effects on N, S, etc...
library(brms)

# poisson error for species richness
S_alpha_mixed <- brms::brm(S ~ alt_enforcement + sstmean + chlomean +
                             nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                             (1 | site),
                           data = alpha_summary, family = 'poisson', cores = 4, chains = 4)
plot(S_alpha_mixed)
pp_check(S_alpha_mixed) # 

# lognormal error for Sn
Sn_alpha_mixed <- brms::brm(Sn ~ alt_enforcement + sstmean + chlomean +
                              nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                              (1 | site),
                            data = alpha_summary, family = 'lognormal', cores = 4, chains = 4,
                            control = list(adapt_delta = 0.9))
plot(Sn_alpha_mixed)
pp_check(Sn_alpha_mixed) # 

# lognormal error for S_PIE
S_PIE_alpha_mixed <- brms::brm(ENSPIE ~ alt_enforcement + sstmean + chlomean +
                                 nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                 (1 | site),
                               data = alpha_summary, family = 'lognormal', cores = 4, chains = 4)

plot(S_PIE_alpha_mixed)
pp_check(S_PIE_alpha_mixed) # 

# lognormal error for N
N_alpha_mixed <- brms::brm(N ~ alt_enforcement + sstmean + chlomean +
                             nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                             (1 | site),
                           data = alpha_summary, family = 'lognormal', cores = 4, chains = 4)
plot(N_alpha_mixed)
pp_check(N_alpha_mixed) # 

# save(alpha_summary, S_alpha_mixed, Sn_alpha_mixed, S_PIE_alpha_mixed, N_alpha_mixed,
#      file = 'alpha_data_and_models_revision.Rdata')

# poisson error first for species richness for fishes with low sensitivity to fishing--------
S_alpha_mixed_low <- brms::brm(S ~ alt_enforcement + sstmean + chlomean +
                                 nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                 (1 | site),
                               data = alpha_summary_sensitivity %>% filter(v70_30=='low'), 
                               family = 'poisson', cores = 4, chains = 4)
plot(S_alpha_mixed_low)
pp_check(S_alpha_mixed_low) # 

# lognormal error for Sn
Sn_alpha_mixed_low <- brms::brm(Sn ~ alt_enforcement + sstmean + chlomean +
                                  nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                  (1 | site),
                                data = alpha_summary_sensitivity %>% filter(v70_30=='low'), 
                                family = 'lognormal', cores = 4, chains = 4)
plot(Sn_alpha_mixed_low)
pp_check(Sn_alpha_mixed_low) # 

# lognormal error for S_PIE
S_PIE_alpha_mixed_low <- brms::brm(ENSPIE ~ alt_enforcement + sstmean + chlomean +
                                     nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                     (1 | site),
                                   data = alpha_summary_sensitivity %>% filter(v70_30=='low'), 
                                   family = 'lognormal', cores = 4, chains = 4)

plot(S_PIE_alpha_mixed_low)
pp_check(S_PIE_alpha_mixed_low) # 

# lognormal error for N
N_alpha_mixed_low <- brms::brm(N ~ alt_enforcement + sstmean + chlomean +
                                 nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                 (1 | site),
                               data = alpha_summary_sensitivity %>% filter(v70_30=='low'), 
                               family = 'lognormal', cores = 4, chains = 4)
plot(N_alpha_mixed_low)
pp_check(N_alpha_mixed_low) # 

# high sensitivity------------------------
S_alpha_mixed_high <- brms::brm(S ~ alt_enforcement + sstmean + chlomean +
                                  nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                  (1 | site),
                                data = alpha_summary_sensitivity %>% filter(v70_30=='high'), 
                                family = 'poisson', cores = 4, chains = 4)
plot(S_alpha_mixed_high)
pp_check(S_alpha_mixed_high) # 

# lognormal error for Sn
Sn_alpha_mixed_high <- brms::brm(Sn ~ alt_enforcement + sstmean + chlomean +
                                   nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                   (1 | site),
                                 # fit to the Sn data only (not those that returned S due to N < 5)
                                 data = alpha_summary_sensitivity %>% filter(v70_30=='high' & N > 5), 
                                 family = 'lognormal', cores = 4, chains = 4)
plot(Sn_alpha_mixed_high)
pp_check(Sn_alpha_mixed_high) # not great, check other distribution...poisson best.


# lognormal error for S_PIE
S_PIE_alpha_mixed_high <- brms::brm(ENSPIE ~ alt_enforcement + sstmean + chlomean +
                                      nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                      (1 | site),
                                    data = alpha_summary_sensitivity %>% filter(v70_30=='high'), 
                                    family = 'lognormal', cores = 4, chains = 4)

plot(S_PIE_alpha_mixed_high)
pp_check(S_PIE_alpha_mixed_high) # 

# lognormal error for N
N_alpha_mixed_high <- brms::brm(N ~ alt_enforcement + sstmean + chlomean +
                                  nitrate + parmean + ph + phos + salinity + impacts_at_coords +
                                  (1 | site),
                                data = alpha_summary_sensitivity %>% filter(v70_30=='high'), 
                                family = 'lognormal', cores = 4, chains = 4)
plot(N_alpha_mixed_high)
pp_check(N_alpha_mixed_high) # 

# saving these are good if you want to avoid fitting them again, but due to the chainsthese fits (all combined) are ~ 530MB 

# save(mpa3_reduced,
#      alpha_summary, S_alpha_mixed, Sn_alpha_mixed, S_PIE_alpha_mixed, N_alpha_mixed,
#      alpha_summary_sensitivity, S_alpha_mixed_low, Sn_alpha_mixed_low, S_PIE_alpha_mixed_low, N_alpha_mixed_low,
#      S_alpha_mixed_high, Sn_alpha_mixed_high, S_PIE_alpha_mixed_high, N_alpha_mixed_high,
#      file = 'alpha_data_and_models_revision_less10long.Rdata')
# load('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/code/alpha_data_and_models_revision_less10long.Rdata')
