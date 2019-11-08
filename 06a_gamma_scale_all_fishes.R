##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# code to do gamma-scale analysis of all species combined, two protection classes 

# clean data and combine with traits
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/01a_data_join_and_clean.R')

# the problem, we have more samples in protected areas
mpa3_reduced %>% 
  group_by(alt_enforcement) %>% 
  summarise(N_site = n_distinct(site),
            N_locations = n_distinct(lat, lon))

# the solution, bootstrap resampling: prepare the data for bootstrap resampling of sites
gamma_dat <- mpa3_reduced %>% 
  # create single covariate for location
  unite(location, c(lat, lon), remove = F) %>% 
  # collate abundances of each species at each location (these are alpha-scale samples)
  group_by(alt_enforcement, location, species) %>% 
  summarise(N = sum(sp.n)) %>% 
  ungroup() %>% 
  group_by(alt_enforcement, location) %>% 
  nest(species, N) %>% 
  ungroup()
  

# for n_samps, get 35 sites (alpha samples) from fished and 35 from protected
n_sites = 35
n_samps <- 200
gamma_metrics <- tibble()
for(i in 1:n_samps){
  print(i)
  # # get 41 random id's to calculate gamma-scale metrics
  # rows2get = sample(1:43, size = 41, replace = F)
  # 
  # get these n_sites rows and calculate alpha S 
  alpha_sub_samp <- gamma_dat %>% 
    # from each group
    group_by(alt_enforcement) %>% 
    # get 35 rows
    sample_n(n_sites, replace = F) %>% 
    # unnest the SADs
    unnest() %>% 
    # calculate N, PIE, S for each site
    group_by(alt_enforcement, location) %>% 
    mutate(alphaS = n_distinct(species),
           alphaN = sum(N),
           alpha_Spie = vegan::diversity(N, index = 'invsimpson')) %>% 
    ungroup() %>% 
    # get the minimum N and mean S for each treatment
    group_by(alt_enforcement) %>% 
    mutate(min_alpha_N = min(alphaN),
           mean_alpha_S = mean(alphaS),
           mean_alpha_Spie = mean(alpha_Spie)) %>% 
    ungroup()
  
  # need alpha Sn for beta-Sn (see Chase et al. 2018 Ecol Lett for beta-Sn introduction, nothing is going on
  # so we chose not to present it here)
  alpha_Sn_sub_samp <- alpha_sub_samp %>% 
    group_by(alt_enforcement, location) %>% 
    nest(N, min_alpha_N) %>% 
    mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$N, method = 'indiv',
                                                    effort = unique(.x$min_alpha_N)))) %>% 
    ungroup() %>% 
    unnest(Sn) %>% 
    group_by(alt_enforcement) %>% 
    mutate(mean_alpha_Sn = mean(Sn))
    
  
  # aggregate same sub sample for gamma calculations
  sub_samp <- alpha_sub_samp %>% 
    # aggregate data to gamma scale
    group_by(alt_enforcement, species) %>% 
    summarise(N = sum(N)) %>% 
    ungroup() %>% 
    # get minimum N for Sn
    group_by(alt_enforcement) %>% 
    mutate(totalN = sum(N)) %>% 
    ungroup() %>% 
    mutate(minN = min(totalN))
  
  # calculate Sn(s)
  gamma_Sn_sub_samp <- sub_samp %>% 
    # add min_alpha_N for rarefying gamma down to an alpha-scale N
    left_join(alpha_sub_samp %>% 
                dplyr::distinct(alt_enforcement, min_alpha_N),
              by = 'alt_enforcement') %>% 
    group_by(alt_enforcement) %>% 
    # gamma Sn (for gamma-scale and alpha-scale minN)
    nest(N, minN, min_alpha_N) %>% 
    mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$N, 
                                                    method = 'indiv',
                                                    effort = unique(.x$minN))),
           Sn_alpha = purrr::map(data, ~mobr::rarefaction(.x$N, 
                                                    method = 'indiv',
                                                    effort = unique(.x$min_alpha_N)))) %>% 
    unnest(Sn, Sn_alpha)
  
  # calculate the metrics we want
  gamma_metrics <- gamma_metrics %>% 
    bind_rows(sub_samp %>% 
                group_by(alt_enforcement) %>% 
                summarise(totalN = sum(N),
                          S = n_distinct(species),
                          ENSPIE = vegan::diversity(N, index = 'invsimpson'),
                          S_PIE = mobr::calc_PIE(N, ENS = T)) %>% 
    # add counter for sample based rarefaction
    mutate(gamma_Sn = gamma_Sn_sub_samp$Sn,
           gamma_Sn_alphaN = gamma_Sn_sub_samp$Sn_alpha,
           alpha_S = unique(alpha_sub_samp$mean_alpha_S),
           alpha_Spie = unique(alpha_sub_samp$mean_alpha_Spie),
           alpha_Sn = unique(alpha_Sn_sub_samp$mean_alpha_Sn),
           resample = i))
}

# summarise the resamples
gamma_boot_results <- gamma_metrics %>% 
  # calculate beta-diversities (beta=gamma/alpha) 
  mutate(beta_S = S/alpha_S,
         beta_S_PIE = ENSPIE/alpha_Spie,
         beta_Sn = gamma_Sn_alphaN/alpha_Sn) %>% 
  group_by(alt_enforcement) %>% 
  summarise(N_mu = mean(totalN),
            N_median = median(totalN),
            N_Q95 = quantile(totalN, probs = 0.95, names = F),
            N_Q5 = quantile(totalN, probs = 0.05, names = F),
            S_mu = mean(S),
            S_median = median(S),
            S_Q95 = quantile(S, probs = 0.95, names = F),
            S_Q5 = quantile(S, probs = 0.05, names = F),
            ENSPIE_mu = mean(ENSPIE),
            ENSPIE_median = median(ENSPIE),
            ENSPIE_Q95 = quantile(ENSPIE, probs = 0.95, names = F),
            ENSPIE_Q5 = quantile(ENSPIE, probs = 0.05, names = F),
            Sn_median = median(gamma_Sn),
            Sn_Q95 = quantile(gamma_Sn, probs = 0.95, names = F),
            Sn_Q5 = quantile(gamma_Sn, probs = 0.05, names = F),
            # and the beta = gamma/alpha diversitites
            beta_S_median = median(beta_S),
            beta_S_Q95 = quantile(beta_S, probs = 0.95, names = F),
            beta_S_Q5 = quantile(beta_S, probs = 0.05, names = F),
            beta_S_PIE_median = median(beta_S_PIE),
            beta_S_PIE_Q95 = quantile(beta_S_PIE, probs = 0.95, names = F),
            beta_S_PIE_Q5 = quantile(beta_S_PIE, probs = 0.05, names = F),
            beta_Sn_median = median(beta_Sn),
            beta_Sn_Q95 = quantile(beta_Sn, probs = 0.95, names = F),
            beta_Sn_Q5 = quantile(beta_Sn, probs = 0.05, names = F)) 


# plot results
gamma_S_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = alt_enforcement, y = S_median, colour = alt_enforcement),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = alt_enforcement, ymin = S_Q5, ymax = S_Q95, 
                    colour = alt_enforcement),
                size = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = 'Species richness (S)'#,
       # tag = '(b)'
       ) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))


gamma_S_PIE_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = alt_enforcement, y = ENSPIE_median, colour = alt_enforcement),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = alt_enforcement, ymin = ENSPIE_Q5, ymax = ENSPIE_Q95, 
                    colour = alt_enforcement),
                size = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(S[PIE]))#,
       # subtitle = 'All fish combined',
       # tag = '(a)'
       ) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))


beta_S_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = alt_enforcement, y = beta_S_median, colour = alt_enforcement),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = alt_enforcement, ymin = beta_S_Q5, ymax = beta_S_Q95, 
                    colour = alt_enforcement),
                size = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(italic(beta),'-S'))#,
       # tag = '(b)'
       ) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))

beta_S_PIE_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = alt_enforcement, y = beta_S_PIE_median, colour = alt_enforcement),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = alt_enforcement, ymin = beta_S_PIE_Q5, ymax = beta_S_PIE_Q95, 
                    colour = alt_enforcement),
                size = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(italic(beta), '-', S[PIE]))#,
       # subtitle = 'All fishes combined',
       # tag = '(a)'
       ) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))


# # plot all gamma-scale results
# cowplot::plot_grid(gamma_S_PIE_all, gamma_S_all, nrow = 1)
# ggsave('~/Dropbox/1current/mediterranean_mpa/presentation/iDiv_2019/figs/gamma_div_all.png',
#        height = 100, width = 200, units = 'mm')
# 
# # plot all beta-scale results
# cowplot::plot_grid(beta_S_PIE_all, beta_S_all, nrow = 1)
# ggsave('~/Dropbox/1current/mediterranean_mpa/presentation/iDiv_2019/figs/beta_div_all.png',
#        height = 100, width = 200, units = 'mm')
# 
