##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# code to:
#   - plot the results of the models for alpha-scale response to protection 


# load the models fits if you've run the models...or, run the models
source('~/Dropbox/1current/mediterranean_mpa/MediterraneanMPA/04_alpha_analysis.R')


S_protectMarginal_all <- marginal_effects(S_alpha_mixed, effects = 'alt_enforcement', re_formula = NA, method = 'fitted')  
Sn_protectMarginal_all <- marginal_effects(Sn_alpha_mixed, effects = 'alt_enforcement', re_formula = NA, method = 'fitted')  
S_PIE_protectMarginal_all <- marginal_effects(S_PIE_alpha_mixed, effects = 'alt_enforcement', re_formula = NA, method = 'fitted')  
N_protectMarginal_all <- marginal_effects(N_alpha_mixed, effects = 'alt_enforcement', re_formula = NA, method = 'fitted')  

theme_set(theme_bw())

all_S <-
ggplot() +
  geom_point(data = alpha_summary,
             aes(x = alt_enforcement, y = S, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = S_protectMarginal_all$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = S_protectMarginal_all$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2', breaks = c(10, 16, 22)) +
  labs(x = '',
       y = 'Species richness (S)',
        tag = '(c)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none', 
        # axis.text = element_text(size = 16),
        # axis.title = element_text(size = 18),
        plot.tag.position = c(0.2, 0.8))

all_Sn <- ggplot() +
  geom_point(data = alpha_summary,
             aes(x = alt_enforcement, y = Sn, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = Sn_protectMarginal_all$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = Sn_protectMarginal_all$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = expression(S[n]),
       tag = '(b)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.2, 0.8))


all_S_PIE <- ggplot() +
  geom_point(data = alpha_summary,
             aes(x = alt_enforcement, y = S_PIE, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = S_PIE_protectMarginal_all$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = S_PIE_protectMarginal_all$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = expression(S[PIE]),
       subtitle = 'All fishes combined',
       tag = '(a)'
       ) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        # plot.subtitle = element_text(size = 20),
        # axis.text = element_text(size = 16),
        # axis.title = element_text(size = 18),
        plot.tag.position = c(0.2, 0.8))

# N results to supplement
all_N <- ggplot() +
  geom_point(data = alpha_summary,
             aes(x = alt_enforcement, y = N, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = N_protectMarginal_all$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = N_protectMarginal_all$alt_enforcement,
                aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = '',# y = expression(paste('Total number of individuals (', italic(N), ' per 375', m^2,')')),
       subtitle = 'All fishes combined',
       tag = '(a)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.3, 0.8))


S_protectMarginal_high <- marginal_effects(S_alpha_mixed_high, effects = 'alt_enforcement')  
Sn_protectMarginal_high <- marginal_effects(Sn_alpha_mixed_high, effects = 'alt_enforcement')  
S_PIE_protectMarginal_high <- marginal_effects(S_PIE_alpha_mixed_high, effects = 'alt_enforcement')  
N_protectMarginal_high <- marginal_effects(N_alpha_mixed_high, effects = 'alt_enforcement')  
# S_PIE_high_h0 <- hypothesis(S_PIE_alpha_mixed_high, '(Intercept + alt_enforcementprotected) - Intercept > 0')
# plot(S_PIE_h0)

high_S <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='high'),
             aes(x = alt_enforcement, y = S, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = S_protectMarginal_high$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = S_protectMarginal_high$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = 'Species richness (S)',
       tag = '(f)'
       ) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        # axis.text = element_text(size = 16),
        # axis.title = element_text(size = 18),
        plot.tag.position = c(0.2, 0.8))

high_Sn <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='high'),
             aes(x = alt_enforcement, y = Sn, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = Sn_protectMarginal_high$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = Sn_protectMarginal_high$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width= 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = expression(S[n]),
       subtitle = '',
       tag = '(e)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.2, 0.8))


high_S_PIE <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='high'),
             aes(x = alt_enforcement, y = ENSPIE, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = S_PIE_protectMarginal_high$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = S_PIE_protectMarginal_high$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = expression(S[PIE]),
       subtitle = 'Fishes with high sensitivity\nto exploitation',
       tag = '(d)'
       ) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        # plot.subtitle = element_text(size = 20),
        # axis.text = element_text(size = 16),
        # axis.title = element_text(size = 18),
        plot.tag.position = c(0.2, 0.8))

# N results to supplement
high_N <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='high'),
             aes(x = alt_enforcement, y = N, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = N_protectMarginal_high$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = N_protectMarginal_high$alt_enforcement,
                aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = expression(paste('Total number of individuals (', italic(N), ' per 375', m^2,')')),
       subtitle = 'Fishes with high sensitivity\nto exploitation',
       tag = '(b)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.3, 0.8))


# low sensitivity
S_protectMarginal_low <- marginal_effects(S_alpha_mixed_low, effects = 'alt_enforcement')  
Sn_protectMarginal_low <- marginal_effects(Sn_alpha_mixed_low, effects = 'alt_enforcement')  
S_PIE_protectMarginal_low <- marginal_effects(S_PIE_alpha_mixed_low, effects = 'alt_enforcement')  
N_protectMarginal_low <- marginal_effects(N_alpha_mixed_low, effects = 'alt_enforcement')  

low_S <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='low'),
             aes(x = alt_enforcement, y = S, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = S_protectMarginal_low$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = S_protectMarginal_low$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = 'Species richness (S)',
       tag = '(i)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.2, 0.8))

low_Sn <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='low'),
             aes(x = alt_enforcement, y = Sn, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = Sn_protectMarginal_low$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = Sn_protectMarginal_low$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = expression(S[n]),
       tag = '(h)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.2, 0.8))


low_S_PIE <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='low'),
             aes(x = alt_enforcement, y = ENSPIE, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = S_PIE_protectMarginal_low$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = S_PIE_protectMarginal_low$alt_enforcement,
                 aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                 size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = expression(S[PIE]),
       subtitle = 'Fishes with low sensitivity\nto exploitation',
       tag = '(g)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.2, 0.8))

# N results to supplement
low_N <- ggplot() +
  geom_point(data = alpha_summary_sensitivity %>% filter(v70_30=='low'),
             aes(x = alt_enforcement, y = N, colour = alt_enforcement), 
             size = 1, alpha = 0.3, position = position_jitter(width = 0.05)) +
  geom_point(data = N_protectMarginal_low$alt_enforcement,
             aes(x = alt_enforcement, y = estimate__, colour = alt_enforcement), size = 4) +
  geom_errorbar(data = N_protectMarginal_low$alt_enforcement,
                aes(x = alt_enforcement, ymin = lower__, ymax = upper__, colour = alt_enforcement),
                size = 1.3, width = 0.1) +
  scale_colour_grey() +
  scale_y_continuous(trans = 'log2') +
  labs(x = '',
       y = '',# y = expression(paste('Total number of individuals (', italic(N), ' per 375', m^2,')')),
       subtitle = 'Fishes with low sensitivity\nto exploitation',
       tag = '(c)') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.tag.position = c(0.3, 0.8))

p1 <- cowplot::plot_grid(all_S_PIE, all_Sn, all_S, 
                         high_S_PIE, high_Sn, high_S, 
                         low_S_PIE, low_Sn, low_S, 
                   align = 'hv', 
                   ncol = 3)

p2 <- cowplot::add_sub(p1, 'Protection')
alpha_title <- cowplot::ggdraw() + cowplot::draw_label(expression(paste(italic(alpha), '-scale', sep = '')))
cowplot::plot_grid(alpha_title, p2, ncol = 1, rel_heights = c(0.025,1))
# ggsave('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision2/figs/Fig2_title.png',
#        width = 250, height = 250, units = 'mm')

# supplement N
pN <- cowplot::plot_grid(all_N,
                         high_N,
                         low_N,
                         align = 'hv', 
                         nrow = 3)

pN2 <- cowplot::add_sub(pN, 'Protection')
cowplot::plot_grid(alpha_title, pN2, ncol = 1, rel_heights = c(0.025,1))

# ggsave('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/figs/FigS3.pdf',
#        width = 250/3, height = 250, units = 'mm')


# all else equal, what is the probability that MPAs have more species?
ndat_all <-  expand.grid(
  alt_enforcement = c('fished', 'protected'),
  sstmean = mean(alpha_summary$sstmean),
  chlomean = mean(alpha_summary$chlomean),
  nitrate = mean(alpha_summary$nitrate),
  parmean = mean(alpha_summary$parmean),
  ph = mean(alpha_summary$ph),
  phos = mean(alpha_summary$phos),
  salinity = mean(alpha_summary$salinity),
  impacts_at_coords = mean(alpha_summary$impacts_at_coords),
  rug_mu = mean(alpha_summary$rug_mu)
)

ndat_high <-  expand.grid(
  alt_enforcement = c('fished', 'protected'),
  sstmean = mean(alpha_summary_sensitivity$sstmean[which(alpha_summary_sensitivity$v70_30=='high')]),
  chlomean = mean(alpha_summary_sensitivity$chlomean[which(alpha_summary_sensitivity$v70_30=='high')]),
  nitrate = mean(alpha_summary_sensitivity$nitrate[which(alpha_summary_sensitivity$v70_30=='high')]),
  parmean = mean(alpha_summary_sensitivity$parmean[which(alpha_summary_sensitivity$v70_30=='high')]),
  ph = mean(alpha_summary_sensitivity$ph[which(alpha_summary_sensitivity$v70_30=='high')]),
  phos = mean(alpha_summary_sensitivity$phos[which(alpha_summary_sensitivity$v70_30=='high')]),
  salinity = mean(alpha_summary_sensitivity$salinity[which(alpha_summary_sensitivity$v70_30=='high')]),
  impacts_at_coords = mean(alpha_summary_sensitivity$impacts_at_coords[which(alpha_summary_sensitivity$v70_30=='high')]),
  rug_mu = mean(alpha_summary_sensitivity$rug_mu[which(alpha_summary_sensitivity$v70_30=='high')])
)

ndat_low <-  expand.grid(
  alt_enforcement = c('fished', 'protected'),
  sstmean = mean(alpha_summary_sensitivity$sstmean[which(alpha_summary_sensitivity$v70_30=='low')]),
  chlomean = mean(alpha_summary_sensitivity$chlomean[which(alpha_summary_sensitivity$v70_30=='low')]),
  nitrate = mean(alpha_summary_sensitivity$nitrate[which(alpha_summary_sensitivity$v70_30=='low')]),
  parmean = mean(alpha_summary_sensitivity$parmean[which(alpha_summary_sensitivity$v70_30=='low')]),
  ph = mean(alpha_summary_sensitivity$ph[which(alpha_summary_sensitivity$v70_30=='low')]),
  phos = mean(alpha_summary_sensitivity$phos[which(alpha_summary_sensitivity$v70_30=='low')]),
  salinity = mean(alpha_summary_sensitivity$salinity[which(alpha_summary_sensitivity$v70_30=='low')]),
  impacts_at_coords = mean(alpha_summary_sensitivity$impacts_at_coords[which(alpha_summary_sensitivity$v70_30=='low')]),
  rug_mu = mean(alpha_summary_sensitivity$rug_mu[which(alpha_summary_sensitivity$v70_30=='low')])
)

fS <- as.data.frame(fitted(S_alpha_mixed, newdata = ndat_all, summary = F, re_formula = NA))
names(fS) <- c('fished', 'protected')
S_h0 <- hypothesis(fS, 'protected - fished > 0')
S_h0$hypothesis
S_h0$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fSn <- as.data.frame(fitted(Sn_alpha_mixed, newdata = ndat_all, summary = F, re_formula = NA))
names(fSn) <- c('fished', 'protected')
Sn_h0 <- hypothesis(fSn, 'protected - fished > 0')
Sn_h0$hypothesis
Sn_h0$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fS_PIE <- as.data.frame(fitted(S_PIE_alpha_mixed, newdata = ndat_all, summary = F, re_formula = NA))
names(fS_PIE) <- c('fished', 'protected')
Spie_h0 <- hypothesis(fS_PIE, 'protected - fished > 0')
Spie_h0$hypothesis
Spie_h0$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

# high sensitivity fishes
fN_high <- as.data.frame(fitted(N_alpha_mixed_high, newdata = ndat_high, summary = F, re_formula = NA))
names(fN_high) <- c('fished', 'protected')
N_h0_high <- hypothesis(fN_high, 'protected - fished > 0')
N_h0_high$hypothesis
N_h0_high$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fS_high <- as.data.frame(fitted(S_alpha_mixed_high, newdata = ndat_high, summary = F, re_formula = NA))
names(fS_high) <- c('fished', 'protected')
S_h0_high <- hypothesis(fS_high, 'protected - fished > 0')
S_h0_high$hypothesis
S_h0_high$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fSn_high <- as.data.frame(fitted(Sn_alpha_mixed_high, newdata = ndat_high, summary = F, re_formula = NA))
names(fSn_high) <- c('fished', 'protected')
Sn_h0_high <- hypothesis(fSn_high, 'protected - fished > 0')
Sn_h0_high$hypothesis
Sn_h0_high$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fS_PIE_high <- as.data.frame(fitted(S_PIE_alpha_mixed_high, newdata = ndat_high, summary = F, re_formula = NA))
names(fS_PIE_high) <- c('fished', 'protected')
Spie_h0_high <- hypothesis(fS_PIE_high, 'protected - fished > 0')
Spie_h0_high$hypothesis
Spie_h0_high$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

# low sensitivity fishes
fN_low <- as.data.frame(fitted(N_alpha_mixed_low, newdata = ndat_low, summary = F, re_formula = NA))
names(fN_low) <- c('fished', 'protected')
N_h0_low <- hypothesis(fN_low, 'protected - fished > 0')
N_h0_low$hypothesis
N_h0_low$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fS_low <- as.data.frame(fitted(S_alpha_mixed_low, newdata = ndat_low, summary = F, re_formula = NA))
names(fS_low) <- c('fished', 'protected')
S_h0_low <- hypothesis(fS_low, 'protected - fished > 0')
S_h0_low$hypothesis
S_h0_low$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fSn_low <- as.data.frame(fitted(Sn_alpha_mixed_low, newdata = ndat_low, summary = F, re_formula = NA))
names(fSn_low) <- c('fished', 'protected')
Sn_h0_low <- hypothesis(fSn_low, 'protected - fished > 0')
Sn_h0_low$hypothesis
Sn_h0_low$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

fS_PIE_low <- as.data.frame(fitted(S_PIE_alpha_mixed_low, newdata = ndat_low, summary = F, re_formula = NA))
names(fS_PIE_low) <- c('fished', 'protected')
Spie_h0_low <- hypothesis(fS_PIE_low, 'protected - fished > 0')
Spie_h0_low$hypothesis
Spie_h0_low$samples %>% .$H1 %>% quantile(c(0.05, 0.95))

