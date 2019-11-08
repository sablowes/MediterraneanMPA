##==============================================================================================================
##	Journal of Applied Ecology 'Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance'
##  code: Shane Blowes
##==============================================================================================================
# code to produce plot of the results of the mobr multiscale analyses for assessing the sensivity to the 
# threshold (percentile) used to determined high and low sensivity to exploitation


rm(list=ls())
# data and libraries ------------------------------------------------------
load('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision3_to_press/data_results/mobr_multiscale_sensitivity.Rdata')

library(mobr)
library(tidyverse)
library(iNEXT)
library(cowplot)

##--vulnerability to changes in the percentiles used to designate vulnerability-------
##	all species combined
alt_only_mobr_df <- filter(alt_enfcmt_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_only_mobr_df2 <- filter(alt_enfcmt_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_only_mobr_df3 <- filter(alt_enfcmt_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_only_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_only_mobr_df <- bind_rows(alt_only_mobr_df, alt_only_mobr_df2, alt_only_mobr_df3) %>%
  mutate(vulnerability='All fishes combined')

##	low v	
alt_enfcmt_v70_30_low_mobr_df <- filter(alt_enfcmt_v70_30_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v70_30_low_mobr_df2 <- filter(alt_enfcmt_v70_30_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v70_30_low_mobr_df3 <- filter(alt_enfcmt_v70_30_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v70_30_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v70_30_low_mobr_df <- bind_rows(alt_enfcmt_v70_30_low_mobr_df, alt_enfcmt_v70_30_low_mobr_df2, alt_enfcmt_v70_30_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity')

##	high v
alt_enfcmt_v70_30_high_mobr_df <- filter(alt_enfcmt_v70_30_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v70_30_high_mobr_df2 <- filter(alt_enfcmt_v70_30_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v70_30_high_mobr_df3 <- filter(alt_enfcmt_v70_30_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v70_30_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v70_30_high_mobr_df <- bind_rows(alt_enfcmt_v70_30_high_mobr_df, alt_enfcmt_v70_30_high_mobr_df2, alt_enfcmt_v70_30_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity')

##	put them all together	
alt_enfcmt_v70_30 <- bind_rows(alt_only_mobr_df , alt_enfcmt_v70_30_low_mobr_df, alt_enfcmt_v70_30_high_mobr_df)	
alt_enfcmt_v70_30$ddelta_effect <- factor(alt_enfcmt_v70_30$ddelta_effect, 
                                          levels=c('Change in S due to N', 'Change in S due to the SAD', 'Change in S due to spatial aggregation'))
alt_enfcmt_v70_30$vulnerability <- factor(alt_enfcmt_v70_30$vulnerability, 
                                          levels=c('All fishes combined', 'High sensitivity', 'Low sensitivity'))

##	low v	
alt_enfcmt_v10_90_low_mobr_df <- filter(alt_enfcmt_v10_90_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v10_90_low_mobr_df2 <- filter(alt_enfcmt_v10_90_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v10_90_low_mobr_df3 <- filter(alt_enfcmt_v10_90_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v10_90_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v10_90_low_mobr_df <- bind_rows(alt_enfcmt_v10_90_low_mobr_df, alt_enfcmt_v10_90_low_mobr_df2, alt_enfcmt_v10_90_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 10)

##
alt_enfcmt_v20_80_low_mobr_df <- filter(alt_enfcmt_v20_80_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v20_80_low_mobr_df2 <- filter(alt_enfcmt_v20_80_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v20_80_low_mobr_df3 <- filter(alt_enfcmt_v20_80_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v20_80_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v20_80_low_mobr_df <- bind_rows(alt_enfcmt_v20_80_low_mobr_df, alt_enfcmt_v20_80_low_mobr_df2, alt_enfcmt_v20_80_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 20)

##	
alt_enfcmt_v30_70_low_mobr_df <- filter(alt_enfcmt_v30_70_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v30_70_low_mobr_df2 <- filter(alt_enfcmt_v30_70_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v30_70_low_mobr_df3 <- filter(alt_enfcmt_v30_70_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v30_70_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v30_70_low_mobr_df <- bind_rows(alt_enfcmt_v30_70_low_mobr_df, alt_enfcmt_v30_70_low_mobr_df2, alt_enfcmt_v30_70_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 30)

##	
alt_enfcmt_v40_60_low_mobr_df <- filter(alt_enfcmt_v40_60_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v40_60_low_mobr_df2 <- filter(alt_enfcmt_v40_60_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v40_60_low_mobr_df3 <- filter(alt_enfcmt_v40_60_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v40_60_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v40_60_low_mobr_df <- bind_rows(alt_enfcmt_v40_60_low_mobr_df, alt_enfcmt_v40_60_low_mobr_df2, alt_enfcmt_v40_60_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 40)

##	
alt_enfcmt_v50_50_low_mobr_df <- filter(alt_enfcmt_v50_50_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v50_50_low_mobr_df2 <- filter(alt_enfcmt_v50_50_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v50_50_low_mobr_df3 <- filter(alt_enfcmt_v50_50_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v50_50_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v50_50_low_mobr_df <- bind_rows(alt_enfcmt_v50_50_low_mobr_df, alt_enfcmt_v50_50_low_mobr_df2, alt_enfcmt_v50_50_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 50)

##	
alt_enfcmt_v60_40_low_mobr_df <- filter(alt_enfcmt_v60_40_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v60_40_low_mobr_df2 <- filter(alt_enfcmt_v60_40_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v60_40_low_mobr_df3 <- filter(alt_enfcmt_v60_40_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v60_40_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v60_40_low_mobr_df <- bind_rows(alt_enfcmt_v60_40_low_mobr_df, alt_enfcmt_v60_40_low_mobr_df2, alt_enfcmt_v60_40_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 60)

##	alt_enfcmt_v70_30_low_mobr_df already exists
alt_enfcmt_v70_30_low_mobr_df <- alt_enfcmt_v70_30_low_mobr_df %>% mutate(Percentile = 70)
##	
alt_enfcmt_v80_20_low_mobr_df <- filter(alt_enfcmt_v80_20_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v80_20_low_mobr_df2 <- filter(alt_enfcmt_v80_20_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v80_20_low_mobr_df3 <- filter(alt_enfcmt_v80_20_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v80_20_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v80_20_low_mobr_df <- bind_rows(alt_enfcmt_v80_20_low_mobr_df, alt_enfcmt_v80_20_low_mobr_df2, alt_enfcmt_v80_20_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 80)

##
alt_enfcmt_v90_10_low_mobr_df <- filter(alt_enfcmt_v90_10_low_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v90_10_low_mobr_df2 <- filter(alt_enfcmt_v90_10_low_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N') %>% 
  # remove strange spike at end
  filter(ddeltaS_emp < 1)
alt_enfcmt_v90_10_low_mobr_df3 <- filter(alt_enfcmt_v90_10_low_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v90_10_low_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v90_10_low_mobr_df <- bind_rows(alt_enfcmt_v90_10_low_mobr_df, alt_enfcmt_v90_10_low_mobr_df2, alt_enfcmt_v90_10_low_mobr_df3) %>%
  mutate(vulnerability='Low sensitivity',
         Percentile = 90)


##	high v	
alt_enfcmt_v10_90_high_mobr_df <- filter(alt_enfcmt_v10_90_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v10_90_high_mobr_df2 <- filter(alt_enfcmt_v10_90_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v10_90_high_mobr_df3 <- filter(alt_enfcmt_v10_90_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v10_90_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v10_90_high_mobr_df <- bind_rows(alt_enfcmt_v10_90_high_mobr_df, alt_enfcmt_v10_90_high_mobr_df2, alt_enfcmt_v10_90_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 90)

##
alt_enfcmt_v20_80_high_mobr_df <- filter(alt_enfcmt_v20_80_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v20_80_high_mobr_df2 <- filter(alt_enfcmt_v20_80_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v20_80_high_mobr_df3 <- filter(alt_enfcmt_v20_80_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v20_80_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v20_80_high_mobr_df <- bind_rows(alt_enfcmt_v20_80_high_mobr_df, alt_enfcmt_v20_80_high_mobr_df2, alt_enfcmt_v20_80_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 80)

##	
alt_enfcmt_v30_70_high_mobr_df <- filter(alt_enfcmt_v30_70_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v30_70_high_mobr_df2 <- filter(alt_enfcmt_v30_70_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v30_70_high_mobr_df3 <- filter(alt_enfcmt_v30_70_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v30_70_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v30_70_high_mobr_df <- bind_rows(alt_enfcmt_v30_70_high_mobr_df, alt_enfcmt_v30_70_high_mobr_df2, alt_enfcmt_v30_70_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 70)

##	
alt_enfcmt_v40_60_high_mobr_df <- filter(alt_enfcmt_v40_60_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v40_60_high_mobr_df2 <- filter(alt_enfcmt_v40_60_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v40_60_high_mobr_df3 <- filter(alt_enfcmt_v40_60_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v40_60_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v40_60_high_mobr_df <- bind_rows(alt_enfcmt_v40_60_high_mobr_df, alt_enfcmt_v40_60_high_mobr_df2, alt_enfcmt_v40_60_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 60)

##	
alt_enfcmt_v50_50_high_mobr_df <- filter(alt_enfcmt_v50_50_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v50_50_high_mobr_df2 <- filter(alt_enfcmt_v50_50_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v50_50_high_mobr_df3 <- filter(alt_enfcmt_v50_50_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v50_50_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v50_50_high_mobr_df <- bind_rows(alt_enfcmt_v50_50_high_mobr_df, alt_enfcmt_v50_50_high_mobr_df2, alt_enfcmt_v50_50_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 50)

##	
alt_enfcmt_v60_40_high_mobr_df <- filter(alt_enfcmt_v60_40_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v60_40_high_mobr_df2 <- filter(alt_enfcmt_v60_40_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v60_40_high_mobr_df3 <- filter(alt_enfcmt_v60_40_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v60_40_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v60_40_high_mobr_df <- bind_rows(alt_enfcmt_v60_40_high_mobr_df, alt_enfcmt_v60_40_high_mobr_df2, alt_enfcmt_v60_40_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 40)

##	alt_enfcmt_v70_30_high_mobr_df already exists
alt_enfcmt_v70_30_high_mobr_df <- alt_enfcmt_v70_30_high_mobr_df  %>% mutate(Percentile = 30)
##	
alt_enfcmt_v80_20_high_mobr_df <- filter(alt_enfcmt_v80_20_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v80_20_high_mobr_df2 <- filter(alt_enfcmt_v80_20_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v80_20_high_mobr_df3 <- filter(alt_enfcmt_v80_20_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v80_20_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v80_20_high_mobr_df <- bind_rows(alt_enfcmt_v80_20_high_mobr_df, alt_enfcmt_v80_20_high_mobr_df2, alt_enfcmt_v80_20_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 20)

##
alt_enfcmt_v90_10_high_mobr_df <- filter(alt_enfcmt_v90_10_high_mobrTest$SAD, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to the SAD')
alt_enfcmt_v90_10_high_mobr_df2 <- filter(alt_enfcmt_v90_10_high_mobrTest$N, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to N')
alt_enfcmt_v90_10_high_mobr_df3 <- filter(alt_enfcmt_v90_10_high_mobrTest$agg, group=='protected') %>% 
  mutate(ddelta_effect = 'Change in S due to spatial aggregation')

names(alt_enfcmt_v90_10_high_mobr_df)[2:6] <- c('effort_sample', 'ddeltaS_emp', 'ddeltaS_null_low', 'ddeltaS_null_median', 'ddeltaS_null_high')

alt_enfcmt_v90_10_high_mobr_df <- bind_rows(alt_enfcmt_v90_10_high_mobr_df, alt_enfcmt_v90_10_high_mobr_df2, alt_enfcmt_v90_10_high_mobr_df3) %>%
  mutate(vulnerability='High sensitivity',
         Percentile = 10)	

##	put them all together	
alt_enfcmt_allPercentiles <- bind_rows(alt_only_mobr_df, alt_enfcmt_v10_90_low_mobr_df, alt_enfcmt_v20_80_low_mobr_df, alt_enfcmt_v30_70_low_mobr_df, 
                                       alt_enfcmt_v40_60_low_mobr_df, alt_enfcmt_v50_50_low_mobr_df, alt_enfcmt_v60_40_low_mobr_df, 
                                       alt_enfcmt_v70_30_low_mobr_df, alt_enfcmt_v80_20_low_mobr_df,
                                       alt_enfcmt_v90_10_low_mobr_df, alt_enfcmt_v10_90_high_mobr_df, alt_enfcmt_v20_80_high_mobr_df, 
                                       alt_enfcmt_v30_70_high_mobr_df, alt_enfcmt_v40_60_high_mobr_df, alt_enfcmt_v50_50_high_mobr_df, 
                                       alt_enfcmt_v60_40_high_mobr_df, alt_enfcmt_v70_30_high_mobr_df, alt_enfcmt_v80_20_high_mobr_df,
                                       alt_enfcmt_v90_10_high_mobr_df)	

alt_enfcmt_allPercentiles$ddelta_effect <- factor(alt_enfcmt_allPercentiles$ddelta_effect, 
                                                  levels=c('Change in S due to N', 'Change in S due to the SAD', 'Change in S due to spatial aggregation'))
alt_enfcmt_allPercentiles$vulnerability <- factor(alt_enfcmt_allPercentiles$vulnerability, 
                                                  levels=c('All fishes combined', 'High sensitivity', 'Low sensitivity'))

ddeltaS_plot_allPercentiles <- ggplot() +
  facet_wrap(vulnerability~ddelta_effect, scales='free') +
  geom_ribbon(data = filter(alt_enfcmt_allPercentiles, vulnerability!='All fishes combined'), 
              aes(x= effort_sample, ymax= ddeltaS_null_high, ymin= ddeltaS_null_low, fill=as.factor(Percentile), group=Percentile), 
              alpha=0.1) +
  geom_line(data = filter(alt_enfcmt_allPercentiles, vulnerability!='All fishes combined'), 
            aes(x=effort_sample, y=ddeltaS_emp, colour=as.factor(Percentile), group=Percentile), lwd=1.2) +
  #	geom_ribbon(data = filter(alt_enfcmt_allPercentiles, vulnerability=='All fishes combined'), 
  #		aes(x= effort_sample, ymax= ddeltaS_null_high, ymin= ddeltaS_null_low, linetype=NA), alpha=0.1) +
  #	geom_line(data = filter(alt_enfcmt_allPercentiles, vulnerability=='All fishes combined'),
  #		aes(x=effort_sample, y=ddeltaS_emp), lwd=1.25) +
  ylab('Number of species') +
  xlab('Sample size') +
  scale_y_continuous(minor_breaks=seq(-5, 10, by=1)) +
  scale_colour_brewer(name='Percentage of vulnerability scores,\ni.e., species, included in vulnerability class', palette='PuBuGn') +
  scale_fill_brewer(guide=FALSE, palette='PuBuGn') +
  #	scale_colour_grey(name='Percentage of vulnerability scores,\ni.e., species, included in vulnerability class') +
  #	scale_fill_grey(guide=FALSE) +
  theme_bw()+
  theme(strip.text=element_text(size=12), legend.position='top', legend.direction='horizontal',
        axis.text = element_text(size = 12), axis.title = element_text(size = 14))

# ggsave('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/figs/FigS7_sensitivity_sensitivity.pdf', 
#        width = 290, height = 200, units = 'mm')
# ggsave('~/Dropbox/1current/mediterranean_mpa/submitted/JApplEcol/revision1/figs/FigS7_sensitivity_sensitivity.png', 
#        width = 290, height = 200, units = 'mm')
