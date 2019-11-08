MediterraneanMPA
================
Shane Blowes

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->
The MediterraneanMPA repository includes R code and data to reproduce the analyses shown in the article:

**Mediterranean marine protected areas have higher biodiversity via increased evenness, not abundance**

*by Shane A. Blowes, Jonathan M. Chase, Antonio Di Franco, Ori Frid, Nicholas J. Gotelli, Paolo Guidetti, Tiffany M. Knight, Felix May, Daniel J. McGlinn, Fiorenza Micheli, Enric Sala, Jonathan Belmaker*

Here we give a brief overview on the code and data files in this repository

Data
----

Files in the data folder include:

**mediterranean\_mpa.csv**: This file contains the species abundance data and meta data for the fish samples

**mpa\_fishbase\_traits.Rdata**: This file contains the trait data (from FishBase) used in the main text analyses

**Micheli\_impacts\_model.tif**: Impacts layer from Micheli et al 2013 *PLoS ONE* **8** e79889. Downloaded from: <https://www.nceas.ucsb.edu/globalmarine/mediterranean> on 7th Jan. 2019

**chlorophyll**: this folder has the chlorophyll data from Bio-ORACLE (Tyberghein et al. 2011 *GEB* **21**: 272-281) used in the analyses

**SST**: this folder has sea surface temperature data from Bio-ORACLE (Tyberghein et al. 2011 *GEB* **21**: 272-281) used in the analyses

**other**: other environmental data from Bio-ORACLE (Tyberghein et al. 2011 *GEB* **21**: 272-281) used in the analyses

**mobr\_multiscale\_sensitivity.Rdata**: results of multi-scale mobr (McGlinn et al. 2019 *MEE* **10**:258-269; <https://github.com/MoBiodiv/mobr>) analyses (scripts 08a and 08b below)

R scripts
---------

Many scripts have two versions: (a) two protection classes as presented in the main text, (b) three protection classes (shown only in the supplement)

01\_: scripts to prepare the fish and trait data for analyses

02\_: scripts to load environmental data and test whether it differs systematically between the treatments (fished and protected areas)

03\_: scripts to calculate the alpha-scale metrics

04\_: fit hierarchical models to alpha-scale data

05\_: plot alpha-scale results

06\_: beta- and gamma-scale calculations for all fish combined

07\_: beta- and gamma-scale calculations for fish with high and low sensitivity to exploitation

08\_: multiscale mobr (McGlinn et al. 2019 *MEE* **10**:258-269; <https://github.com/MoBiodiv/mobr>) analyses for: (a) main text, (b) supplement

mobr\_stack\_: modified functions from mobr function `overlap_effects()` for plotting results of multiscale analyses
