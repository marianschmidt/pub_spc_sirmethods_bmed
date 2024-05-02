# Code for Histology-specific SIR paper

This repository includes the analysis code for paper 'Histology-specific standardized incidence ratio improves the estimation of second primary lung cancer risk' submitted to BMC Medicine.


## Publication

The final publication can be accessed via the publisher’s website https://doi.org/10.1186/s12916-024-03398-9.


## Structure of the repository

`1_input`: Folder containing input, i.e. data files. This folder is currently empty due to raw data access restrictions (see below).

`2_scripts`: Folder containing analysis scripts. Due to missing input files, the scripts will currently not run, but you are welcome to check the code for errors or inconsistencies.

`3_output`: Folder containing all output files. This includes all figures and tables.

`4_manuscript`: Folder containing the source file for the manuscript. Availability may depend on the journal's embargo policy.


## Package version control

This repository uses `renv` to ensure that a project-specific R package library is used and that functionality is maintained even if packages have breaking changes in the future.

If you clone this package, make sure you create the same package library by using the command `renv::restore()` to restore the project library locally to your machine.
You find more details on https://rstudio.github.io/renv/articles/collaborating.html.


## Raw data access

Due to legal restrictions, the individual-level raw data used for this analysis (i.e., German cancer registry data) is only available via request to the German Centre for Cancer Registry Data (ZfKD), which can provide a scientific use file. More information on the application process is provided on the ZfKD website (https://www.krebsdaten.de/Krebs/EN/Content/ScientificUseFile/scientificusefile_node.html).

The validation data set (i.e., U.S. cancer registry data) is publicly available via the Surveillance, Epidemiology, and End Results Program (SEER). More information on the data request process is provided on the SEER website (https://seer.cancer.gov/data/access.html).

The authors of this paper strongly support open science and have therefore published both the analysis code and the newly programmed software to review and download under a GPL-3 license. The respective references are provided in the manuscript.

Repository for analysis code: https://github.com/marianschmidt/pub_spc_sirmethods_bmed

Repository for software code: https://doi.org/10.5281/zenodo.5055870


## Code archive

This repository is available via a permanent identifier.
[![DOI](https://zenodo.org/badge/765910283.svg)](https://zenodo.org/doi/10.5281/zenodo.11068786)
