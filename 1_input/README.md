## Data sources

- Population Germany: 
  - DESTATIS 2019-01-23, https://www-genesis.destatis.de/genesis/online Table 12411 
  - NRW 2019-06-27, https://www.landesdatenbank.nrw.de, Table 12411-10ir
- Population United States:
  - SEER 2020-11-03, https://seer.cancer.gov/popdata.thru.2017/yr1969_2017.singleages/us.1969_2017.singleages.adjusted.exe (modified 2018-12-21; not available anymore); county-level population files single-year age groups, 1969-2017, all states combined adjusted 
- Incidence and reference rates for Germany: 
  - ZfKD (not published, for access see below)
- Incidence and reference rates for the United States: 
  - SEER (not published, for access see below)
  
### Used data sets and preparation scripts

1.  **wide_spc_methods** *(Individual level cancer data (ZfKD + SEER data) in wide format):*

    -   `1_input/81.spn.dataset.methods.wide.RData` (last modified: `r file.info(analysis_file_wide)$mtime`)

    -   dependent on scripts:

        -   01.cr_read.seer.dataset.R

        -   03.cr_dm.seer.dataset.R

        -   04.cr_save.seer.analysis.dataset.R

        -   11.cr_read.zfkd.dataset.R

        -   13.cr_dm.zfkd.dataset.R

        -   14.cr_save.zfkd.analysis.dataset.R

        -   81.01.cr_prefilter.methods.zfkd.R

        -   81.02.cr_prefilter.methods.seer.R

        -   81.03.cr_dm.save.methods.R

2.  **wide_spc_methods_iarc** *(Individual level cancer data (ZfKD + SEER data) in wide format, only counting international primaries):*

    -   `1_input/83.spn.dataset.methods.iarc.wide.RData` (last modified: `r file.info(analysis_file_wide2)$mtime`)

    -   dependent on scripts:

        -   01.cr_read.seer.dataset.R

        -   03.cr_dm.seer.dataset.R

        -   04.cr_save.seer.analysis.dataset.R

        -   11.cr_read.zfkd.dataset.R

        -   13.cr_dm.zfkd.dataset.R

        -   14.cr_save.zfkd.analysis.dataset.R

        -   81.11.cr_prefilter.methods.iarc.zfkd.R

        -   81.12.cr_prefilter.methods.iarc.seer.R

        -   81.13.cr_dm.save.methods.R

3.  **refrates_lungcancer_dco_calc** *(File with reference incidence rates for lung cancer, including DCO cases, calculated from registry data)*

    -   `1_input/58.refrates.methods.lungcancer.dco.calculated.RData` (last modified: `r file.info("1_input/58.refrates.methods.lungcancer.dco.calculated.RData")$mtime`

    -   dependent on scripts:

        -   57.cr_read.refrates.us.dco.R

        -   83.05.cr_refrates.from.cohort.zfkd.dco.R

        -   83.07.cr_refrates.merge.methods.R

4.  **refrates_methods_lcsubtype_histgroupiarc_dco** *(File with reference incidence rates for subtypes of lung cancer, based on t_histgroupiarc, including DCO cases, calculated from registry data)*

    -   `1_input/82.02.rates.lc.subtype.histgroupiarc.methods.dco.RData` (last modified: `r file.info(rates_file2)$mtime`)

    -   dependent on scripts:

        -   82.21.cr_refrates.by.lcsubtype.histgroupiarc.seer.R

        -   82.22.cr_refrates.by.lcsubtype.histgroupiarc.zfkd.R

        -   82.23.cr_refrates.by.lcsubtype.histgroupiarc.methods.R
        
5.  **refrates_methods_lcsubtype_histgroupiarc_iarc_dco** *(File with reference incidence rates for subtypes of lung cancer, based on t_histgroupiarc, including DCO cases, calculated from registry data; only counting cases that fulfill IARC/IACR MP Rules)*

    -   `1_input/82.03.rates.lc.subtype.histgroupiarc.methods.iarc.dco.RData` (last modified: `r file.info(rates_file3)$mtime`)

    -   dependent on scripts:

        -   82.31.cr_refrates.by.lcsubtype.histgroupiarc.iarc.seer.R

        -   82.22.cr_refrates.by.lcsubtype.zfkd.R

        -   82.33.cr_refrates.by.lcsubtype.histgroupiarc.methods.iarc.R


## Raw data access

Due to legal restrictions, the individual level raw data used for this analysis is only available via request to the German Center for Cancer Registry Data (ZfKD) that can provide a scientific use file.
More information on the application process is provided on the ZfKD website (https://www.krebsdaten.de/Krebs/EN/Content/ScientificUseFile/scientificusefile_node.html).

The validation data set (i.e., U.S. cancer registry data) is publicly available via the Surveillance, Epidemiology, and End Results Program (SEER). More information on the data request process is provided on the SEER website (https://seer.cancer.gov/data/access.html).
