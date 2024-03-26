# CCBER-Vegetation-Monitoring
Cleaning and creating figures from the Survey123 vegetation monitoring data.

### Quadratcleaning.r
Survey 123 data comes in 5 seperate files. General, natives, nonnatives, other, and unlisted species. With VP cleaning we categorize each sheet and merge into one mastersheet based on the 'parent code'

### YearlyreportfiguresNCOS.r
Using the previously cleaned Masterdataset we filter for NCOS site only and make figures for native and nonnative relative and absolute cover and relative of all cover.

### InternalVegetationReportNCOS.r
Using the previously cleaned Masterdataset we filter for NCOS and make boxplots with jitter to show if there are outliers at any sites.
