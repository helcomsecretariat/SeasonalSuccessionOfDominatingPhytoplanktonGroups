# SeasonalSuccessionOfDominatingPhytoplanktonGroups
#### Repository for R scripts used for calculating HELCOM Seasonal succession of dominating phytoplankton groups indicator
The HELCOM indicator Seasonal succession of dominating phytoplankton groups is used as part of the status assessment of pelagic habitats in the HELCOM Holistic Assessment of the Baltic Sea. The indicator report with information on indicator concept and results can be found on the HELCOM website ([link](https://helcom.fi/baltic-sea-trends/indicators/)).
## Scripts
This repository includes two scripts for calculation of the Seasonal succession of dominating phytoplankton groups indicator. 
#### _M1_ICES.Rmd_
This script reads a phytoplankton data extract from the ICES Data Portal and prepares the data for the M1-eng.R script, i.e. it groups the data based on the taxonomic information and aggregates biomasses for the groups.
#### _M1-eng.R_
This script calculates the indicator results and produces an indicator assessment. 
## Indicator data
The indicator use data collected as part of the [HELCOM COMBINE monitoring program](https://helcom.fi/action-areas/monitoring-and-assessment/monitoring-guidelines/combine-manual/). Data are reported to the COMBINE database maintained by ICES and accessible through the [ICES Data Portal](https://data.ices.dk/). 
