# MAR_tourism

Creating this repo to track the modeling and data prep code for the tourism modeling for the MAR project. Beginning July, 2019.

### Visitation Model

#### Data prep
`preparing_aoi.R` - Requires: Single shapefile of AOI outline. Output: Gridded (hexagonal) AOI in ESPG 4326 for globalrec workflow.

#### Analysis
`downscaling.R` - Downscales national tourism numbers to in vs out of AOI according to proportion of PUD in and out of the AOI in that country.
- Requires: globalrec outputs for the CountriesplusTAOI_v3 shapefile, national visitation estimates
- Output: proportioned_viz_2017_AOIv3.csv

`preparing_socmed.R` - Reads in monthly PUD and TUD tables & pid shapefile, then calculates average annual PUD, TUD, and SMUD (combined social media UDs)
- Requires: Outputs from globalrec (XXX_pid.shp, userdays_total_monthly_bypid.csv for both pud and tud)
- Outputs: aoi_smud.shp, aoi_smud_gt_0.shp

`summarising_viz_expends.R` - Reads in social media user-day data across the landscape, as well as proportional estimates of visitors to the AOI per country and estimated expenditure data. Combines them to create estimated visitors and expenditures per grid cell, country, and MPA.
- Requires: Total visitors to the AOI in each country to be distributed out based on PUD and TUD (proportioned_viz_2015.csv from downscaling.r); Outputs from preparing_socmed.R (aoi_smud.shp)
- Output: aoi_viz_exp.shp, various plots of estimated visitation vs empirical visitation, country_summaries.csv, mar_summary

#### Exploratory
`belize_viz_model.R` - Testing the best way to combine PUD and TUD to get an estimate of visitation (by comparing to empirical data from belize)

### Preference Model

#### Data prep

`processing_climate.R` - Reads in the csv files of climate projections shared by Columbia and converts them into rasters which are written out at tiffs.

`reprojecting_files.R` - Reads in either shapefiles or rasters and writes out valid versions that have been transformed to ESPG 32616 (WGS84 16N).

`combining_wildlife.R` - Combined several different wildlife layers into wildlife2.shp (I think I ended up with a 3, which must have some additional modifications)

#### Analysis
`preparing_predictors.R` - Large and messy script where predictors are intersected with the gridded aoi. Contains a record of how each intersection was done. Requires: AOI, various predictor shapefiles (some paths are certainly broken). Output: csv of predictor values per grid cell (`CombinedPredictors_dddd.csv`)

`preparing_climate_predictors.R` - Equivalent to `preparing_predictors.R`, but smaller and cleaner. Inputs: AOI and future climate tiffs (from processing_climate.R). Output: csv of climate per grid cell (`Future_Climate_RCP85_2050s.csv`)

`preparing_non_climate_predictors.R` - Clean version of `preparing_predictors.R` to be used in a more reproducible and easy way. Reads in clean predictor shapefiles (ESPG 32616), intersects them with the AOI, and writes out a csv and geojson of predictor values per grid cell (NonClimatePredictors_dddd.csv and .geojson).

`viz_model.R` - Exploratory modeling script. Final version of the model is in `viz_model_clean.R`

`viz_model_clean.R` - Builds final visitation model. Two versions are built and output - one is scaled (so the magnitude of estimates are comparable), and the other is "raw" (so estimates are not comparable, but future predictor variables don't need to be transformed before being fed into the model). Requires: `CombinedPredictors_dddd.csv`. Outputs: Model objects (`Models/viz_model_raw.rds` & `Models/viz_model_scaled.rds`) and predictors that went into them (`Data/Predictors_Baseline.csv` & `Data/Predictors_Baseline_scaled.csv`).

`viz_predict.R`
