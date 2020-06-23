# MAR_tourism

Creating this repo to track the modeling and data prep code for the tourism modeling for the MAR project. Beginning July, 2019.

`processing_climate.R` - Reads in the csv files of climate projections shared by Columbia and converts them into rasters which are written out at tiffs.

`preparing_aoi.R` - Requires: Single shapefile of AOI outline. Output: Gridded (hexagonal) AOI in ESPG 4326 for globalrec workflow.

`preparing_predictors.R` - Large and messy script where predictors are intersected with the gridded aoi. Contains a record of how each intersection was done. Requires: AOI, various predictor shapefiles (some paths are certainly broken). Output: csv of predictor values per grid cell (`CombinedPredictors_dddd.csv`)

`preparing_future_climate_predictors.R` - Equivalent to `preparing_predictors.R`, but smaller and cleaner. Inputs: AOI and future climate tiffs (from processing_climate.R). Output: csv of future climate per grid cell (`Future_Climate_RCP85_2050s.csv`)

`viz_model.R` - Exploratory modeling script. Final version of the model is in `viz_model_clean.R`

`viz_model_clean.R` - Builds final visitation model. Two versions are built and output - one is scaled (so the magnitude of estimates are comparable), and the other is "raw" (so estimates are not comparable, but future predictor variables don't need to be transformed before being fed into the model). Requires: `CombinedPredictors_dddd.csv`. Outputs: Model objects (`Models/viz_model_raw.rds` & `Models/viz_model_scaled.rds`) and predictors that went into them (`Data/Predictors_Baseline.csv` & `Data/Predictors_Baseline_scaled.csv`).
