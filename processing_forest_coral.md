### A "recipe" for processing Coastal Forest and coral
### 7/20/20

I attempted R scripts (such as in `processing_forest.R`), but found that R + my computer couldn't handle the memory requirements of processing the rasters. So, instead I developed a simple workflow that can be done interactively in QGIS. The workflows for coastal forest and for coral are very similar.

Steps (Coastal Forest):
1. Open a new QGIS window and load ModelRuns/baseline_20200715/T_AOI_v4_5k_32616_pid.shp
2. Export a copy (keeping only "pid") as a geojson.
  - Ex: For protect forest, I'm saving it as `T_AOI_protect_forest.geojson` in the MAR/ROOT/ProtectForest/ folder
3. Load in the LULC layer.
  - Ex: `bz_s3_prot_fors_luc_names.tif`
4. Create a binary coastal forest Layer
  - Use Raster Calculator
  - Save as `Created/bz_s3_prot_fors_coastal_binary.tif`
  - Open `coastal_forest_eqns.txt` in atom and find-replace all the layer names. Then copy the entire eqn into the Raster Calculator expression
5. Run zonal Statistics
  - Toolbox / Raster analysis / Zonal Statistics
  - Raster layer: `*_coastal_binary`
  - Vector layer: `T_AOI_protect_forest`
  - Output column prefix: `bz_`
  - Statistics to calculate: Count, Sum (for now)
6. Repeat 4 & 5 with other countries (Guatemala and Honduras)
  - NOTE: Make sure to double check the results. I had some weird over-writing experiences, and ended up choosing the really basic "bz_" prefix, which seems to have fixed it (earlier I tried "bz_s3_forest_")

Coral steps
1. As above
2. As above (but keep all columns for the first `T_AOI_coral_baseline.geojson`)
  - Saved as `T_AOI_prot_fors_coral.geojson` in `MAR/ROOT/ProtectForest/CoralScenarios`
3. Create a binary coastal layer by removing pixels where coral cover is < 10%.
  - Use Raster Calculator
  - Save as  `CoralScenarios/Created/bz_s3_prot_fors_clim0_cor_10.tif`
  - Equation: "s3_prot_fors_clim0_cor@1" > 10
4. Run zonal Statistics (as above)
  - Output column prefix: `bz_c0_`
  - Statistic: Sum
  - NOTE: this will leave NULL values in some cells which don't overlap with the coral raster. This is ok - they're dealt with later in the `preparing_predictors.r` script
