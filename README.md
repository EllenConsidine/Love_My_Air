# Love_My_Air
Calibrating air pollution sensors for the Denver Department of Public Health and Environment

Data needed to run these analyses:
* Hourly air quality, temperature and humidity from the Denver Department of Public Health and Environment (DDPHE) Canary-S sensors and the EPA collocated Federal Reference Monitors.
* Shapefile of Denver streets from the Denver Open Data Catalog [ https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-street-centerline]
* Coordinates (latitude, longitude) of the sensor locations

R scripts (in order):
1. Incorporating_roads.R
2. Final_data_prep.R
3. No-outliers_final_training.R (gets functions from LOO_functions.R)
4. Final_sensor_calibration_OTF.R
5. Test_set.R
