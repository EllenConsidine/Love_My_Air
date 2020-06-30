# Love_My_Air
Calibrating air pollution sensors for the Denver Department of Public Health and Environment

Data needed to run these analyses (all files are in the R/Data/ folder):
* Hourly air quality, temperature and humidity from the Denver Department of Public Health and Environment (DDPHE) Canary-S sensors and the EPA collocated federal equivalence method (FEM) monitors.
* Shapefile of Denver streets from the [Denver Open Data Catalog](https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-street-centerline) -- the processed file, called Road_lengths_3.csv, is included instead of the original shapefiles because those are too large to upload on GitHub.
* Coordinates (latitude, longitude) of the sensor locations

Python code: DDPHE_code.ipynb illustrates how to run the final long-term and real-time (on-the-fly) calibration models.
