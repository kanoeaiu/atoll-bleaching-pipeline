# atoll-bleaching-pipeline
scripts to retrieve and analyze sea surface temperature data from NOAA's multi scale high resolution dataset. Analysis includes calculation of several statistics related to coral bleaching, modeling of impact of atoll rim typology and depth on temperature profile, and comparison of atolls by region.n
These scripts were developed collaboratively during an internship with the Palumbi Lab at Hopkins Marine Station.

This repository includes 
1. A script to pull NOAA sst data and conduct preliminary analysis. This requires user input of a working directory, in addition to 4 points deliniating an ellipse withing the atoll.
These points should be your best approximate of the endpoints of ellipse axis based on visual assessment on GIS software. This script saves relevant calculated values for each atoll into
a single csv, adding a column automatically each time a new atoll is provided.
2. An ellipse generation function: the path to this file must be filled in as the source in the atoll pipeline. This function develops a parametric of the ellipse based on the user inputted
points and filters the NOAA dataset for sensor points withing the ellipse.
3. Rerunning Ellipses: This script reads in a csv with the name of the atoll, each ellipse point with latitude longitude pairs in adjacent columns and opposite endpoints of one axis, then opposite
endpoints of the other axis. There is an option to include 2 additional columns with the latitudinal and longitudinal shift desired. The entire pipeline is run on each row representing one atoll in the csv.
This script has an additional loop for running the graphing function on atolls for which the data has already been downloaded and filtered.
4. Noaa style dhw: this script is for calculating the NOAA degree heating weeks on already downloaded data sets. Only input needed is the csv created by the atoll-pipeline script.
5. Getting bathymetry data: This script requires the download of NOAA's etopo 1 bathymetry data. It uses the table created by the main pipeline, and searches a square surrounding the atoll center
for the deepest point. Next release will use the unique set of points within the SST dataset, find the nearest bathymetry point to each sst point, and average the bathymetry over the entire atoll.
6. Graphing atolls: This script requires the user to add an additional row at the bottom of the outputted data table containing visual submersion ratings. It then generates linear models and visuals
comparing temperature differences to various atoll traits.
