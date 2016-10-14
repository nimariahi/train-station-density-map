# Plot choropleth map using R

This is a short demonstration example on how to use `R` and the `R` package `ggplot2` to produce a choropleth map. 


## What visualization problems are solved

A colored choropleth map is produced from a shape file and an external data source. There are two layers of polygons: the colored polygons based on second-level political entities (*Gemeinden*) and white boundaries based on frist-level poltical entities (*Kantone*).

- **Geographic polygons**: political boundaries in Switzerland
- **The visualized statistic**: density of train stations (/km^2)


## How to run the code

In R, change the working directory to the root directory where this package is stored. Open and run `station-density-map-CH.R`.


## Data source

The data are from the Open Government Data site https://opendata.swiss. Details are found in the data directories.


