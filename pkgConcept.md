# phenoRS Package Concept

## 1. Data Acquisition

### 1.1 Goals

* Automatized Download of MODIS Vegetation Index Products:
    
    | Product Name | Terra ID | Aqua ID |
    | ------------ | :------: | :-----: |
    | Vegetation Indices 16-Day L3 Global 250m 	| MOD13Q1 |	MYD13Q1 |
    | Vegetation Indices 16-Day L3 Global 500m | MOD13A1 |	MYD13A1 |
    | Vegetation Indices 16-Day L3 Global 1km |	MOD13A2 | MYD13A2 |
    | Vegetation Indices 16-Day L3 Global 0.05Deg CMG |	MOD13C1 | MYD13C1 |
    | Vegetation Indices Monthly L3 Global 1km |	MOD13A3 | MYD13A3 |
    | Vegetation Indices Monthly L3 Global 0.05Deg CMG |MOD13C2 | MYD13C2 |

* Interactive AOI Selection based on getSpatialData package
* Interactive Time Range selection based on getSpatialData package
* Batched download of the selected MODIS acquisitions to a specified output directory
* Optional optimization of download process using Karina Küberts implementation of getSpatialData Example (https://github.com/16EAGLE/getSpatialData)

### 1.2 Dependencies

* getSpatialData (https://github.com/16EAGLE/getSpatialData)
* parallel (https://stat.ethz.ch/R-manual/R-patched/RHOME/library/parallel/)
* doParrallel (https://cran.r-project.org/web/packages/doParallel/index.html)

### 1.3 Functions

* queryMODIS:
    - Parameters:
        * time_range
        * aoi
        * product
        * value
    - Description:
    - Pseudo-Code:
        ```
        queryMODIS <- function(time_range, aoi, product, value) {

            
        }
        ```
* downloadMODIS:
    - Parameters:
        * records
        * out_dir
        * do_parallel
    - Description:
    - Pseudo-Code:
        ```
        ```

## 2. Data Preparation

### 2.1 Goals

* Extraction of all necessary data from the downloaded records in a specified location
* Inspection of the downloaded records using custom visualizations:
    * Viewer with clickable pixels to see Band-Infos at one glance
    * Pages to scroll through all acquisitions
* Conversion of inefficient raster data to a more primitive data format (testing necessary, which option performs the best):
    * .txt files containing raw time series VI values for each pixel and row
    * binary data (similar to TIMESAT)
    * RCPP ready data format ??
* Indexing/Mapping of the converted data to link meta-information (DOY, Sensor usw.)

### 2.2 Dependencies

### 2.3 Functions

* prepData:
    - Parameters
    - Description
    - Pseudo-Code:

## 3. Curve Smoothing

## 4. Phenology Metrics Processing

## 5. Output
