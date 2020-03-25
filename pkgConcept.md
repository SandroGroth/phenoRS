# phenoRS Package Concept

## 1. Data Acquisition

### 1.1 Goals

* Automatized Download of MODIS Vegetation Index Products:
    
    | Product Name                                     | Terra ID | Aqua ID |
    | ------------------------------------------------ | :------: | :-----: |
    | Vegetation Indices 16-Day L3 Global 250m 	       | MOD13Q1  |	MYD13Q1 |
    | Vegetation Indices 16-Day L3 Global 500m         | MOD13A1  |	MYD13A1 |
    | Vegetation Indices 16-Day L3 Global 1km          | MOD13A2  | MYD13A2 |
    | Vegetation Indices 16-Day L3 Global 0.05Deg CMG  | MOD13C1  | MYD13C1 |
    | Vegetation Indices Monthly L3 Global 1km         | MOD13A3  | MYD13A3 |
    | Vegetation Indices Monthly L3 Global 0.05Deg CMG | MOD13C2  | MYD13C2 |

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
* Conversion of inefficient raster data to more basic data format (testing necessary, which option performs the best):
    * .txt/.ascii files containing raw time series VI values for each row
    * binary data (similar to TIMESAT)
    * RCPP ready data format ??
    * Splitting of the data based on the available/specified Processor cores for parallel processing
* Indexing/Mapping of the converted data to link meta-information (DOY, Sensor usw.)
* Clipping images to (non-)rectangular AOI and filling up other values with NA (improves processing time)
* Reading optional Land Use Information to improve Curve fitting (weight assignment)
* Time Series Preprocessing / Curve Fitting:
    * Weight assignment using QA-Band
    * Spikes / Outliers removal, when:
        1. it deviates more than a max deviation from the median in a moving window [1]
        2. its lower than mean value of its neighbors minus cutoff [1]
        3. its larger than highest value of its neighbor plus cutoff [1]
    * Upper envelope adaption using weights
    * Number of seasons determination using seasonality parameter
    * Support for different fitting methods:
        * Savitzky-Golay Filter
        * asymetric Gaussian
        * Least-square Fits
* Viewer to inspect selected time series curves to assess the success of the curve fitting (good example: <https://github.com/kongdd/phenofit>)

### 2.2 Dependencies

### 2.3 Functions

* prepData:
    * Parameters:
        * data_location / data_index_file
        * clip_aoi (True/False)
        * image_format (Bit, Signed/Unsigned etc.)
        * lulc_image (optional): Land Use Information
        * seasonality: Number of expected seasons
        * smooth (True/False)
        * fitting_method
* view_TS

## 3. Phenology Metrics Processing

### 3.1 Goals

* Following Phenology Metrics should be supported (similar to [1]):
    * Start of season (SOS)
    * End of season (EOS)
    * Length of season (LOF)
    * Base value
    * time of middle season
    * Maximum value
    * Amplitude
    * Small integrated value
    * Large integrated value

## 4. Visualization

## 5. Output

# **References**

> \[1\] Eklundh, L., and Jönsson, P., 2017, TIMESAT 3.3 with seasonal trend decomposition and parallel processing - Software Manual. Lund University, 92 pp. <http://web.nateko.lu.se/timesat/docs/TIMESAT33_SoftwareManual.pdf>

