# phenoRS

![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg) 
[![CRAN version](https://www.r-pkg.org/badges/version/phenoRS)](https://CRAN.R-project.org/package=phenoRS)
[![Build Status](https://travis-ci.org/SandroGroth/phenoRS.svg?branch=master)](https://travis-ci.org//SandroGroth/phenoRS)
![Last Commit](https://img.shields.io/github/last-commit/SandroGroth/phenoRS/develop)
![codecov]("https://codecov.io/github/SandroGroth/phenoRS/branch/master/graph/badge.svg")(https://codecov.io/github/SandroGroth/phenoRS)
![License](https://img.shields.io/github/license/SandroGroth/phenoRS)


R package for automated phenology metrics extraction from remotely sensed data.

:warning: **This package is still in an early state of development and not yet ready to use.**


# Project Status:

- [ ] **Data Aquisition**:
    - [ ] Automatized download of remote sensing products (based on getSpatialData [4] package):
        - [x] MODIS VI Composites 
        - [ ] Landsat 4, 5, 7, 8
        - [ ] Sentinel 2
    - [x] Downloading options:
        - [x] aria2c parallelized bulk download
        - [x] R parallelized download (using doSNOW)
        - [x] standard download
        
- [ ] **Data Extraction and Preparation**:
    - [x] Supported products:
        - [x] MOD13Q1 v6
        - [ ] Landsat 4, 5, 7, 8
        - [ ] Sentinel 2
    - Preparation functions (based on GDAL):
        - [ ] Data extraction:
            - [x] .hdf -> .tif
            - [ ] .tar.gz -> .tif
            - [ ] SAFE -> .tif
        - [ ] Vegetation Index calculation
        - [x] Tile mosaicking
        - [x] Reprojection
        - [x] AOI Cropping
        - [x] AOI Masking
        - [x] Conversion to .envi binary files (for faster processing)
    - [ ] Reading optional Land Use Information to improve Curve fitting (weight assignment)
    
- [ ] **Time Series Preprocessing / Curve Fitting**:
    - [ ] Shiny App for interactive selection of fine tuning parameters (in Progress)
    - [ ] Settings-Management: Reding/Wrtiting settings as .json files. (in Progress)
    - [ ] Initial weight assignment based on QA Band for:
        - [x] MOD13Q1 v6 Summary QA
        - [ ] MOD13Q1 v6 Detailed QA
        - [ ] Landsat QA
        - [ ] Sentinel 2 QA
    - [ ] Outilier-Detection:
        - [x] QA-Band
        - [x] Modified Hampel Median Filter [3]
        - [ ] STL decomposition [1]
        - [ ] STL * initial weights [2] 
    - [ ] Seasonalty extraction
    - [ ] Fitting methods:
        - [ ] Adaptive Savitzky-Golay Filter
        - [ ] Asymetric Gaussian
        - [ ] Double logistic functions
        - [ ] Harmonic modeling [5]
    - [ ] Upper envelope adaption
    
- [ ] **Phenology Metrics Extraction**:
    - [ ] Start of season (SOS)
    - [ ] End of season (EOS)
    - [ ] Length of season (LOF)
    - [ ] Base value
    - [ ] time of middle season
    - [ ] Maximum value
    - [ ] Amplitude
    - [ ] Small integrated value
    - [ ] Large integrated value
    
- [ ] **Change Detection**:
    - tbd
    
- [ ] **Output Visualisations**:
    - tbd

# References

> \[1\] Cleveland, R.B., Cleveland, W.S., McRae, J.E., and Terpenning, I., 1990, STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3-73.

> \[2\] Eklundh, L., and Jönsson, P., 2017, TIMESAT 3.3 with seasonal trend decomposition and parallel processing - Software Manual. Lund University, 92 pp. <http://web.nateko.lu.se/timesat/docs/TIMESAT33_SoftwareManual.pdf>.

> \[3\] Hampel F. R.,1974, The influence curve and its role in robust estimation.
Journal of the American Statistical Association, 69, 382–393.

> \[4\] Schwalb-Willmann, J., 2018, getSpatialData - Get different kinds of freely available
spatial datasets. R package version 0.0.4. <http://www.github.com/16eagle/getSpatialData/>.

> \[5\] Philipp, M, 2020, rHarmonics- R package for harmonic modelling of time-series data.
R package version 0.1.0. <http://www.github.com/MBalthasar/rHarmonics/>.
