# phenoRS

![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg) 
[![CRAN version](https://www.r-pkg.org/badges/version/esriOpenData)](https://CRAN.R-project.org/package=esriOpenData)

R package for automated phenology metrics extraction from remotely sensed data.

## Project Status:

- [ ] **Data Aquisition**:
    - [x] Automatized Download of MODIS Vegetation Index Products:
        - [x] MOD13Q1 V6
        - [ ] MOD13A1
        - [ ] MOD13A2  
        - [ ] MOD13C1
        - [ ] MOD13A3
        - [ ] MOD13C2
    - [x] Include aria2c bulk downloading
- [ ] **Data Preprocessing**:
    - [x] Extraction of data from .hdf archive
        - [x] NDVI
        - [x] EVI
        - [x] QA
        - [x] DOY
    - [x] Mosaicing of all tiles with similar DOY
    - [ ] Inspection of the downloaded records using custom visualizations:
        - [ ] Viewer with clickable pixels to see Band-Infos at one glance
        - [ ] Pages to scroll through all acquisitions
    Clipping images to (non-)rectangular AOI and filling up other values with NA (improves processing time)
    - [ ] Reading optional Land Use Information to improve Curve fitting (weight assignment)
    - [ ] Time Series Preprocessing / Curve Fitting:
        - [ ] Weight assignment using QA-Band
        - [ ] Spikes / Outliers removal, when:
            - [ ] it deviates more than a max deviation from the median in a moving window [1]
            - [ ] its lower than mean value of its neighbors minus cutoff [1]
            - [ ] its larger than highest value of its neighbor plus cutoff [1]
        - [ ] Upper envelope adaption using weights
        - [ ] Number of seasons determination using seasonality parameter
        - [ ] Support for different fitting methods:
            - [ ] Savitzky-Golay Filter
            - [ ] asymetric Gaussian
            - [ ] Least-square Fits
    - [ ] Viewer to inspect selected time series curves to assess the success of the curve fitting (good example: <https://github.com/kongdd/phenofit>)
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

# References

> \[1\] Eklundh, L., and Jönsson, P., 2017, TIMESAT 3.3 with seasonal trend decomposition and parallel processing - Software Manual. Lund University, 92 pp. <http://web.nateko.lu.se/timesat/docs/TIMESAT33_SoftwareManual.pdf>