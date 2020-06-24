library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(lubridate)
library(rgdal)
library(sf)
library(sp)
library(raster)
library(mapview)
library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)
library(rHarmonics)


#' @title Test Curve Fitting in GUI.
#'
#' @import shiny
#' @import shinyFiles
#' @import shinydashboard
#' @import leaflet
#' @importFrom lubridate leap_year
#' @importFrom rHarmonics harmonics_fun
#' @importFrom dplyr pull mutate case_when
#' @importFrom raster extract raster brick
#' @importFrom magrittr %>% set_names
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs useShinyjs hide show hidden toggle toggleElement
#' @importFrom shinydashboardPlus boxPad
#' @import sf
#' @import sp
#' @import mapview
#' @import tibble
#' @import ggplot2
#'
#' @export
#'
testFIT <- function() {

  # UI =========================================================================================================

  ui <- dashboardPage(

    # Header ---------------------------------------------------------------------------------------------------
    dashboardHeader(title = "Test Curve Fitting"),

    # Sidebar --------------------------------------------------------------------------------------------------
    dashboardSidebar(disable = TRUE),

    # Body -----------------------------------------------------------------------------------------------------
    dashboardBody(
      useShinyjs(),
      fluidRow(
        column(8,
          box(width = NULL, solidHeader = TRUE,
             plotOutput('fitPlot') %>%
               withSpinner(color="#0dc5c1")
          )
        ),
        column(4,
          box(width = NULL, solidHeader = TRUE,
             leafletOutput("map") %>%
               withSpinner(color="#0dc5c1")
          )
        )
      ),
      fluidRow(
        column(12/4,
          box(width = NULL, status = "warning",
             h3("1. Load Data"),
             fluidRow(style = 'padding:3px;',
                      column(3,
                             shinyFilesButton('aoiFileChoose', label = 'Load AOI', title = 'Select a AOI', multiple = FALSE)
                      ),
                      column(9,
                             textOutput('aoiPathOutput')
                      )
             ),
             fluidRow(style = 'padding:3px;',
                      column(3,
                             shinyDirButton('prepDirChoose', label = 'Load TS', title = 'Load Timeseries data')
                      ),
                      column(9,
                             textOutput('prepDirOutput') %>%
                               withSpinner(color="#0dc5c1", size = 0.3, proxy.height = "50px")
                      )
             ),
             selectInput('displayViSelect', label = 'Display VI', choices = NULL),
             fluidRow(
               column(12/3,
                      numericInput('maxDataGapNum', label = 'Max. data gap', value = 4, min = 0, step = 1)
               ),
               column(12/3,
                      numericInput('internalMinNum', label = 'Internal Min.', value = -999, step = 1)
               ),
               column(12/3,
                      numericInput('pointsPerYearNum', label = 'Points per year', value = 23, min = 0, step = 1)
               )
             )
          )
        ),
        column(12/4,
          box(width = NULL, status = "warning",
            h3("2. Outlier removal"),
            checkboxInput('useQaCheck', label = 'Use Quality data', value = FALSE),
            fluidRow(
              column(12/3,
                     numericInput('wMinNum', label = 'Min. weight', value = 0.0, min = 0.0, max = 1.0, step = 0.1)
              ),
              column(12/3,
                     numericInput('wMedNum', label = 'Med. weight', value = 0.5, min = 0.0, max = 1.0, step = 0.1)
              ),
              column(12/3,
                     numericInput('wMaxNum', label = 'Max. weight', value = 1.0, min = 0.0, max = 1.0, step = 0.1)
              )
            ),
            selectInput('spikeMethodSelect', label = 'Spike Method', choices = c('None', 'Median', 'STL', 'STL_w')),
            hidden(
              numericInput('spikeValueNum', label = 'Spike Value', value = 2.0, step = 0.1)
            ),
            hidden(
              numericInput('stlStiffnessNum', label = 'STL stiffness', value = 3.0, step = 0.1)
            )
          )
        ),
        column(12/4,
          box(width = NULL, status = "warning",
            h3("3. Curve Fitting"),
            h4("Fitting Methods: "),
            fluidRow(
              column(12/3,
                checkboxInput('harmModCheck', label = 'Harmonic Modeling', value = FALSE)
              ),
              column(12/3,
                checkboxInput('savGolCheck', label = 'Adapt. Sav. Gol.', value = FALSE)
              ),
              column(12/3,
                checkboxInput('asymGaussCheck', label = 'Asymmetric Gauss', value = FALSE)
              )
            ),
            hidden(
              boxPad(id = 'harmModParamPad', color = 'gray',
                span("Harmonic Modeling: "),
                numericInput('nSeasonsNum', label = 'Number of seasons per year', value = 1)
              )
            ),
            hidden(
              boxPad(id = 'savGolParamPad', color = 'gray',
                span("Adaptive Savitzky-Golay Filter: "),
                fluidRow(
                  column(12/2,
                    numericInput('winSizeNum', label = 'Window size', value = 7)
                  ),
                  column(12/2,
                    numericInput('polyDegNum', label = 'Polynomial degree', value = 2)
                  )
                ),
                checkboxInput('upperEnvCheck', label = 'Adaption to upper envelope', value = FALSE),
                hidden(
                  fluidRow(id = 'upperEnvParamRow',
                    column(12/2,
                      numericInput('nIterNum', label = 'Iterations', value = 1, min = 1, step = 1)
                    ),
                    column(12/2,
                      numericInput('wFactNum', label = 'Weight update factor', value = 2, min = 1, step = 1)
                    )
                  )
                )
              )
            )
          )
        ),
        column(12/4,
          box(width = NULL, status = "warning",
             h3("4. Phenology metrics")
          )
        )
      )
    )
  )

  # Server =====================================================================================================

  server <- function(input, output, session) {

    # Globals --------------------------------------------------------------------------------------------------

    volumes <- getVolumes()

    # init elements --------------------------------------------------------------------------------------------

    # AOI File selection
    shinyFileChoose(input, 'aoiFileChoose', roots = volumes, filetypes = c('', 'shp'))
    output$aoiPathOutput <- renderText(basename(unname(parseFilePaths(volumes, input$aoiFileChoose)$datapath)))

    # VI FIles selection
    shinyDirChoose(input, 'prepDirChoose', roots = volumes(), filetypes = c('', 'envi', 'tif'), session = session,
                   restrictions = system.file(package = 'base'))
    output$prepDirOutput <- renderText(unname(parseDirPath(volumes, input$prepDirChoose)))

    # Initial Map View
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(noWrap = TRUE)) %>%
        setView(0, 0, zoom = 1) %>%
        addMouseCoordinates()
    })

    # Initial Plot
    output$fitPlot <- renderPlot(ggplot())

    # reactives ------------------------------------------------------------------------------------------------

    # ---- 0. General Settings ----

    max_data_gap <- reactive({input$maxDataGapNum})

    internal_min <- reactive({input$internalMinNum})

    points_per_year <- reactive({input$pointsPerYearNum})

    # ---- 1. Load Data ----

    ### Input Paths
    aoi_path <- reactive({
      req(input$aoiFileChoose)
      unname(parseFilePaths(volumes, input$aoiFileChoose)$datapath)
    })

    prep_path <- reactive({
      req(input$prepDirChoose)
      unname(parseDirPath(volumes, input$prepDirChoose))
    })

    vi_paths <- reactive({
      req(prep_path())
      list.files(prep_path(), pattern = '.*_(NDVI|EVI)_prepbin.envi$', full.names = T, no.. = T)
    })

    doy_paths <- reactive({
      req(prep_path())
      list.files(prep_path(), pattern = '.*_DOY_prepbin.envi$', full.names = T, no.. = T)
    })

    qa_paths <- reactive({
      req(prep_path())
      list.files(prep_path(), pattern = '.*_QA_prepbin.envi$', full.names = T, no.. = T)
    })

    ### Datacubes
    vi_datacube <- reactive({
      req(vi_paths())

      s <- raster::brick()
      n <- length(vi_paths())

      withProgress(message = "Building VI data cube...", {
        for (i in 1:n) {
          r <- readAll(raster::raster(vi_paths()[i]))
          names(r) <- strsplit(basename(vi_paths()[i]), '_')[[1]][1]
          s <- addLayer(s, r)
          incProgress(1/n)
        }
      })

      s <- set_names(s, unlist(lapply(vi_paths(), function (x) {basename(x)})))
      readAll(s)

      #raster::brick(sapply(vi_paths(), function(x) {
      #  r <- raster::raster(x)
      #  names(r) <- strsplit(basename(x), '_')[[1]][1]
      #  return(r)})) %>%
      #  set_names(unlist(lapply(vi_paths(), function (x) {basename(x)})))
    })

    doy_datacube <- reactive({
      req(doy_paths())

      s <- raster::brick()
      n <- length(doy_paths())

      withProgress(message = "Building DOY data cube...", {
        for (i in 1:n) {
          r <- readAll(raster::raster(doy_paths()[i]))
          names(r) <- strsplit(basename(doy_paths()[i]), '_')[[1]][1]
          s <- addLayer(s, r)
          incProgress(1/n)
        }
      })

      s <- set_names(s, unlist(lapply(doy_paths(), function(x) {basename(x)})))
      s

      #sapply(doy_paths(), function(x) {raster(x)}) %>%
      #  brick()
    })

    qa_datacube <- reactive({
      req(qa_paths())

      s <- raster::brick()
      n <- length(qa_paths())

      withProgress(message = "Building QA data cube...", {
        for (i in 1:n) {
          r <- readAll(raster::raster(qa_paths()[i]))
          names(r) <- strsplit(basename(qa_paths()[i]), '_')[[1]][1]
          s <- addLayer(s, r)
          incProgress(1/n)
        }
      })

      s <- set_names(s, unlist(lapply(qa_paths(), function(x) {basename(x)})))
      s

      #sapply(qa_paths(), function(x) {raster(x)}) %>%
      #  brick()
    })

    doy_years <- reactive({
      req(doy_paths())
      doy_paths() %>%
        lapply(. %>% {basename(.)}) %>%
        unlist() %>%
        substr(1, 4) %>%
        as.integer()
    })

    ### Currently displayed VI Layer
    vi_crs <- reactive({
      req(vi_datacube())
      crs(vi_datacube())
    })

    vi_selected <- reactive({
      req(vi_datacube())
      input$displayViSelect
    })

    ### Current click coordinates
    curr_click <- reactive({
      req(input$map_click)
      print("Getting click ccordinates")
      SpatialPoints(cbind(input$map_click$lng, input$map_click$lat), proj4string = CRS("+proj=longlat")) %>%
        spTransform(., vi_crs())
    })

    ## Current Time Series (at click location)
    curr_ts <- reactive({
      req(vi_datacube(), doy_datacube(), qa_datacube(), curr_click())
      list(
        'vi' = extract(vi_datacube(), curr_click()) %>%
          unname() %>%
          .[1, ] %>%
          check_vi_ts(., max_data_gap(), internal_min()),
        'dates' = extract(doy_datacube(), curr_click()) %>%
          unname() %>%
          .[1, ] %>%
          check_doy_ts(.) %>%
          data.frame(years=doy_years(), doys=as.integer(.), stringsAsFactors = F) %>%
          as.tibble() %>%
          mutate(
            years_1 = case_when(
              !is.na(doys) & leap_year(years) & doys > 366  ~ as.integer(years + 1),  # doy > 366 and leap year
              TRUE ~ years
            ),
            doys_1 = case_when(
              !is.na(doys) & leap_year(years) & doys > 366  ~ as.integer(doys - 366), # doy > 366 and leap year
              TRUE ~ doys
            )
          ) %>%
          mutate(
            years_2 = case_when(
              !is.na(doys) & !leap_year(years) & doys > 365 ~ as.integer(years + 1),  # doy > 365 and no leap year
              TRUE ~ years_1
            ),
            doys_2 = case_when(
              !is.na(doys) & !leap_year(years) & doys > 365 ~ as.integer(doys - 365), # doy > 365 and no leap year
              TRUE ~ doys_1
            )
          ) %>%
          mutate(
            dates = as.Date(paste0(as.character(years_2), as.character(doys_2)), format = "%Y%j")
          ) %>%
          pull(dates),
        'qa' = extract(qa_datacube(), curr_click()) %>%
          unname() %>%
          .[1, ]
      )
    })

    # ---- 2. Spike removal ----

    use_qa <- reactive({input$useQaCheck})

    w_min <- reactive({input$wMinNum})

    w_med <- reactive({input$wMedNum})

    w_max <- reactive({input$wMaxNum})

    spike_method <- reactive({input$spikeMethodSelect})

    spike_value <- reactive({input$spikeValueNum})

    stl_stiffness <- reactive({stlStiffnessNum})

    curr_weights <- reactive({
      req(curr_ts())
      if(isTRUE(use_qa())) {
        switch(spike_method(),
          'Median' = MODIS_summary_qa(curr_ts()$qa, w_min(), w_med(), w_max()) %>%
                      spike_median(curr_ts()$vi, ., points_per_year(), w_min(), spike_value(), cpp = F),
          'STL'    = NULL, # TODO
          'STL_w'  = NULL, # TODO
          'None'   = MODIS_summary_qa(curr_ts()$qa, w_min(), w_med(), w_max())
        )
      } else {
        switch(spike_method(),
          'Median' = rep(w_max(), length(curr_ts()$qa)) %>%
                      spike_median(curr_ts()$vi, ., points_per_year(), w_min(), spike_value(), cpp = F),
          'STL'    = NULL,
          'STL_w'  = NULL,
          'None'   = rep(w_max(), length(curr_ts()$vi))
        )
      }
    })

    curr_weight_flag <- reactive({
      req(curr_weights(), curr_ts(), internal_min())

      rep('others', length(curr_weights())) %>%
        replace(curr_ts()$vi == internal_min(), 'internal min') %>%
        replace(curr_weights() == w_min(), 'bad') %>%
        replace(curr_weights() == w_med(), 'marginal') %>%
        replace(curr_weights() == w_max(), 'good')
    })

    # ---- 3. Curve Fitting ----

    ## Harmonic Modeling
    n_seasons <- reactive({input$nSeasonsNum})

    harm_ts <- reactive({
      req(curr_ts(), n_seasons())
      rHarmonics::harmonics_fun(curr_ts()$vi, curr_ts()$dates, n_seasons())
    })

    ## Adaptive Savitzky Golay Filter
    savgol_win_size <- reactive({input$winSizeNum})

    savgol_poly_deg <- reactive({input$polyDegNum})

    savgol_is_upper_env <- reactive({input$upperEnvCheck})

    savgol_n_iterations <- reactive({input$nIterNum})

    savgol_w_fact <- reactive({input$wFactNum})

    savgol_ts <- reactive({
      #req(curr_ts(), curr_weights(), points_per_year(), savgol_win_size(), savgol_poly_deg(),
      #    savgol_is_upper_env(), savgol_n_iterations(), savgol_w_fact())
      phenoRS::smooth_regWSavGol(curr_ts()$vi, curr_weights(), points_per_year(), savgol_win_size(),
                                 savgol_poly_deg(), savgol_n_iterations(), savgol_w_fact()) %>%
        .$fits %>%
        .[[length(.)]]
    })

    # ---- Plot preparation ----

    ## Current Timeseries Dataframe
    curr_ts_df <- reactive({
      req(curr_ts(), curr_weights(), curr_weight_flag(), harm_ts(), savgol_ts())
      data.frame(date=curr_ts()$dates,
                 vi=curr_ts()$vi,
                 weights = curr_weights(),
                 weight_flags = curr_weight_flag(),
                 vi_harm=harm_ts(),
                 vi_savgol=savgol_ts())
    })

    ## Base Plot
    base_plot <- reactive({
      req(curr_ts_df())
      ggplot(curr_ts_df(), aes(x=date)) +
        geom_point(aes(y=vi, colour = weight_flags)) +
        scale_colour_manual("",
                            breaks = c("good", "marginal", "bad", "internal_min", "Harmonic", "Sav. Gol."),
                            values = c("#00a65a", "#f39c12", "#dd4b39", "#001f3f", "#3c8dbc", "black"))
    })

    ## Fitting methods layer
    layer_harm_mod <- function() {
      geom_line(aes(y=vi_harm, colour = 'Harmonic'))
    }

    layer_savgol <- function() {
      geom_line(aes(y=vi_savgol, colour = 'Sav. Gol.'))
    }

    ## Current Plot
    # curr_plot <- reactive({
    #   req(curr_ts_df(), base_plot())
    #   ggplot(curr_ts_df(), aes(x=date)) +
    #     geom_point(aes(y=vi, colour = weight_flags)) +
    #     geom_line(aes(y=vi_harm, colour = "Harmonic")) +
    #     scale_colour_manual("",
    #                         breaks = c("Harmonic", "good", "marginal", "bad", "internal_min"),
    #                         values = c("red", "green", "yellow", "red", "orange"))
    # })

    # observers ------------------------------------------------------------------------------------------------

    ## Visibility

    # spike parameters
    observeEvent(input$spikeMethodSelect, {
      toggleElement(id = 'spikeValueNum',
                    condition = spike_method() == 'Median',
                    anim = TRUE)
      toggleElement(id = 'stlStiffnessNum',
                    condition = spike_method() %in% c('STL', 'STL_w'),
                    anim = TRUE)
    }, ignoreInit = TRUE)

    # Curve fitting parameters
    observeEvent(input$harmModCheck, {
      toggleElement(id = 'harmModParamPad',
                    condition = isTRUE(input$harmModCheck),
                    anim = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$savGolCheck, {
      toggleElement(id = 'savGolParamPad',
                    condition = isTRUE(input$savGolCheck),
                    anim = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$upperEnvCheck, {
      toggleElement(id = 'upperEnvParamRow',
                    condition = isTRUE(input$upperEnvCheck),
                    anim = TRUE)
    }, ignoreInit = TRUE)

    ## Map
    # Add AOI to leaflet map
    observe({
      if (length(aoi_path()) > 0) {
        aoi_data <- st_read(aoi_path()) %>%
          st_transform(crs = "+init=epsg:4326")
        bbox <- unname(st_bbox(aoi_data))
        names(st_geometry(aoi_data)) <- NULL
        leafletProxy('map', data = aoi_data) %>%
          clearShapes() %>%
          addPolygons(
            weight = 1,
            color = '#FF0000',
            opacity = 0.9,
            fill = FALSE,
            options = pathOptions(
              clickable = FALSE
            )
          ) %>%
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
        }
      })

    # Add loaded VI paths to VI Selector
    observe({
      if (length(vi_paths()) > 0) {
        updateSelectInput(session = session, inputId = 'displayViSelect', choices = basename(vi_paths()))
      }
    })

    # Add selected Vi to map
    observe({
      if (vi_selected() != "") {
        r <- vi_datacube()[[grep(vi_selected(), names(vi_datacube()))]]
        leafletProxy('map') %>%
          addRasterImage(
            r
          )
      }
    })

    # Extract click coordinates
    observeEvent(input$map_click, {
      ##
      print("CLICK")
      print(savgol_win_size())
      print(savgol_poly_deg())
      print(savgol_is_upper_env())
      print(savgol_n_iterations())
      ##
      click <- input$map_click
      leafletProxy('map')%>%
        clearMarkers() %>%
        addMarkers(lng = click$lng, lat = click$lat)
      output$fitPlot <- renderPlot({
        p <- base_plot()
        if(input$harmModCheck == TRUE) p <- p + layer_harm_mod()
        if(input$savGolCheck == TRUE) p <- p + layer_savgol()
        p
      })

      print(savgol_ts())
      print(head(curr_ts_df()))
    })
  }

  shinyApp(ui, server)

}
