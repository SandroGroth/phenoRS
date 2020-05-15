library(shiny)
library(shinyFiles)
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

# UI ============================================================================================================

ui <- fluidPage(

  # Title ------------------------------------------------------------------------------------------------------
  titlePanel("Test Curve Fitting"),

  # Map View ---------------------------------------------------------------------------------------------------
  fluidRow(
    column(8,
      leafletOutput("map"),
    ),
    column(4,
      fluidRow(
        column(4,
          shinyFilesButton('aoiFileChoose', label = 'Load AOI', title = 'Select a AOI', multiple = FALSE)
        ),
        column(8,
          textOutput('aoiPathOutput')
        )
      ),
      fluidRow(
        column(4,
          shinyDirButton('prepDirChoose', label = 'Load TS', title = 'Load Timeseries data')
        ),
        column(8,
           textOutput('prepDirOutput')
        )
      ),
      fluidRow(
        selectInput('displayViSelect', label = "Display VI", choices = NULL)
      )
    )
  ),

  # Plot View --------------------------------------------------------------------------------------------------
  fluidRow(
    column(8,
      plotOutput('fitPlot')
    ),
    column(4,
      numericInput('nSeasons', label = 'Number of Seasons / Year', value = 1, step = 1)
    )
  )
)

# Server ========================================================================================================

server <- function(input, output, session) {

  # Globals -----------------------------------------------------------------------------------------------------

  volumes <- getVolumes()

  # init elements -----------------------------------------------------------------------------------------------

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

  # reactives ---------------------------------------------------------------------------------------------------

  aoi_path <- reactive({
    if (!is.null(input$aoiFileChoose)) {
      unname(parseFilePaths(volumes, input$aoiFileChoose)$datapath)
    } else NULL
  })

  prep_path <- reactive({
    if (!is.null(input$prepDirChoose)) {
      unname(parseDirPath(volumes, input$prepDirChoose))
    } else NULL
  })

  vi_paths <- reactive({
    if (!is.null(prep_path())) {
      list.files(prep_path(), pattern = '.*_(NDVI|EVI)_prep.envi$', full.names = T, no.. = T) #TODO: _prepbin
    } else NULL
  })

  doy_paths <- reactive({
    if (!is.null(prep_path())) {
      list.files(prep_path(), pattern = '.*_DOY_prep.envi$', full.names = T, no.. = T) #TODO: _prepbin
    } else NULL
  })

  qa_paths <- reactive({
    if (!is.null(prep_path())) {
      list.files(prep_path(), pattern = '.*_QA_prep.envi$', full.names = T, no.. = T) #TODO: _prepbin
    } else NULL
  })

  vi_datacube <- reactive({
    if (!is.null(vi_paths())) {
      dc_vi  <- raster::brick(sapply(vi_paths(), function(x) {
        r <- raster::raster(x)
        names(r) <- strsplit(basename(x), '_')[[1]][1]
        return(r)}))
      names(dc_vi) <- unlist(lapply(vi_paths(), function (x) {basename(x)}))
      dc_vi
    } else NULL
  })

  doy_rasters <- reactive({
    if(!is.null(doy_paths())) {
      sapply(doy_paths(), function(x) {raster(x)})
    } else NULL
  })

  doy_datacube <- reactive({
    if (!is.null(doy_rasters()) & !is.null(doy_paths())) {
      brick(doy_rasters())
    } else NULL
  })

  doy_years <- reactive({
    if (!is.null(doy_paths()) & !is.null(curr_click())) {
      doy_paths() %>%
        lapply(. %>% {basename(.)}) %>%
        unlist() %>%
        substr(1, 4) %>%
        as.integer()
    } else NULL
  })

  doy_curr_ts <- reactive({
    if (!is.null(doy_datacube()) &!is.null(doy_paths())) {
      extract(doy_datacube(), curr_click()) %>%
        unname() %>%
        .[1, ]
    } else NULL
  })

  doy_df <- reactive({
    if (!is.null(doy_years()) & !is.null(doy_curr_ts())) {
      data.frame(years=doy_years(), doys=doy_curr_ts(), stringsAsFactors = F) %>%
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
         )
    } else NULL
  })

  doy_dates <- reactive({
    if (!is.null(doy_df())) {
      doy_df() %>%
        pull(dates)
    } else NULL
  })

  vi_crs <- reactive({
    if (!is.null(vi_datacube())) {
      crs(vi_datacube())
    } else NULL
  })

  vi_selected <- reactive({
    if (!is.null(vi_datacube)) {
      input$displayViSelect
    } else NULL
  })

  curr_click <- reactive({
    click <- input$map_click
    if (!is.null(click)) {
      sp_click <- SpatialPoints(cbind(click$lng, click$lat), proj4string = CRS("+proj=longlat"))
      sp_click <- spTransform(sp_click, vi_crs())
      sp_click
    } else NULL
  })

  curr_vi_ts <- reactive({
    if (!is.null(curr_click()) & !is.null(vi_datacube)) {
      # TODO: Check if click is within raster extent
      extract(vi_datacube(), curr_click()) %>%
        unname() %>%
        .[1, ]
    } else NULL
  })

  n_seasons <- reactive({
    input$nSeasons
  })

  harm_ts <- reactive({
    if(!is.null(curr_vi_ts()) & !is.null(doy_dates())) {
      harmonics_fun(curr_vi_ts(), doy_dates(), n_seasons())
    }
  })

  curr_ts_df <- reactive({
    if (!is.null(curr_vi_ts()) & !is.null(doy_dates())) {
      data.frame(date=doy_dates(), vi=curr_vi_ts(), vi_harm=harm_ts())
    } else NULL
  })

  curr_plot <- reactive({
    if (!is.null(curr_ts_df())) {
      ggplot(curr_ts_df(), aes(x=date)) +
        geom_line(aes(y=vi, colour = "Original")) +
        geom_line(aes(y=vi_harm, colour = "Harmonic")) +
        scale_colour_manual("",
                            breaks = c("Original", "Harmonic"),
                            values = c("grey", "red"))
    } else NULL
  })

  # observers ---------------------------------------------------------------------------------------------------

  # Add AOI to leaflet map
  observe({
    if (length(aoi_path()) > 0) {
      print(aoi_path())
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
    click <- input$map_click
    leafletProxy('map')%>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat)
    plot(vi_datacube()[[1]])
    plot(curr_click(), add = T)
    print(doy_dates())
    output$fitPlot <- renderPlot(curr_plot())
  })

  observe({print(n_seasons())})
}

shinyApp(ui, server)
