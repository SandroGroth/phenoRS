library(shiny)
library(shinyFiles)
library(leaflet)
library(rgdal)
library(sf)
library(raster)

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
          #shinyFilesButton('viFileChoose', label = 'Load VI', title = 'Select VI Files', multiple = TRUE)
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
      "SETTINGS"
    )
  )
)

# Server ========================================================================================================

server <- function(input, output, session) {

  # init elements -----------------------------------------------------------------------------------------------

  volumes <- getVolumes()

  # AOI File selection
  shinyFileChoose(input, 'aoiFileChoose', roots = volumes, filetypes = c('', 'shp'))
  output$aoiPathOutput <- renderText(basename(unname(parseFilePaths(volumes, input$aoiFileChoose)$datapath)))

  # VI FIles selection
  shinyDirChoose(input, 'prepDirChoose', roots = volumes(), filetypes = c('', 'envi', 'tif'), session = session,
                 restrictions = system.file(package = 'base'))
  #shinyFileChoose(input, 'viFileChoose', roots = volumes, filetypes = c('', 'envi', 'tif'))
  output$prepDirOutput <- renderText(unname(parseDirPath(volumes, input$prepDirChoose)))

  # Initial Map View
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(noWrap = TRUE)) %>%
      setView(0, 0, zoom = 1)
  })

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

  vi_selected <- reactive({
    if (!is.null(vi_datacube)) {
      input$displayViSelect
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
          fill = FALSE
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

}

shinyApp(ui, server)
