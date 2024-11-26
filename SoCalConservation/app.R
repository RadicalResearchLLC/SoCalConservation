library(shiny)
library(leaflet)
library(bslib)
library(bsicons)
library(tidyverse)
library(sf)
library(lwgeom)


deploy_date <- 'November 25, 2024.'
version <- 'Alpha v0.3 deployed'
data_conservation <- '2024 30x30 Conservation data and Biodiversity index rank from:'
biodiversity_data <- 'https://www.californianature.ca.gov/pages/open-data'
jurisdiction_data <- 'Jurisdiction data from US Census TIGER/Line 2023'

sf_use_s2(FALSE)

# Define UI for application that draws a histogram
ui <- page_sidebar(
  #tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  title = shiny::img(height = 60, src = 'https://www.socalearth.org/wp-content/uploads/2024/07/Bright-Background-Horizontal-1024x520.png'),
  window_title = 'SoCal Conservation',
  sidebar = sidebar(
      width = 400,
      title = 'Inputs',
      selectizeInput(
        inputId = 'Jurisdiction',
        label = 'Select Jurisdiction',
        choices = c(sort(jurisFinal2$NAME)),
        selected = 'Southern California',
        multiple = FALSE
        ),
      actionButton(inputId = 'Reset', label = 'Reset to full extent'),
      sliderInput(inputId = 'Buffer',
                  label = 'Accessible open space distance in miles',
                  min = 0.5, max = 5, 
                  step = 0.5, 
                  value = 1),
      textOutput('conservedSpace'),
      hr(),
      p(version, deploy_date), 
      uiOutput('link'),
      p(jurisdiction_data)
     
  ),
       # Show a plot of the generated distribution
  layout_columns(
    uiOutput('consvdSpace'),
    uiOutput('accessSpace'),
    uiOutput('bioSpace')
    ),
  card(
     shinycssloaders::withSpinner(leafletOutput('map', height =  600))
     )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

 url <- a('30x30 California Open Data',
        href = 'https://www.californianature.ca.gov/pages/open-data')
 
 output$link <- renderUI({
   tagList("30x30 Conservation data and Biodiversity index from:", url)
 })
  
##Basic Map
    output$map <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(provider = providers$Esri.WorldShadedRelief, group = 'Topography') |>
        addProviderTiles(provider = providers$Esri.WorldGrayCanvas, group = 'Basemap') |>
        addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |> 
        setView(lat = 34, lng = -117.30, zoom = 8) |> 
          addLayersControl(baseGroups = c('Topography', 'Basemap', 'Imagery'),
                       overlayGroups = c('Conserved lands',
                                           'Jurisdiction',
                                           'Biodiversity',
                                           'Counties'), 
                         options = layersControlOptions(collapsed = FALSE),
                         position = 'topright'
        ) |> 
        hideGroup(c('Biodiversity', 'Counties')) |>
        addMapPane('Jurisdiction', zIndex = 405)  |>  
        addMapPane('Conserved lands', zIndex = 425) |> 
        addMapPane('Biodiversity', zIndex = 415) |> 
        addMiniMap(position = 'bottomleft')
    })

##Conservation layer
observe({    
  leafletProxy("map", data = conserve_simple)  |> 
    clearGroup(group = 'Conserved lands')  |>
    addPolygons(
      color = 'darkgreen',
      weight = 1,
      fillOpacity = 0.5,
      label = ~UNIT_NAME,
      group = 'Conserved lands',
      options = pathOptions(pane = 'Conserved lands'))
})

##Jurisdiction layer
observe({ 
  leafletProxy('map') |> 
    clearGroup(group = 'Jurisdiction')  |>
      addPolygons(
        data = jurisSelected(),
        color = 'black', 
        weight = 2,
        fillOpacity = 0.3,
        group = 'Jurisdiction',
        options = pathOptions(pane = 'Jurisdiction')) |> 
      addPolylines(
       data = bufferJ(),
       color = 'darkorange',
       weight = 3,
       fillOpacity = 0.5,
       group = 'Jurisdiction') 
  })

##County layer
observe({ 
  leafletProxy('map', data = CA_counties) |>
    clearGroup(group = 'Counties') |> 
      addPolylines(
        color = 'black',
        weight = 1,
        group = 'Counties')
})

##Biodiversity
pal2 <- colorFactor(domain = bio_simpler$SpBioRnkEc,
                    palette = 'Purples')

observe({ 
  leafletProxy('map') |>
    clearGroup(group = 'Biodiversity') |> 
    addPolygons(
      data = bio_simpler,
      color = ~pal2(SpBioRnkEc),
      weight = 0.1,
      fillOpacity = 0.5,
      group = 'Biodiversity',
      options = pathOptions(pane = 'Biodiversity')) |> 
    addLegend(
      data = bio_simpler,
      title = 'Biodiversity Rank',
      pal = pal2,
      values = ~SpBioRnkEc,
      group = 'Biodiversity',
      position = 'bottomright')
})

# choose jurisdiction 
jurisSelected <- reactive({
  #req(input$Jurisdiction) 
  filter(jurisFinal2, NAME == input$Jurisdiction) 
})

### Calculations for three text boxes
# calculate conserved space within jurisdiction
space <- reactive({
  areaJuris <- st_area(jurisSelected()) 
  areaConsvd1<- conserve_simple |> 
    st_intersection(jurisSelected()) #|> 
  
  if(nrow(areaConsvd1) > 0) {
  areaConsvd <- areaConsvd1 |> 
    summarize() |> 
    st_area()
  pct_space <- round(100*areaConsvd/areaJuris, 1)
  } else  {pct_space <- 0}
  return(pct_space)
})

## Calculate buffer polygon
bufferJ <- reactive({
  jurisNAD83 <- st_transform(jurisSelected(), crs = 3310) #albers california
  miles2km <- input$Buffer*1.6093*1000 #convert miles to meters
  buffered <- st_buffer(jurisNAD83, miles2km) |> 
    st_transform(crs = 4326) 
  return(buffered)
})

# calculate accessible space within a distance
bufferAreaOS <- reactive({
  OS1 <- conserve_simple |> 
    st_intersection(bufferJ()) |> 
    summarize() |> 
    st_area()
  area1 <- st_area(bufferJ())
  percent <- round(100*OS1/area1,1)
  return(percent)
})

# calculate fraction of high biodiverse space conserved within jurisdiction

bioPct <- reactive({
  high_bio_poly <- high_bio |> 
    st_intersection(jurisSelected()) # find any high_biodiversity area within jurisdiction

  if(nrow(high_bio_poly) > 0)   #error handling 1
    {
      consvd_high_bio_area1 <- conserve_simple |> 
        st_intersection(jurisSelected())
      if(nrow(consvd_high_bio_area1) > 0) #error handling 2
        {
         high_bio_area <- st_area(high_bio_poly) #calculate area of high biodiversity
         consvd_high_bio_area2 <- consvd_high_bio_area1 |> 
           summarize() |> 
           st_intersection(high_bio_poly)  
        }
        if(nrow(consvd_high_bio_area2) > 0) {
          consvd_high_bio_area <- consvd_high_bio_area2 |> 
            st_area()
          percentBio <- round(100*consvd_high_bio_area/high_bio_area,1)
          } else {percentBio <- 0}
      }
  else {percentBio <- 0}
    
  return(percentBio)
  })

output$consvdSpace <- renderUI({
  bslib::value_box(
    title = 'Conserved Open Space',
    value = str_c(space(), '%'),
    showcase = bs_icon('pie-chart'),
  theme = 'yellow',
  p('Land conserved as open space'),
  p('Calculated as % of total land area')
  )
})

#Accessible space textbox
output$accessSpace <- renderUI({
  bslib::value_box(
    title = 'Accessible Open Space',
    value = str_c(bufferAreaOS(), '%'),
    showcase = bs_icon('bicycle'),
    theme = 'orange',
    p('Includes area within ', input$Buffer, ' mile(s)'),
    p('Calculated as % of total area')
    )
})

#Biodiversity space textbox

output$bioSpace <- renderUI({
  bslib::value_box(
    title = 'High Biodiversity Open Space %',
    value = str_c(bioPct(), '%'),
    showcase = bs_icon('tree'),
    theme = 'purple',
    p('Percent of high biodiversity area'),
    p('Calculated as % of high biodiversity area')
  )
})

#create bounding box for map zoom
bbox <- reactive({
  bounds  <- as.numeric(st_bbox(jurisSelected()))
})

#observe extent of polygons if available
observe({
  leafletProxy('map', data = bbox()) |> 
    fitBounds(
      lng1 = bbox()[1], 
      lat1 = bbox()[2], 
      lng2 = bbox()[3], 
      lat2 = bbox()[4]
    )
})

observeEvent(input$Reset, {
  updateSelectizeInput(session, 
    "Jurisdiction",
    label = 'Select Jurisdiction',
    choices = c(jurisFinal2$NAME),
    selected = 'Southern California')
}
)

}
# Run the application 
shinyApp(ui = ui, server = server)
