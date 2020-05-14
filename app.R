# Title: Bridging Divides Map Project - Shiny Map for Borja Sample Data (Single Layer - No Resource Data)
# Author: Carolina S. Roe-Raymond (Altered by Autumn Lewien)
# Initial Project Date: May 11
# Last Edited: May 13

## Variable conventions
## Variables that start with...mean...
# d - data
# i - input
# o - output
# r - reactive

# for data filtering
library(DT)
library(dplyr)
library(geojsonsf)
library(rsconnect) # to deploy app on shinyapps.io
library(sf)
library(shiny)
library(stringr)
library(tidyr)

# for leaflet map
library (rgdal)
library (geojsonsf)
library (htmltools)
library (leaflet.extras)

setwd("~/Desktop/R Directory/AAPI Hate Incidents")

## ------------ ## BORJA DATA ## ------------ ##

d_borja<-read.csv("Borja Sample - Cleaned with lat and long [May 12, 2020].csv", 
                  header=TRUE,
                  na.strings=c("", "NA")) ## Replace all non-standard na's with "NA"
d_borja_df <- as.data.frame(d_borja)
## Create geometry column (based off of latitude and longitude columns)
d_borja_df<-st_as_sf(d_borja_df, coords = c("longitude", "latitude"), crs = 4326, sf_column_name = "geometry")
d_borja_df_tidy <- d_borja_df %>% 
  separate_rows(EventCodes, sep = "\\|") 

data_test<-st_sf(d_borja_df_tidy)

l_code <- levels(as.factor(d_borja_df_tidy$EventCodes))


## ---------------- ## UI ## ---------------- ##
ui <- fluidPage(
  fluidRow(column(width=12,
                  tags$h1("Example Map", style="font-family:Georgia; font-size:50px;"),
                  style= "color:White; background-color:Black; margin-left:25px;")),
  leafletOutput("o_map", height=600, width="100%"),
  br(),
  tags$h2("Filter Data"),
  br(),
  fluidRow(column(width=1,
                  offset=0.5,
                  actionButton("i_reset", label="Reset"))
  ),
  br(),
  fluidRow(column(width=5,
                  offset=0.5,
                  # Code
                  selectInput("i_code",
                              label = "Event Code",
                              choices = c(l_code),
                              selected = NULL,
                              multiple = TRUE,
                              selectize = FALSE,
                              width = "100%"))
  ),
  tags$h2("View/Download Data"),
  tabPanel("Borja Data",
           br(),
           # Download Button
           fluidRow(column(width = 1, 
                           #offset = 9,
                           downloadButton("o_data_borja_download", 
                                          label = "Download Data"))),
           br(),
           # Data Table
           fluidRow(column(DT::dataTableOutput("o_data_borja_selected"), width=12)
           )
  )
)


## ------------- ## SERVER ## ------------- ##

server <- function(input, output, session) {
  addLegendCustom <- function(map, colors, labels, sizes = 5, shapes = c("circle"), borders, group, position = c("topright","bottomright", "bottomleft","topleft"), opacity = 0.5){
    make_shapes <- function(colors, sizes, borders, shapes) {
      shapes <- gsub("circle", "50%", shapes)
      shapes <- gsub("square", "0%", shapes)
      paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
    }
    make_labels <- function(sizes, labels) {
      paste0("<div style='display: inline-block;height: ", 
             sizes, "px;margin-top: 4px;line-height: ", 
             sizes, "px;'>", labels, "</div>")
    }
    legend_colors <- make_shapes(colors, sizes, borders, shapes)
    legend_labels <- make_labels(sizes, labels)
    return(addLegend(map, colors = legend_colors, labels = legend_labels, position = "bottomleft", opacity = opacity, group = group))
  }
  r_filteredborja<- reactive({
    # if none of the inputs have been selected, show full data set
    if(!isTruthy(input$i_code)){
      d_borja_df_tidy
    } else {
      # otherwise, filter any data that has input (i.e. not null)
      r_filteredborja <- d_borja_df_tidy 
      if (is.null(input$i_code) == FALSE) {
        r_filteredborja <- r_filteredborja %>% filter(EventCodes %in% input$i_code)
        }
      r_filteredborja
    }
  })
  observeEvent(input$i_reset, {
    updateSelectInput(session,
                      "i_code",
                      label = "Event Code:", 
                      choices = c(l_code), 
                      selected = NULL)
    r_filteredborja() # call reactive variable so that it updates
  }) 
  output$o_map <- renderLeaflet({
    bins <- c(0, 30, 100, 500, 1000, 1500, 2500, 3500, 5500, Inf)
    pal <- colorBin("YlOrBr",  bins = bins)
    leaflet() %>%
      setView(-96, 37.8, 4) %>%
      setMapWidgetStyle(list(background="white")) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(minZoom = 2, maxZoom = 20, opacity = 0.5)) %>%
      # Overlay groups
      addTiles(urlTemplate = "http://www.justicemap.org/tile/county/density/{z}/{x}/{y}.png",
               group = "Population Density",
               attribution = paste("<b><a href='https://www.justicemap.org'>JusticeMap.org</a></b>"),
               options = tileOptions(minZoom = 2, maxZoom = 20, opacity = 0.4))%>%
      addTiles(urlTemplate = "http://www.justicemap.org/tile/tract/density/{z}/{x}/{y}.png",
               group = "Population Density",
               attribution = paste("<b><a href='https://www.justicemap.org'>JusticeMap.org</a></b>"),
               options = tileOptions(opacity = 0.4,
                                     minZoom = 10, maxZoom = 20))%>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Borja Data","Population Density"),
        options = layersControlOptions(collapsed = FALSE, position = "bottomright")
      )%>%
      # Legends
      addLegend(
        pal = pal,
        opacity = 0.4,
        values = bins,
        title = "Population per Sq Mi",
        position = "bottomleft",
        group = "Population Density"
      ) %>%
      addLegendCustom(
        opacity = 0.5,
        colors = "blue",
        borders = "blue",
        labels = "Borja",
        group = "Borja Event Data"
      ) %>%
      hideGroup("Population Density")
  }) # end of renderLeaflet
  # Add the feature that changes dynamically
  observe({
    # Create needed data set:
    # select only the relevant columns of data,
    # remove all duplicate rows (important, so that each dot truly only represent one record), and
    # convert data frame back to class sf
    d_borja_single_records_sf <- r_filteredborja() %>%
      select(Headline, Date, Location.New, EventCodes, geometry) %>%
      distinct() %>%
      st_sf()
    # Add dots to map:
    leafletProxy("o_map", session) %>%
      clearMarkers() %>%
      addCircleMarkers(
        data=d_borja_single_records_sf,
        fillColor="blue",
        fillOpacity = 0.5,
        radius = 5,
        color = "blue",
        weight = 0.5,
        group = "Borja Data",
        popup = paste("<b>",d_borja_df_tidy$Date,"</b>",d_borja_df_tidy$EventCodes))
  })
  ## Data Table
  output$o_data_borja_selected <- DT::renderDataTable({
    r_filteredborja_show <- r_filteredborja() %>%
      select(Headline, Date, Source, Location.New, EventCodes)
    r_filteredborja_show
  })
  
  # Download Buttons
  output$o_data_borja_download <- downloadHandler(
    filename = function() {
      paste('sample-borja-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      # select only columns that we want to be shown
      r_filteredborja_download <- r_filteredborja () %>% 
        select(Headline, Date, Source, Location.New, EventCodes) %>%
        write.csv(r_filteredborja_download, con)
    }
  )
}


## ------------- ## SHINY APP ## ------------- ##

shinyApp(ui = ui, server = server)


