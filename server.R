library(shiny)
library(sf)
library(ggplot2)
library(viridis)
library(leaflet)
library(stringr)


## github toe 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

dir <- getwd()
print(dir)


## loading merged dataframe for map
load(paste0(dir, "/data/merged_map.rda"))
load(paste0(dir, "/data/merged_map_pop.rda"))
## load df long for data tab 
load(paste0(dir, "/data/merged_map_long.rda"))
# load(paste0(dir, "/data/merged_map_spanish.rda"))
# load(paste0(dir, "/data/merged_map_pop_spanish.rda"))
## load df long for data tab 
#load(paste0(dir, "/data/merged_map_long_spanish.rda"))
df_indicator_choices <- read.csv(paste0(dir, "/data/Indicators_Farmworker_WebApplication.csv"))

dict_english_to_spanish <- c()

for (i in 1:nrow(df_indicator_choices)) {
  row <- df_indicator_choices[i,]
  eng_inc <- row$indicator
  span_inc <- row$spanish_translation
  dict_english_to_spanish[span_inc] = eng_inc
}

print(dict_english_to_spanish["Calidad del aire"])
map_base <- st_transform(df_merge, 4326)

#server_indicator <- "Air_Quality"
# print(head(map))
# print("head map")

source("global.R") 


shinyServer(function(input, output, session) {
  
  server_indicator <- reactiveVal("Air_Quality")
  
  observeEvent(input$language, {
  if (input$language == "English") {
    print("English")
    
    # df_indicator_choices <- df_indicator_choices[df_indicator_choices$category == paste0(input$category),]
    # print(df_indicator_choices)
    # indicator_choices <- df_indicator_choices$indicator
    # print(indicator_choices)
    
    indicator_choices <- df_indicator_choices$indicator 
    category_choices <- df_indicator_choices$category
    
    updateSelectInput(session, "indicator",
                      label = "indicator",
                      choices = indicator_choices, 
                      selected = "Air_Quality")
    
    updateSelectInput(session, "category",
                      label = "category",
                      choices = category_choices, 
                      selected = "Air")
    
    
  } else if (input$language == "Spanish"){
    indicator_choices <- df_indicator_choices$spanish_translation
    category_choices <- df_indicator_choices$spanish_category
    
    updateSelectInput(session, "indicator",
                      label = "indicator",
                      choices = indicator_choices, 
                      selected = "Calidad del aire")
    
    updateSelectInput(session, "category",
                      label = "category",
                      choices = category_choices, 
                      selected = "Aire")
    # df_merge <- df_merge_spanish
    # map_base <- st_transform(df_merge_spanish, 4326)
    # #df_merge_long <- df_merge_long_spanish
    # df_merge_pop <- df_merge_pop_spanish
    # map_react()
    #print(map_base[[paste(indicator_choices[10])]])
    #print(map_base$GEOID)
  }  
  }
  )
  
  observeEvent(input$category, {
    if (input$language == "English") {
    
    df_indicator_choices <- df_indicator_choices[df_indicator_choices$category == paste0(input$category),]
    indicator_choices <- df_indicator_choices$indicator
    
    updateSelectInput(session, "indicator",
                      label = "indicator",
                      choices = indicator_choices)
  } else if (input$language == "Spanish") {
    
    df_indicator_choices <- df_indicator_choices[df_indicator_choices$spanish_category == paste0(input$category),]
    indicator_choices <- df_indicator_choices$spanish_translation
    
    updateSelectInput(session, "indicator",
                      label = "indicator",
                      choices = indicator_choices)
  }
  })
  
  observeEvent(input$indicator, {
    if (input$language == "English") {
      server_indicator(paste0(input$indicator))
    } else if (input$language == "Spanish"){
      server_indicator(dict_english_to_spanish[paste0(input$indicator)])
      
    }
  })
  
  dataset <- reactive({
    
    df <- df_merge_long
    
    # grep user input on table
    if (length(input$indicator) != 0) {
      grep_query = paste0("^", server_indicator(), "$")
      print(grep_query)
      print(input$indicator)
      df <- dplyr::filter(df, grepl(grep_query, indication))
    }
  })
  

  
  output$tbl1 <- DT::renderDataTable(server = FALSE, {
    
    ## dropping unnecesary columns
    data <- dataset()
    
    ## inputs for making a nice looking table
    datatable(
      data, rownames= FALSE, extensions = 'Buttons', escape = FALSE,
      options = list(pageLength = 50, autoWidth = FALSE, 
                     dom = 'lBfrtip',
                     buttons = list('copy', list(
                       extend = 'collection',
                       buttons = list(
                         list(extend = 'csv', filename = paste0('filename')),
                         list(extend = 'excel', filename = paste0('filename'))),
                       text = 'Download')),
                     lengthMenu = list(c(10, 20, 50, 100), 
                                       c('10', '20', '50', '100')),
                     columnDefs = list(
                       list(width = '70px',                        
                            targets = c(),
                            render = JS(
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data.length > 6 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                              "}")
                       )),
                     scrollX = TRUE, fixedColumns = list(leftColumns = 1),
                     # list(className = 'dt-right', targets = 6:7)
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'font-size': '100%'});",
                       "}"),
                     rowCallback = JS(
                       "function(row, data) {",
                       "for (i = 10; i < 10; i++) {",
                       "if (data[i]<10000 && data[i] != null) {",
                       "$('td:eq('+i+')', row).html(data[i].toFixed(2));",
                       "}",
                       "}",
                       "}"))
      
    )})
    # %>% formatStyle(columns = c(1,2,3,4,5,6,7,8,9, 10, 11, 12  , 13, 14, 15,16, 17, 18), fontSize = '90%')
  

  # map <- st_read("/Users/ewanwhittaker-walker/Rshiny/tl_2019_06_tract/tl_2019_06_tract.shp", quiet = TRUE)
  # head(map)
  

  ## mapview code 
  # map_plot <- mapview(map, zcol = "TRACTCE")
  # output$map <- renderLeaflet({
  #   map_plot@map
  # })
  
  ## leaflet code 
  # observe({print(input$indicator)})
  
  # indicator <- reactive({paste0(input$indicator)})
  # observe({print(indicator())})
  map_react <- reactive({
    map_base$col_to_show <- map_base[[paste(server_indicator())]]
    print("in obs")
    print(head(map_base$col_to_show))
    map_base
    })
  #pal <- observe({colorNumeric("YlOrRd", domain = map$voting)})
  # map$col_to_show <- map[["voting"]]

  ## rendering the map 
  print("rendered base map")
  output$map <-
    renderLeaflet({
      leaflet(map_base, options = leafletOptions(minZoom = 6)) %>%
      addTiles() %>% 
      setView(lng = -120.00000, lat = 37.00000, zoom = 6.45)  %>% 
      
      setMaxBounds( lng1 =  -125.00000
                    , lat1 = 30.00000
                    , lng2 =  -110.00000
                    , lat2 = 45.00000 )%>%  
      addPolygons(
        weight = 0.2,
        color = NA, 
        # fillColor = ~ pal(col_to_show),
        #fillColor = ~ pal(indicator),
        fillColor = NA,
        fillOpacity = 0, 
        dashArray = "3",
        layerId = map_base$GEOID,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          # fillOpacity = 0,
          bringToFront = FALSE)
      )
      })
    
  observe({
    # map1 <- map_react()
    print("made pal")
    pal <- colorNumeric("YlOrRd", domain = map_base[[paste(server_indicator())]], na.color = NA)
    # leafletProxy("map", data = map) %>% clearShapes() %>%  
    #   addPolygons(
    #     weight = 1,
    #     color = "white", 
    #     fillColor = ~ pal(col_to_show),
    #     #fillColor = ~ pal(indicator),
    #     fillOpacity = 1, 
    #     dashArray = "3",
    #     layerId = map$GEOID, 
    #     highlightOptions = highlightOptions(
    #       weight = 5,
    #       color = "#666",
    #       dashArray = "",
    #       fillOpacity = 0.5,
    #       bringToFront = TRUE)
    #   ) %>% 
    #print(colnames(map_base))
    
    #print(map_base[[paste(input$indicator)]])
    #print(head(pal( map_base[[paste(input$indicator)]])))
    leafletProxy("map", data = map_base) %>% 
        setShapeStyle(layerId = map_base$GEOID, 
                      fillColor = pal( map_base[[paste(server_indicator())]]), 
                      color = "#666", 
                      fillOpacity = 0.8) %>%  
        clearControls() %>%
        addLegend(pal = pal, 
                values = map_base[[paste(server_indicator())]], 
                opacity = 1, 
                title = input$indicator) 
      print("updated map")
    })
  
  ## showing popups 
  showPopup <- function(id, lat, lng) {
    
    census <- str_sub(id,-6,-1)
    county <- str_sub(id,2,5)
    
    print(as.integer(county))
    
    pop_dataset <- reactive({
      
      df <- df_merge_pop
      
      # grep user input on table
      if (length(input$indicator) != 0) {
        # grep_query = paste(input$indicator, collapse = "|")
        # df <- dplyr::filter(df, grepl(grep_query, Definition))
        df <- df[,c("County", "GEOID", paste(server_indicator()))]
      }
      print(colnames(df))
      df <- dplyr::filter(df, grepl(paste0(id), GEOID))
    })
    
    popup_df <- pop_dataset()
    # print(popup_df)
    
    # print(head(popup_df))
    # 
    # print(as.numeric(unlist(popup_df[1,"Mean"])))
    # print(toString(unlist(popup_df[1,"Year"])))
    # 

    indic <- paste0(server_indicator())
    
    print(paste("County: ", unlist(popup_df[1,"County"][1])))
    print("sep")
    print(indic)
    print("sep")
    print(as.numeric(unlist(popup_df[1,indic])))
    
    content <- as.character(tagList(
      tags$h4(paste("County: ", unlist(popup_df[1,"County"][1])[1])),
      tags$strong(HTML(
        sprintf("Selected Indication: %s", paste(input$indicator))), tags$br(),
      # sprintf("latitude: %0.5f", as.integer(lat)), tags$br(),
      # sprintf("longitude: %0.5f", as.integer(lng)), tags$br(),
       sprintf("Value: %0.2f", as.numeric(unlist(popup_df[1,indic])[1])), tags$br(), 
       #sprintf("Year: %s", toString(popup_df[1,"Year"])), tags$br(),
      #sprintf("More info?") 
    )))
    leafletProxy("map") %>% 
      addPopups(lng, lat, content) 
    
    
  }
  
  ## observing clics 
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event)) {
      print('null event')
      print(input$map_shape_click)
      
    } else {
      print('populated event')
      print(input$map_shape_click)
      isolate({
        showPopup(event$id, event$lat, event$lng)
      })
    }
  })
  
  # # querying data based on observed clics 
  # observe({
  #   event <- input$map_shape_click
  #   
  #   if (is.null(event)) {
  #     print('null event')
  #     print(input$map_shape_click)
  #     
  #   } else {
  #     leafletProxy("map") %>% clearShapes()
  #     
  #     print('populated event')
  #     print(input$map_shape_click)
  #     isolate({
  #       
  #     })
  #   }
  # })

  ## air quality index widget
  
  source_aqi <- reactiveVal(paste0("https://widget.airnow.gov/aq-dial-widget/?latitude=38.068501031272&longitude=-120.30029296875"))
  
  observe({ 
    event <- input$map_shape_click
    latitude <- as.character(event$lat)
    longitude <- as.character(event$lng)
    print(latitude)
    print(longitude)    
    source_aqi(paste0("https://widget.airnow.gov/aq-dial-widget/?latitude=", latitude, "&longitude=", longitude))
    
  })
  
  output$frame <- renderUI({
    my_test <- tags$iframe(src=paste0(source_aqi()), height=340, width=240)
    print(my_test)
    my_test
  })
    

    
    
    
    # isolate({
    #   showZipcodePopup(event$id, event$lat, event$lng)
    # })
  
})