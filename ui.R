library(shiny)
library("DT")
library("shinyBS")
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library("openxlsx")
library(mapview)
library(leaflet)

dir <- getwd()
print(dir)

CHVI_df <- read.csv(paste0(dir, "/data/selectedCHVIdata.csv"))
print(CHVI_df)
print(colnames(CHVI_df))
counties <- unique(CHVI_df$County)
definition <- unique(CHVI_df$Definition)

load(paste0(dir, "/data/merged_map.rda"))
indicator_choices <- colnames(df_merge)
indicator_choices <- indicator_choices[16:503]

# 
# CES_df <- read.csv("/Users/ewanwhittaker-walker/Rshiny/calenviroscreen40resultsdatadictionary_F_2021.xlsx")
# print(CES_df)
# print(colnames(CES_df))
# #counties <- unique(CES_df$County)
# 
# ROI_df <- read.csv("/Users/ewanwhittaker-walker/Rshiny/ROI_downloadable_data_2014.xlsx")
# print(CES_df)
# print(colnames(CES_df))
#counties <- unique(df$County)

shinyUI(navbarPage(
  theme = shinythemes::shinytheme("flatly"),  # <--- To use a theme, uncomment this
  "Farmworkers Health App",
  tabPanel("Public Data", 
           # sidebarPanel(
           #   # Input: Select a dataset ----
           #   # tags$style(type='text/css', ".selectize-input { font-size: 13px;} .selectize-dropdown { font-size: 13px; line-height: 14px; }"),
           #   # selectInput("cohort", "Select a cohort:",
           #   #             choices = c(4, 3, 2, 1, 0)),
           #   selectInput("county", "select a county:",
           #               choices = counties), 
           #   width = 2 ### EDIT HERE
           # ),
           conditionalPanel("input.tabselect==2",
                            sidebarPanel(selectInput("county", 
                                                     "select a county:", 
                                                     choices = counties))
           ),
           conditionalPanel("input.tabselect==1",
                            sidebarPanel(selectInput("indicator", 
                                                     "select a indicator:",
                                                     choices = indicator_choices))
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Map", value = 1,
                        leafletOutput("map") 
                        #mapview:::plainViewOutput("test"),
               ),
               tabPanel("Data", value = 2,
                        # tags$head(tags$style("#tbl1 {white-space: nowrap;}")),
                        # tags$head(tags$style(".modal-dialog{ width:1000px}")),
                        tags$head(tags$style("table.dataTable thead th {
                                      padding: 8px 10px !important;}")),
                        DT::dataTableOutput('tbl1')
                        # bsModal("geneModal", "Beeswarm", "",plotOutput('gPlot'), size = "large")
               ),
               id = "tabselect")
             
             # tabPanel("Completetion Bar Chart", 
             #  plotOutput("Hist"))
           )
           )
))
