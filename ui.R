#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#   https://rstudio.github.io/shinydashboard/get_started.html
#

# install.packages("shinydashboard")
# install.packages("openxlsx")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggmap")
# install.packages("data.table")
# install.packages("plotly")
# install.packages("leaflet")
# install.packages("htmltools")
# install.packages("RColorBrewer")
# install.packages("threejs")
# install.packages("DT")
# install.packages("shiny")

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Chimbu Account Reporting"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Market Analysis", tabName = "widgets", icon = icon("balance-scale")),
        menuItem("Overall by Companies", tabName = "DataOverallCompanies", icon = icon("bars")),
        # menuItem("Price Analysis", tabName = "OppAnalysis", icon = icon("inr")),
        menuItem("Data View", tabName = "DataView", icon = icon("th")),
        menuItem("Elevator Volumes (ALL)", tabName = "ElevVolumes", icon = icon("map-marker")),
        menuItem("Elevator Analysis", tabName = "ElevAnalysis", icon = icon("map-marker")),
        menuItem("Price Analysis", tabName = "priceAnalysis", icon = icon("inr")),
        menuItem("TOP SELLER", tabName = "UnitAnalysis", icon = icon("bar-chart")),
        menuItem("Missing Data", tabName = "missingSummary", icon = icon("warning"))
       # menuItem("Closed opportunities", tabName = "widgets2", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        #### DF summary ########
        # tabItem(tabName = "missingSummary",
        #         fluidRow(
        #           box(title = "Missing Data Summary (Closed opportunities)",width=12,status = "warning", solidHeader = TRUE,collapsible = FALSE,collapsed = FALSE,
        #               dataTableOutput("tabmissingSummary"))
        #         )
        #       ),
        # First tab content
        # tabItem(tabName = "dashboard",
        #         fluidRow(
        #           selectInput(inputId = "MarSegChoose","Market Segment",ClosedMarketSeg,multiple = FALSE),
        #          valueBoxOutput("TotalOpp",width=6)
        #          # ,
        #          # valueBoxOutput("ClosedSuccess"),
        #          
        #         # valueBoxOutput("ClosedSuccessPer")
        #          ),#fluidrow ends
        #         fluidRow(
        #           box(plotlyOutput("DashSuccessChart", height = 350),status = "success"),
        #           box(plotlyOutput("TopCompetitor", height = 350),status = "warning")
        #         )#Fluid row ends
        # ),
        tabItem(tabName = "DataView",
                fluidRow(
                  box(title = "Overall",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      dataTableOutput("tabOverAllCompanies"))
                )
              ),
        # Second tab content
        tabItem(tabName = "DataOverallCompanies",
                fluidRow(
                  box(title = "Overall",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      dataTableOutput("tabOverAllMarket"))
        )# Third tab content ends
        ),
        
        #### DF summary ########
        tabItem(tabName = "missingSummary",
                fluidRow(
                  box(title = "Missing Data Summary",width=12,status = "warning", solidHeader = TRUE,collapsible = FALSE,collapsed = FALSE,
                      dataTableOutput("tabmissingSummary"))
                )
               )
        
        )
      )#dashboard Body Close
  )#dashboard page close
)
