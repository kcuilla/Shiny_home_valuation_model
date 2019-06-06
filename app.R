library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(data.table)
library(V8)

# File used for datset ---
results_file <- "U:/Profile/Documents/Shiny Apps/Shiny App/data/Best_Model_Results.csv"
home_final <- read.csv(results_file)

# Code to sample dataset for 5000 entries and create valuation levels based on results ---
# home_final <- sample_n(home_final,5000) %>% 
# mutate(PREDICTED_PRICE = round(PREDICTED_PRICE), PERCENT_DIFF = round(((SALE_PRICE/PREDICTED_PRICE)-1)*100,digits=2)) %>% 
# mutate(ACCURACY = ifelse(PERCENT_DIFF < -1,"Undervalued", ifelse(PERCENT_DIFF > 1,"Overvalued","Expected Value"))) %>% 
# mutate(Radius = (abs(PERCENT_DIFF)+7.5)*25)

home_final$ACCURACY <- factor(home_final$ACCURACY, levels = c("Undervalued","Overvalued","Expected Value"))

jscode <- "shinyjs.refresh = function() { history.go(0); }"

# Dashboard appearance --- 
ui <- dashboardPage(skin="blue",
                    dashboardHeader(title="Home Valuation Model",titleWidth=250),
                    dashboardSidebar(width=250,
                                     sidebarMenu(
                                       br(),
                                       # Tab options ---
                                       menuItem(tags$em("Interactive Map",style="font-size:150%"),icon=icon("map-marked-alt"),tabName="maptab"),
                                       menuItem(tags$em("Property Distributions",style="font-size:150%"),icon=icon("bar-chart-o"),tabName="charttab"),
                                       menuItem(tags$em("Valuation Charts",style="font-size:150%"),icon=icon("signal"),tabName="accuracytab"),
                                       menuItem(tags$em("About",style="font-size:150%"),icon=icon("file"),tabName="abouttab"),
                                       # Filter options ---
                                       checkboxGroupInput("accuracy", label = "Valuation", choices = c("Undervalued","Overvalued","Expected Value"), selected = c("Undervalued","Overvalued","Expected Value")),
                                       actionButton("toggle", "Show/Hide Location Filters"),
                                       conditionalPanel(
                                         condition = "input.toggle % 2 == 1",
                                         checkboxGroupInput("counties", label = "County", choices = c("Los Angeles County","Orange County","Ventura County"), selected = c("Los Angeles County","Orange County","Ventura County"))),
                                       actionButton("toggle2", "Show/Hide Property Filters"),
                                       conditionalPanel(
                                         condition = "input.toggle2 % 2 == 1",
                                         sliderInput("saleprice", label = "Price Sold", min = min(home_final$SALE_PRICE), max = max(home_final$SALE_PRICE), value = range(home_final$SALE_PRICE), step = 50000),
                                         sliderInput("rooms", label = "Rooms", min = min(home_final$COUNT_ROOMS), max = max(home_final$COUNT_ROOMS), value = range(home_final$COUNT_ROOMS), step = 1),
                                         sliderInput("bathrooms", label = "Bathrooms", min = min(home_final$COUNT_BATHROOM), max = max(home_final$COUNT_BATHROOM), value = range(home_final$COUNT_BATHROOM), step = 1),
                                         sliderInput("squarefeet", label = "Sqft", min = min(home_final$AREA_TOTAL_CALC), max = max(home_final$AREA_TOTAL_CALC), value = range(home_final$AREA_TOTAL_CALC), step = 250),
                                         selectizeInput("yearbuilt", label = "Decade Built", multiple = TRUE, choices = c("1881-1890","1891-1900","1901-1910","1911-1920","1921-1930","1931-1940","1941-1950","1951-1960","1961-1970","1971-1980","1981-1990","1991-2000","2001-2010","2011-2020","Unknown"), selected = c("1881-1890","1891-1900","1901-1910","1911-1920","1921-1930","1931-1940","1941-1950","1951-1960","1961-1970","1971-1980","1981-1990","1991-2000","2001-2010","2011-2020","Unknown"))),
                                       shinyjs::useShinyjs(),
                                       shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                                       actionButton("refresh","Reset Map and Filters")
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        # Interactive map output ---
                        tabItem(tabName="maptab",
                                fluidRow(
                                  column(width = 9,
                                         solidHeader = TRUE,
                                         leafletOutput("map", height = "800px")),
                                  column(width = 3,
                                         h3("Map Info"), HTML("<ul><li>Each bubble on the map represents a property that was sold in 2017</li>
                                                                  <li>Bubble size represents the percent difference between the estimated value of the property and the actual sale price</li>
                                                                  <li>Click on the bubbles to get summary info about each property</li>
                                                                  <li>Additional filter controls are provided on the side panel. These filters are applied across all tabs</li></ul>"
                                         ),
                                         br(),
                                         plotOutput("propertyhistogram",height=350))
                                )),
                        # Property Distributions tab output ---
                        tabItem(tabName="charttab",
                                fluidRow(
                                  plotlyOutput("salepriceplot"),
                                  plotlyOutput("roomplot"),
                                  plotlyOutput("sqftplot"),
                                  plotlyOutput("bathroomplot"),
                                  plotlyOutput("builddecadeplot"))),
                        # Valuation Charts tab output ---
                        tabItem(tabName="accuracytab",
                                fluidRow(
                                  plotlyOutput("countyaccuracyplot"),
                                  plotlyOutput("salepriceaccuracyplot"),
                                  plotlyOutput("countroomsaccuracyplot"),
                                  plotlyOutput("countbathroomsaccuracyplot"),
                                  plotlyOutput("builddecadeaccuracyplot"))),
                        # About tab output ---
                        tabItem(tabName="abouttab",
                                fluidRow(
                                  column(width = 6,
                                  h1("Home Valuation Model"),
                                  br(),
                                  h5("The purpose of this Shiny app is to show the distribution of undervalued and overvalued homes in southern California based on their actual sale price vs their estimated sale price.",
                                     "The filters can be used to help determine what features of a property or location led to a home to be sold for more or less of it's estimated value.",
                                     "Details about the dataset and how the estimated sale price of each home was determined can be seen below:"),
                                  br(),
                                  HTML("<ul><li><b>Undervalued</b> = home sold for less than it's estimated value</li>
                                    <li><b>Overvalued</b> = home sold for more than it's estimated value</li>
                                    <li><b>Expected value</b> = home sold within +/- 1% of it's estimated value</li>
                                    <li>The estimated value of each home was calculated by a Neural Network model that was built in R and trained on the dataset</li>
                                    <li>The results shown in this app are 5,000 randomly selected properties that sold for less than $1M</li></ul>"),
                                  br(),
                                  p(em("The data is publicly available through Zillow as part of the Zillow Zestimate competition hosted by Kaggle:")),
                                  a(href="https://www.kaggle.com/c/zillow-prize-1/data","https://www.kaggle.com/c/zillow-prize-1/data")
                                )))
                            ))
                      )

server <- shinyServer(function(input, output) {
  
  # Filtered dataset based on user inputs ---
  filteredData <- reactive({
    home_final[
      home_final$COUNTY_NAME %in% input$counties
      & home_final$ACCURACY %in% input$accuracy
      & home_final$AREA_TOTAL_CALC >= input$squarefeet[1] & home_final$AREA_TOTAL_CALC <= input$squarefeet[2]
      & home_final$COUNT_ROOMS >= input$rooms[1] & home_final$COUNT_ROOMS <= input$rooms[2]
      & home_final$SALE_PRICE >= input$saleprice[1] & home_final$SALE_PRICE <= input$saleprice[2]
      & home_final$BUILD_DECADE %in% input$yearbuilt
      & home_final$COUNT_BATHROOM >= input$bathrooms[1] & home_final$COUNT_BATHROOM <= input$bathrooms[2],]
  })

  # Colors used for map labels ---
  pal <- colorFactor(c("darkgreen","red","blue"), domain = c("Overvalued","Undervalued","Expected Value"))
  
  # Base map details ---
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Hydda.RoadsAndLabels,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Hydda.Base,
                       options = providerTileOptions(noWrap = TRUE, opacity = 0.40)) %>%
      setView(lng = -118.25, lat = 33.98, zoom = 9.5)
  })
  
  # Bubbles for map ---
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(~LONGITUDE, ~LATITUDE, layerId=~PARCEL_ID, radius=~Radius, fillColor = ~pal(ACCURACY), fillOpacity = 0.4, 
                 stroke=FALSE) 
  })
  
  # Legend for map ---
  observe({
    proxy <- leafletProxy("map", data = home_final)
    
    proxy %>% clearControls() %>% addLegend(position = "topright", pal=pal, title = "Valuation", values=~ACCURACY)
  })
  
  observeEvent(input$refresh, {
    shinyjs::js$refresh() 
  })
  
  # Clickable info shown on map ---
  showhouseinfo <- function(PARCEL_ID, LATITUDE, LONGITUDE) {
    selectedPARCEL_ID <- home_final[home_final$PARCEL_ID == PARCEL_ID,]
    content <- as.character(tagList(
      tags$h4(selectedPARCEL_ID$ACCURACY,"(",percent(selectedPARCEL_ID$PERCENT_DIFF/100, accuracy=0.1),")"),
      tags$strong(HTML(sprintf("Sold: %s", dollar(selectedPARCEL_ID$SALE_PRICE)
      ))), tags$br(),
      tags$strong(HTML(sprintf("Estimated: %s", dollar(selectedPARCEL_ID$PREDICTED_PRICE)
      ))), tags$br(),
      sprintf("City: %s", selectedPARCEL_ID$CITY_NAME), tags$br(),
      sprintf("County: %s", selectedPARCEL_ID$COUNTY_NAME), tags$br(),
      sprintf("Rooms: %s", selectedPARCEL_ID$COUNT_ROOMS), tags$br(),
      sprintf("Bathrooms: %s", selectedPARCEL_ID$COUNT_BATHROOM), tags$br(),
      sprintf("Sqft: %s", comma(selectedPARCEL_ID$AREA_TOTAL_CALC)), tags$br(),
      sprintf("Decade Built: %s", selectedPARCEL_ID$BUILD_DECADE)
    ))
    leafletProxy("map") %>% addPopups(LONGITUDE, LATITUDE, content, layerId = PARCEL_ID)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showhouseinfo(event$id, event$lat, event$lng)
    })
  })
  
  # Tables needed to create charts on Valuation Charts tab ---
  countyaccuracytable<-reactive({
    withProgress({
      setProgress(message = "Loading Charts...")
      if(is.null(filteredData()))
        return(NULL)
      filteredData() %>%
        group_by(COUNTY_NAME, ACCURACY) %>%
        dplyr::summarise (n = n()) %>%
        mutate(freq = n / sum(n))
    })
  })
  salepriceaccuracytable<-reactive({
    if(is.null(filteredData()))
      return(NULL)
    filteredData() %>%
      group_by(SALE_PRICE_RANGE, ACCURACY) %>%
      dplyr::summarise (n = n()) %>%
      mutate(freq = n / sum(n))
  })
  countroomsaccuracytable<-reactive({
    if(is.null(filteredData()))
      return(NULL)
    filteredData() %>%
      mutate(COUNT_ROOMS = factor(COUNT_ROOMS)) %>%
      group_by(COUNT_ROOMS, ACCURACY) %>%
      dplyr::summarise (n = n()) %>%
      mutate(freq = n / sum(n))
  })
  countbathroomsaccuracytable<-reactive({
    if(is.null(filteredData()))
      return(NULL)
    filteredData() %>%
      mutate(COUNT_BATHROOM = factor(COUNT_BATHROOM)) %>%
      group_by(COUNT_BATHROOM, ACCURACY) %>%
      dplyr::summarise (n = n()) %>%
      mutate(freq = n / sum(n))
  })
  builddecadeaccuracytable<-reactive({
    if(is.null(filteredData()))
      return(NULL)
    filteredData() %>%
      group_by(BUILD_DECADE, ACCURACY) %>%
      dplyr::summarise (n = n()) %>%
      mutate(freq = n / sum(n))
  })
  
  # Colors used for Valuation Charts ---
  pal2 <- c("#1F77B4","#D62728","#2CA02C")
  
  # Charts for Valuations Charts tab ---
  output$countyaccuracyplot <- renderPlotly({
      withProgress({
        setProgress(message = "Loading Charts...")
        plot_ly(data=countyaccuracytable(), type = 'bar', x = ~COUNTY_NAME, y = ~freq, color = ~ACCURACY, colors = pal2, text = ~n, textposition = 'auto', textfont = list(color = "black")) %>%
        layout(title = "County", yaxis = list(title = "",tickformat = "%"), xaxis = list(title = ""))
      })
    }) 
  output$salepriceaccuracyplot <- 
    renderPlotly({
      plot_ly(data=salepriceaccuracytable(), type = 'bar', x = ~SALE_PRICE_RANGE, y = ~freq, color = ~ACCURACY, colors = pal2, text = ~n, textposition = 'auto', textfont = list(color = "black")) %>%
        layout(title = "Price Sold", yaxis = list(title = "",tickformat = "%"), xaxis = list(title = ""))
    }) 
  output$countroomsaccuracyplot <- 
    renderPlotly({
      plot_ly(data=countroomsaccuracytable(), type = 'bar', x = ~COUNT_ROOMS, y = ~freq, color = ~ACCURACY, colors = pal2, text = ~n, textposition = 'auto', textfont = list(color = "black")) %>%
        layout(title = "Number of Rooms", yaxis = list(title = "",tickformat = "%"), xaxis = list(title = ""))
    }) 
  output$countbathroomsaccuracyplot <- 
    renderPlotly({
      plot_ly(data=countbathroomsaccuracytable(), type = 'bar', x = ~COUNT_BATHROOM, y = ~freq, color = ~ACCURACY, colors = pal2, text = ~n, textposition = 'auto', textfont = list(color = "black")) %>%
        layout(title = "Number of Bathrooms", yaxis = list(title = "",tickformat = "%"), xaxis = list(title = ""))
    })
  output$builddecadeaccuracyplot <- 
    renderPlotly({
      plot_ly(data=builddecadeaccuracytable(), type = 'bar', x = ~BUILD_DECADE, y = ~freq, color = ~ACCURACY, colors = pal2, text = ~n, textposition = 'auto', textfont = list(color = "black")) %>%
        layout(title = "Decade Built", yaxis = list(title = "",tickformat = "%"), xaxis = list(title = ""))
    })
  
  # Charts for Property Distributions tab ---
  output$roomplot <- renderPlotly({
    withProgress({
      setProgress(message = "Loading Charts...")  
      plot_ly(data=filteredData(), x = ~COUNT_ROOMS, type = "histogram", color = ~COUNTY_NAME) %>% layout(title = "Number of Rooms", xaxis = list(title = ""))
    })
  })
  output$bathroomplot <- renderPlotly({
    plot_ly(data=filteredData(), x = ~COUNT_BATHROOM, type = "histogram", color = ~COUNTY_NAME) %>% layout(title = "Number of Bathrooms", xaxis = list(title = ""))
  }) 
  output$salepriceplot <- renderPlotly({
    plot_ly(data=filteredData(), x = ~SALE_PRICE, type = "histogram", color = ~COUNTY_NAME) %>% layout(title = "Price Sold", xaxis = list(title = ""))
  }) 
  output$sqftplot <- renderPlotly({
    plot_ly(data=filteredData(), x = ~AREA_TOTAL_CALC, type = "histogram", color = ~COUNTY_NAME) %>% layout(title = "Square Footage", xaxis = list(title = ""))
  }) 
  output$builddecadeplot <- renderPlotly({
    plot_ly(data=filteredData(), x = ~BUILD_DECADE, type = "histogram", color = ~COUNTY_NAME) %>% layout(title = "Decade Built", xaxis = list(title = ""))
  })
  
  # Gather visible properties on map ---
  propertiesinrange <- reactive({
    if (is.null(input$map_bounds))
      return(home_final[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(home_final,
             LATITUDE >= latRng[1] & LATITUDE <= latRng[2] &
             LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] &
             COUNTY_NAME %in% input$counties &
             ACCURACY %in% input$accuracy &
             AREA_TOTAL_CALC >= input$squarefeet[1] & AREA_TOTAL_CALC <= input$squarefeet[2] &
             COUNT_ROOMS >= input$rooms[1] & COUNT_ROOMS <= input$rooms[2] &
             SALE_PRICE >= input$saleprice[1] & SALE_PRICE <= input$saleprice[2] &
             BUILD_DECADE %in% input$yearbuilt &
             COUNT_BATHROOM >= input$bathrooms[1] & COUNT_BATHROOM <= input$bathrooms[2])
  })
  
  # Visible properties histogram ---
  output$propertyhistogram <- renderPlot({
    if (nrow(propertiesinrange()) == 0)
      return(NULL)
    hist(propertiesinrange()$PERCENT_DIFF,
         breaks=20,
         main = "Valuation Distribution (visible properties)",
         xlab = "<--- Undervalued (-)  |  Overvalued (+) --->",
         xlim = c(min(propertiesinrange()$PERCENT_DIFF),max(propertiesinrange()$PERCENT_DIFF)),
         col = 'black',
         border = 'white')
  })
})

shinyApp(ui, server)
