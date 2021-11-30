library(shiny) 
library(leaflet)
library(shinydashboard)
library(readxl)
library(dygraphs)
library(tidyverse)
library(here)
library(lubridate)
library(xts)
library(reshape)

## Read the Updated Data
ph_clean_updated <- read_csv(here("data", "ph_clean_1121.csv"))
ph_clean_updated <- ph_clean_updated %>%
  mutate(date=mdy(date))
f = "%m/%d/%Y" 
ph_clean_updated$SetDateMonth <- format(as.POSIXct(ph_clean_updated$date, format = f), "%m")


## Read GPS data for map
site_gps <- read_excel("data/site gps.xlsx")
site_gps <- site_gps %>% 
  mutate(popup_info = paste(site, "<br/>", 
                            "Average ph", Avg_ph, "<br/>", 
                            "Average temperature", Avg_temp, "<br/>", 
                            "Average tide", Avg_tide ))


## Create the ui (user interface)
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "Outreach Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Description", tabName = "description", icon = icon("leaf")),
      menuItem("Map", tabName = "map", icon = icon("thumbtack")),
      menuItem("Tables", tabName = "table", icon = icon("table")),
      menuItem("Time Series Trend", tabName = "TS", icon = icon("chart-line", lib = "font-awesome")),
      menuItem("Scatterplot",tabName = "scatter", icon = icon("chart-bar", lib = "font-awesome")),
      menuItem("Lompoc Landing",tabName = "lompoc", icon = icon("anchor", lib = "font-awesome")),
      menuItem("Alegria",tabName = "alegria", icon = icon("docker", lib = "font-awesome")),
      menuItem("Bodega Bay",tabName = "bodega", icon = icon("fish", lib = "font-awesome"))
    )
  ), ## end dashboardSidebar
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              HTML('<iframe width="200" height="100" src="https://www.youtube.com/embed/Gyrfsrd4zK0" frameborder="0" allowfullscreen></iframe>'),
      ),
      # Second tab content
      tabItem(tabName = "description",
              h2("Our Research", color = "red"),
              tags$img(src = "sc1.jpeg", align = "center",height = 72, width = 72),
              h3("Scientific method: observation, hypothesis, method"),
              tags$img(src = "th.jpeg", align = "center",height = 72, width = 72),
      ),
      
      # Map tab content
      tabItem(tabName = "map",
              leafletOutput(outputId = "map")    
         ),
      
      # Time Series tab content 
      tabItem(tabName = "TS",
              sidebarPanel("Time Series Trend Visualization",  ##Adding a title within the sidebar
                           selectInput(inputId = "site",   
                                       label = "Visualization of sites(you can choose multiple sites)",
                                       choices = c("Alegria","Lompoc Landing", "Bodega Bay" )),
                           dateRangeInput(inputId = "date_range", 
                                          label = 'Filter tide by date',
                                          start = as.Date('2021-06-18') , end = as.Date('2021-10-08')),
                           
              ),
              mainPanel("Some text for the main panel",
                        plotOutput(outputId="ph_ts_plot"))),
      
      ##Scatterplot tab content
      tabItem(tabName = "scatter",
              sidebarPanel("Scatterplot Visualization", ##Adding a title within the sidebar
                           selectInput(inputId = "x",
                                       label = "X variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" )),
                           selectInput(inputId = "y",
                                       label = "Y variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" ))
                           
              ),
              
              mainPanel("Some text for the main panel",
                        plotOutput(outputId="ph_scatterplot_Alegria", width = "50%", height = "200px"),
                        plotOutput(outputId="ph_scatterplot_Bodega", width = "50%",height = "200px"),
                        plotOutput(outputId="ph_scatterplot_Lompoc", width = "50%",height = "200px"))
              
              
      ),
      
      ## Lompoc Landing 
      tabItem(tabName = "lompoc",
              sidebarPanel("Lompoc Landing Visualization",
                           selectInput(inputId = "x2",
                                       label = "X variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" )),
                           selectInput(inputId = "y2",
                                       label = "Y variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" ))
                          
                           ),
              mainPanel("Some text for the main panel",
                        plotOutput(outputId="scatterplot_lompoc"))
      ),
      
      ## Alegria
      tabItem(tabName = "alegria",
              sidebarPanel("Alegria Visualization",
                           selectInput(inputId = "x3",
                                       label = "X variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" )),
                           selectInput(inputId = "y3",
                                       label = "Y variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" ))
                           
              ),
              mainPanel("Some text for the main panel",
                        plotOutput(outputId="scatterplot_alegria"))
      ),
      
      ## Bodega Bay
      tabItem(tabName = "bodega",
              sidebarPanel("Bodega Bay Visualization",
                           selectInput(inputId = "x4",
                                       label = "X variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" )),
                           selectInput(inputId = "y4",
                                       label = "Y variable",
                                       choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" ))
                           
              ),
              mainPanel("Some text for the main panel",
                        plotOutput(outputId="scatterplot_bodega"))
      )
    )
  ))

## Create the Server
server <- function(input, output) {
  
  ## map tab
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, popup = ~popup_info)
    }) 
  
  ## time series tab
  
  dateFiltered <- reactive({
    ph_clean_updated %>%
      filter(site == input$site) %>%
      filter(date>=input$date_range[1] & date<input$date_range[2])
  })
  
  #reactive plot
  output$ph_ts_plot <- renderPlot({
    ggplot(data = dateFiltered(), 
           aes(x = date_time, y=tide)) + geom_line()
  })
  
  ## scatterplot tab
  output$ph_scatterplot_Alegria <- renderPlot({
    ggplot(subset(ph_clean_updated, site %in% "Alegria"), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      ggtitle("Alegria")
  })
  output$ph_scatterplot_Bodega <- renderPlot({
    ggplot(subset(ph_clean_updated, site %in% "Bodega Bay"), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      ggtitle("Bodega Bay")
  })
  output$ph_scatterplot_Lompoc <- renderPlot({
    ggplot(subset(ph_clean_updated, site %in% "Lompoc Landing"), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      ggtitle("Lompoc Landing")
  })
  
  ## lompoc tab
  output$scatterplot_lompoc <- renderPlot({
    ggplot(subset(ph_clean_updated, site %in% "Lompoc Landing"), 
           aes_string(x = input$x2, y = input$y2)) +
      geom_point(aes(color = SetDateMonth)) +
      ggtitle("Lompoc Landing")
  })
  
  ## alegria tab
  output$scatterplot_alegria <- renderPlot({
    ggplot(subset(ph_clean_updated, site %in% "Alegria"), 
           aes_string(x = input$x3, y = input$y3)) +
      geom_point(aes(color = SetDateMonth)) +
      ggtitle("Alegria")
  })
  
  ## bodega tab
  output$scatterplot_bodega <- renderPlot({
    ggplot(subset(ph_clean_updated, site %in% "Bodega Bay"), 
           aes_string(x = input$x4, y = input$y4)) +
      geom_point(aes(color = SetDateMonth)) +
      ggtitle("Bodega Bay")
  })
  
}


## Combine the UI and the server
shinyApp(ui = ui, server = server)