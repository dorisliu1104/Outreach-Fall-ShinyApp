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
ui <- fluidPage(
  
  dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "Outreach Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the Intertidal", tabName = "about_the_intertidal", icon = icon("compass",lib="font-awesome")),
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
      # First tab content: About the Intertidal
      tabItem(tabName = "about_the_intertidal",
              h1("About The Intertidal", align = "center"),
              h3("What is ocean acidification?"),
              fluidRow(
              column(
                br(),
                p("Ocean acidification (OA) is a process that occurs when the ocean absorbs excessive amounts of anthropogenically (*resulting from human influence*) produced CO2 from the air.",
                br(),
                  tags$img(src = "OCcycle.jpeg", align = "center"),
                  style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                br(),
                p("The ocean absorbs 25% of all anthropogenically released CO2 as a carbon sink. Ocean pH is normally ~8.1, which is pretty neutral, but becomes more acidic when CO2 dissolves in water.",
                br(),
                  tags$img(src = "phscaleEPA.png", align = "center",height=350,width=500),  
                  style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                br(),
                p("When CO2 dissolves in water, it releases H+ protons and combines with carbonate to produce bicarbonate, taking carbonate out of the water
                Many marine organisms use carbonate to make shells (calcium carbonate). Taking carbonate out of the water makes it harder to make hard shells
                Furthermore, more H+ protons in the water means the water becomes more acidic and corrodes the shells of many organisms. For example, coral reefs feel OA’s impact because there’s less carbonate in the water to build the 3D reef structure.",
                br(),
                tags$img(src = "corealreef.jpeg", align = "center",height=300,width=500),
                  style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                br(),
                p("OA occurs at different rates in different ecosystems; coastal environments like the intertidal experience more rapid rates of acidification due to eutrophication (nutrient runoff caused by human activity). 
                  Eutrophication can cause algal blooms, which release large amounts of CO2 that dissolve into the water when decomposed.
                  Coastal upwelling also introduces CO2 rich water from the deep sea.",
                br(),
                  tags$img(src = "algalbloom.jpeg", align = "center",height=300,width=500),  
                  style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                br(),
                p("The intertidal is highly variable and dynamic and has extreme environmental conditions when it comes to temperature, salinity, and pH.
                  Coastal upwelling also introduces CO2 rich water from the deep sea, which contributes to coastal acidification.
                  Fossil fuels, carbon emmissions, and deforestation are humam impacts that further ocean acidification.",
                  style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                  width = 12
               )),
              h3("Your turn"),
              h4("Using the information above, answer the following questions."),
              fluidRow(
                column(
                  br(),
                  p("What do you know about ocean acidification? Follow this link to a Mentimeter[https://www.menti.com/uijfevceik] 
                    and submit the first word/phrase that comes to mind when you think of ocean acidification.",
                    style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                  br(),
                  p("What makes the intertidal unique in terms of ocean acidification and other environmental processes?",
                    style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                  br(),
                  p("How do intertidal organisms adapt to the extreme environmental conditions in which they live?",
                    style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                  width=12
                  ))),
  
      # Second tab content
      tabItem(tabName = "description",
              h2("Our Research"),
              tags$img(src = "sc1.jpeg", align = "center",height = 72, width = 72),
              h3("Scientific method: observation, hypothesis, method"),
              tags$img(src = "th.jpeg", align = "center",height = 72, width = 72)
      ),
      
      # Map tab content
      tabItem(tabName = "map",
              fluidRow(
                column(5,
                       tabPanel("Map", leafletOutput(outputId = "map", width = "100%", height = 600 ))),
                column(7, tabsetPanel(id="plot_tabs",
                                      tabPanel("Picture of Sites",
                                               h3("Pic of Alegria"),
                                               tags$img(src = "alegria.jpeg", align = "center",height = 300, width = 300)),
                                      tabPanel("Questions",
                                               h3("Pic of Lompoc Landing"),
                                               tags$img(src = "lompoc.jpeg", align = "center",height = 300, width = 300)),
                                      tabPanel("Bodega Bay",
                                               h3("Pic of Bodega Bay"),
                                               tags$img(src = "bodega.jpeg", align = "center",height = 300, width = 300))
                       )))),
              
                    
      
      # Time Series tab content 
      tabItem(tabName = "TS",
              sidebarPanel("Time Series Trend Visualization",  ##Adding a title within the sidebar
                           selectInput(inputId = "site",   
                                       label = "Visualization of sites(you can choose multiple sites)",
                                       choices = c("Alegria","Lompoc Landing", "Bodega Bay" )),
                           dateRangeInput(inputId = "date_range", 
                                          label = 'Filter tide by date',
                                          start = as.Date('2021-06-18') , end = as.Date('2021-10-08'))
                           
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
      )))))

## Create the Server
server <- function(input, output) {
  
  ## map tab
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = ~Avg_temp * 2, popup = ~popup_info, color = '#ff0000')
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