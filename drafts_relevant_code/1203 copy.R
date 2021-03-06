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
ph_clean_final <- read_csv(here("data", "ph_clean_final.csv"))
ph_clean_final <- ph_clean_final %>%
  mutate(date=mdy(date))
f = "%m/%d/%Y" 
ph_clean_final$SetDateMonth <- format(as.POSIXct(ph_clean_final$date, format = f), "%m")


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
        menuItem("Our Research", tabName = "research", icon = icon("docker",lib="font-awesome")),
        menuItem("Data (Lompoc Results)", tabName = "data", icon = icon("anchor",lib="font-awesome")),
        menuItem("Compare and Contrast", tabName = "compare", icon = icon("chart-bar",lib="font-awesome")),
        menuItem("Conclusion", tabName = "conclusions", icon = icon("fish", lib = "font-awesome")),
        menuItem("Acknowledgements", tabName="acknowledgements",icon=icon("trophy",lib="font-awesome"))
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
                      br(),
                      tags$img(src = "OCcycle.jpeg", align = "center"),
                      style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                    br(),
                    p("The ocean absorbs 25% of all anthropogenically released CO2 as a carbon sink. Ocean pH is normally ~8.1, which is pretty neutral, but becomes more acidic when CO2 dissolves in water.",
                      br(),
                      br(),
                      tags$img(src = "phscaleEPA.png", align = "center",height=350,width=500),  
                      style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                    br(),
                    p("When CO2 dissolves in water, it releases H+ protons and combines with carbonate to produce bicarbonate, taking carbonate out of the water
                Many marine organisms use carbonate to make shells (calcium carbonate). Taking carbonate out of the water makes it harder to make hard shells
                Furthermore, more H+ protons in the water means the water becomes more acidic and corrodes the shells of many organisms. For example, coral reefs feel OA’s impact because there’s less carbonate in the water to build the 3D reef structure.",
                      br(),
                      br(),
                      tags$img(src = "corealreef.jpeg", align = "center",height=300,width=500),
                      style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                    br(),
                    p("OA occurs at different rates in different ecosystems; coastal environments like the intertidal experience more rapid rates of acidification due to eutrophication (nutrient runoff caused by human activity). 
                  Eutrophication can cause algal blooms, which release large amounts of CO2 that dissolve into the water when decomposed.
                  Coastal upwelling also introduces CO2 rich water from the deep sea.",
                      br(),
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
        
      
        # our research tab content
        
        tabItem(tabName = "research",
                h1("Our Research"),
                fluidRow(
                  column(5,
                         tabPanel("Map", leafletOutput(outputId = "map", width = "100%", height = 600 ))),
                  column(7, tabsetPanel(id="plot_tabs",
                                        tabPanel("Picture of Sites",
                                                 radioButtons("pics","Pictures of Sites", 
                                                              choices=c("Alegria"="alegria" , "Bodega Bay"="bodega", "Lompoc Landing"="lompoc"), inline=T),
                                                 imageOutput("pic_site")),
                                        tabPanel("questions",
                                                 p("Examine the three pictures from the three different sites in which sensors were deloyed. What are some visual differences between each environment?",
                                                   br(),
                                                   br(),
                                                   actionButton("answer1", label = "Show answer"),
                                                   verbatimTextOutput("text1"),
                                                   style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                                                 br(),
                                                 p("Why were these three sites selected? What are the geographic differences between each site?",
                                                   br(),
                                                   br(),
                                                   actionButton("answer2", label = "Show answer"),
                                                   verbatimTextOutput("text2"),
                                                   style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
                                                 br(),
                                                 p("What other physical variables in the intertidal could affect the data collected by the sensors besides the ones being tested for (pH and temperature)?",
                                                   br(),
                                                   br(),
                                                   actionButton("answer3", label = "Show answer"),
                                                   verbatimTextOutput("text3"),
                                                   style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"))
                  )))),
        
        tabItem(tabName = "data",
                titlePanel("Time Series Trend Visualization for Lompoc Landing"),
                sidebarPanel(selectInput(inputId = "var",   
                                         label = "Variables",
                                         choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" )),
                             dateRangeInput(inputId = "date_range", 
                                            label = 'Filter tide by date',
                                            start = as.Date('2021-06-18') , end = as.Date('2021-10-08'))
                             ),
                mainPanel(plotOutput(outputId="ph_ts_plot")),
                tabPanel("Questions",
                 p("Q1. What trends do you notice between time and tide height for the Lompoc site?")
                 )),
        
        tabItem(tabName = "compare",
                fluidRow(
                  column(5,
                         tabPanel("Map", leafletOutput(outputId = "map2", width = "100%", height = 600 ))))),
                         
        
        # conclusion & global implications tab content
        tabItem(tabName = "conclusions",
                h1("Conclusion and Global Implications"),
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
        
        # acknowledgements tab content
        tabItem(tabName="acknowledgements",
                h1("Acknowledgements"),
        )
      ))))

## Create the Server
server <- function(input, output) {
  
  # answers for "our research" tab
  observeEvent(input$answer1, {
      output$text1 <- renderText({"Alegria has a lot more sand and is flatter than rocky Bodega. Lompoc is structured like a shelf with steps. The sensor was exposed at the Bodega site."})
    })
  
  observeEvent(input$answer2, {
    output$text2 <- renderText({"Alegria is furthest south (only one south of Point Conception, meaning it has less intense upwelling and higher average pH), followed by Lompoc and Bodega Bay north of Point Conception which are in the same upwelling regime"})
  })
  
  observeEvent(input$answer3, {
    output$text3 <- renderText({"zonation, isolation of pools, depth"})
  })
  
  ## map tab
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = ~Avg_temp * 2, popup = ~popup_info, color = '#ff0000')
  }) 
  
  output$pic_site <- renderImage({
    filename <- normalizePath(file.path('./www/', paste(input$pics, ".png", sep="")))
    
    list(src = filename, height = 300, width = 300)
  }, deleteFile = FALSE
  )
  
  ## time series tab
  
  dateFiltered <- reactive({
    ph_clean_final %>%
      filter(site == "Lompoc Landing") %>%
      filter(date>=input$date_range[1] & date<input$date_range[2])
  })
  
  #reactive plot
  output$ph_ts_plot <- renderPlot({
    ggplot(data = dateFiltered(), 
           aes(x = date_time, y = input$var)) + geom_line()
  })
  
  ## compare and contrast
  output$map2 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = ~Avg_temp * 2, popup = ~popup_info, color = '#ff0000')
  }) 
}

## Combine the UI and the server
shinyApp(ui = ui, server = server)