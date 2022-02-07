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
library(highcharter)

## Read the Updated Data
ph_clean_final <- read_csv(here("data", "ph_clean_final.csv"))
ph_clean_final <- ph_clean_final %>%
  mutate(date=mdy(date))
f = "%m/%d/%Y" 
ph_clean_final$SetDateMonth <- format(as.POSIXct(ph_clean_final$date, format = f), "%m")

## Read GPS data for map
site_gps <- read_excel("data/site gps.xlsx")
site_gps <- site_gps %>% 
  mutate(popup_info = paste("Average pH", Avg_ph, "<br/>", 
                            "Average temperature", Avg_temp))

# filter data for lompoc and set axis for highchart
lompoc <- ph_clean_final %>% 
  dplyr::filter(site=="Lompoc Landing") %>% 
  unite("date_time", "date", "time", sep="\ ", remove = FALSE) %>%
  mutate(date_time=ymd_hms(date_time))

y1 <- lompoc$temp_c #set first y axis
y2 <- lompoc$p_h  #set second y axis
y3 <- lompoc$tide_height #set third y axis
x <- lompoc$date_time #set x axis

lompoc2 <- filter(lompoc, between(date, as.Date("2021-07-25"), as.Date("2021-08-05")))

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
                tabsetPanel(id="about_the_intertidal",
                            tabPanel(h4("What is ocean acidification?"),
                                     h4(p("Ocean acidification (OA) is a process that occurs when the ocean absorbs excessive amounts of anthropogenically (*resulting from human influence*) produced CO2 from the air.",
                                       br(),
                                       br(),
                                       tags$img(src = "OCcycle.jpeg", style="display: block; margin-left: auto; margin-right: auto;"),
                                       br(),
                                       br(),
                                       "The ocean absorbs 25% of all anthropogenically released CO2 as a carbon sink. Ocean pH is normally ~8.1, which is pretty neutral, but becomes more acidic when CO2 dissolves in water.",
                                       br(),
                                       br(),
                                       tags$img(src = "phscaleEPA.png", style="display: block; margin-left: auto; margin-right: auto;", height=350,width=500),  
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))),
                            tabPanel(h4("The effect on coral and shelled organisms"),
                                     h4(p("When CO2 dissolves in water, it releases H+ protons and combines with carbonate to produce bicarbonate, taking carbonate out of the water.
                                       Many marine organisms use carbonate to make shells (calcium carbonate). Taking carbonate out of the water makes it harder to make hard shells.
                                       Furthermore, more H+ protons in the water means the water becomes more acidic and corrodes the shells of many organisms. For example, coral reefs feel OA’s impact because there’s less carbonate in the water to build the 3D reef structure.",
                                       br(),
                                       br(),
                                       tags$img(src = "corealreef.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height=300,width=500),
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))),
                            tabPanel(h4("Eutrophication"),
                                     h4(p("OA occurs at different rates in different ecosystems; coastal environments like the intertidal experience more rapid rates of acidification due to eutrophication (nutrient runoff caused by human activity).
                                       Eutrophication can cause algal blooms, which release large amounts of CO2 that dissolve into the water when decomposed.
                                       Coastal upwelling also introduces CO2 rich water from the deep sea.",
                                       br(),
                                       br(),
                                       tags$img(src = "algalbloom.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height=300,width=500),
                                       br(),
                                       br(),
                                       "The intertidal is highly variable and dynamic and has extreme environmental conditions when it comes to temperature, salinity, and pH.
                                         Lastly, fossil fuels, carbon emmissions, and deforestation are humam impacts that further ocean acidification.",
                                         style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))),
                            tabPanel(h4("Your turn"),
                                     h4(p(strong("Using the information you just learned, answer the following questions."),
                                          style="text-align:center"),
                                     br(),
                                     p("1. What do you know about ocean acidification? Follow this link to a",
                                       tags$a(href="https://www.menti.com/uijfevceik", "Mentimeter"), 
                                       "and submit the first word/phrase that comes to mind when you think of ocean acidification.",
                                       br(),
                                       em("View your class's Mentimeter",
                                       tags$a(href="https://www.mentimeter.com/s/29f86468afbfa7aa26f27554857d25d9/974479e8d82c", "here.")),
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                     br(),
                                     p("2. What makes the intertidal unique in terms of ocean acidification and other environmental processes?",
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                     br(),
                                     p("3. How do intertidal organisms adapt to the extreme environmental conditions in which they live?",
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     width=12
                            ))),
        
        # our research tab content
        
        tabItem(tabName = "research",
                h1("Our Research"),
                fluidRow(
                  column(5,
                         h3("About the sites"),
                         tabsetPanel(id="about_the_sites_tabs",
                                     tabPanel(h4("Site pictures"),
                                              h4("Bodega Bay",
                                                 style="text-align:center"),
                                              tags$img(src = "bodega_site.jpg", align = "center",height=225,width=375),
                                              br(),
                                              br(),
                                              h4("Lompoc Landing",
                                                 style="text-align:center"),
                                              tags$img(src = "lol_site.jpg", align = "center",height=225,width=375),
                                              br(),
                                              br(),
                                              h4("Alegria",
                                                 style="text-align:center"),
                                              tags$img(src = "alegria_site.jpg", align = "center",height=225,width=375),
                                              ),
                                     tabPanel(h4("On the map"),
                                              leafletOutput(outputId = "map", width = "100%", height = 600 )))),
                         
                  column(7, 
                         h3("Your turn"),
                         tabsetPanel(id="our_research_tabs",
                                     tabPanel(h4("Question 1"),
                                              h4(p("Examine the three pictures from the three different sites in which sensors were deloyed. What are some visual differences between each environment?",
                                                style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                              column(12, align="right",
                                                     checkboxInput("checkbox1", label = "Show answer", value = FALSE)),
                                                conditionalPanel(
                                                  condition = "input.checkbox1 == 1",
                                                  h4(p(em("A lot more sand in Alegria, more flat than rocky Bodega; Lompoc is structured like a shelf with steps; sensor was exposed at Bodega site"),
                                                    style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")))),
                                     tabPanel(h4("Question 2"),
                                              h4(p("Why were these three sites selected? What are the geographic differences between each site?",
                                                style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                                column(12, align="right",
                                                       checkboxInput("checkbox2",label = "Show answer", value = FALSE)),
                                                conditionalPanel(
                                                  condition = "input.checkbox2 == 1",
                                                  h4(p(em("Alegria is furthest south (only one south of Point Conception, meaning it has less intense upwelling and higher average pH), followed by Lompoc and Bodega Bay north of Point Conception which are in the same upwelling regime"),
                                                    style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))
                                                )),
                                     tabPanel(h4("Question 3"),
                                              h4(p("What other physical variables in the intertidal could affect the data collected by the sensors besides the ones being tested for?",
                                                style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                                column(12, align="right",
                                                       checkboxInput("checkbox3", label = "Show answer", value = FALSE)),
                                                conditionalPanel(
                                                  condition = "input.checkbox3 == 1",
                                                  h4(p(em("zonation, isolation of pools, depth"),
                                                    style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))))
                  )))),
        
        tabItem(tabName = "data",
                titlePanel("Time Series Trend Visualization for Lompoc Landing"),
                tabsetPanel(id = "lomdata",
                tabPanel(h4("Question 1"),
                          sidebarPanel(dateRangeInput(inputId = "date_range", 
                                            label = 'Filter tide by date',
                                            start = as.Date('2021-06-14') , end = as.Date('2021-10-06')),
                                       br(),
                                       h4("Question 1"),
                                       h4("What trends do you notice between pH and temperature for the Lompoc site?")),
                                       
                mainPanel(highchartOutput("ph_ts_plot"))
               ),
                
                tabPanel(
                         h4("Question 2"),
                         sidebarPanel(selectInput(inputId = "ph_temp",
                                                  label = "select pH or temperature",
                                                  choices = c("Temperature"="temp_c","pH"="p_h")),
                                      br(),
                                      h4("Question 2"),
                                      h4("What do you notice about the scale of change for both pH and temp over hours? Days? Weeks/months?")),
                         mainPanel(plotOutput(outputId = "q2plot"))),
                tabPanel(
                         h4("Question 3"),
                         sidebarPanel(
                           h4("Question 3"),
                           h4("Search up the weather for August 2 and compare it to the Lompoc data. What do you think could’ve caused the spikes in the data? What are some reasons why the temperature might’ve hit an extreme that day? "),
                           br()),
                         mainPanel(highchartOutput("q3plot"))
                         )
                
                         
                )),
        
        tabItem(tabName = "compare",
                fluidRow(
                  column(5,
                         tabPanel("Map", leafletOutput(outputId = "map2", width = "100%", height = 600 ))))),
        
        
        # conclusion & global implications tab content
        tabItem(tabName = "conclusions",
                h1("Conclusion and Global Implications"),
                tabsetPanel(id="conclusiontabs",
                            tabPanel(h4("Questions"),
                                     h4(p("1. What have you learned about ocean acidification? Check your comprehension and summarize what you know about ocean acidification by creating a concept map using these terms. Add to the map as you learn more about OA:",
                                      br(),
                                      br(),
                                      em("Burning fossil fuels, Nutrient runoff, Eutrophication, Carbon emissions, Bicarbonate, shelled organisms, ocean acidification, coral reefs, & intertidal"),
                                      style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                      br(),
                                      p("2. How can the data collected and its conclusions be used to inform conservation and management efforts in the intertidal?",
                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")
)),
                            tabPanel(h4("Get involved"),
                                        h4(p(strong("Click on each logo to explore the organization's website!"))),
                                        tags$a(imageOutput("ucsblogo.jpeg"), href="https://www.ucsb.edu")))),
                
        
        # acknowledgements tab content
        tabItem(tabName="acknowledgements",
                h1("Acknowledgements"),
        )))))

## Create the Server
server <- function(input, output) {
  
  ## map tab
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = 7, popup = ~popup_info, color = '#ff0000') %>%
      addLabelOnlyMarkers(
        lng = -125.5921856, lat = 38.31875756,
        label = "Bodega Bay",
        labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "15px",
                                    style = list("font-style" = "italic"))) %>%
      addLabelOnlyMarkers(
        lng = -121.3505135, lat = 34.70743071,
        label = "Lompoc Landing",
        labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "15px",
                                    style = list("font-style" = "italic"))) %>%
      addLabelOnlyMarkers(
        lng = -121.1519116, lat = 34.19907704,
        label = "Alegria",
        labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "15px",
                                    style = list("font-style" = "italic")))
      
      
    
  }) 
  
  ## Data (Lompoc) Tab
  
  dateFiltered <- reactive({
    ph_clean_final %>%
      filter(site == "Lompoc Landing") %>%
      filter(date>=input$date_range[1] & date<input$date_range[2])
  })
  
  # Highchart
  output$ph_ts_plot <- renderHighchart({
    y1 <- dateFiltered()$temp_c #set first y axis
    y2 <- dateFiltered()$p_h  #set second y axis
    y3 <- dateFiltered()$tide_height #set third y axis
    x <- dateFiltered()$date_time
    
    highchart() %>% 
      hc_add_series(data = y1, dashStyle="solid", name = "Temperature") %>% #plot temp
      hc_add_series(data = y2, yAxis = 1, name = "pH") %>% #plot pH
      hc_add_series(data = y3, yAxis = 2, name = "Tide") %>% #plot tide height
      hc_yAxis_multiples(
        list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temperature")), #label/colorize temp y axis
        list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
        list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide"))) %>% #label/colorize tide y axis
      hc_xAxis(title = "Date", categories = x, breaks=10) %>% #label x axis
      hc_colors(c("#D55E00", #set specific colors for points (note same color order as y axis)
                  "#009E73",
                  "#0072B2"))
      
    
  })
  # lompoc reactive
  
  lompocReactive <- reactive({
    lompoc %>%
      select(date_time, input$ph_temp) %>%
      mutate(variable = input$ph_temp)
  })
  
  # data_q2_plot
  output$q2plot <- renderPlot({
    ggplot(lompoc, aes(x=date_time, y=get(input$ph_temp)))+ #plot pH here
      geom_line(size = 0.7,color = ifelse(input$ph_temp == "temp_c", "#D55E00","#009E73" )) + #make it a line chart
      geom_smooth(method="loess", span=0.1) + #plot trend line for each site
      #scale_color_manual(values = ifelse(input$ph_temp == "temp_c", "#D55E00","#009E73" )) + #color lines by custom site color palette
      scale_x_datetime(breaks = scales::date_breaks("1 week"), 
                       labels = date_format("%m/%d %H:%m")) + #change x axis to make it look cleaner - each tick is one week, display month/day hour/minute
      xlab("Date time") + #change x axis label
      ylab(ifelse(input$ph_temp == "temp_c", "Temperature", "pH")) + #change y axis label
      theme_bw() +
      theme(#legend.position = "none", #remove legend
        axis.text.x=element_text(angle=45, vjust = 1, hjust=1, size=12), #adjust x axis text format
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=12), #adjust y axis text format
        axis.title.y=element_text(size=15))
  })
  
  # data_q3plot
  output$q3plot <- renderHighchart({
    y4 <- lompoc2$temp_c #set first y axis
    y5 <- lompoc2$p_h  #set second y axis
    y6 <- lompoc2$tide_height #set third y axis
    x <- lompoc2$date_time
    
    highchart() %>% 
      hc_add_series(data = y4, dashStyle="solid", name = "Temperature") %>% #plot temp
      hc_add_series(data = y5, yAxis = 1, name = "pH") %>% #plot pH
      hc_add_series(data = y6, yAxis = 2, name = "Tide") %>% #plot tide height
      hc_yAxis_multiples(
        list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temperature")), #label/colorize temp y axis
        list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
        list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide"))) %>% #label/colorize tide y axis
      hc_xAxis(title = "Date", categories = x, breaks=10) %>% #label x axis
      hc_colors(c("#D55E00", #set specific colors for points (note same color order as y axis)
                  "#009E73",
                  "#0072B2"))
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