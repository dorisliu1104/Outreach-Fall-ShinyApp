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
library(DT)
library(scales)
library(slickR)
library(janitor)

# #add functionality to publish app
# library(rsconnect)
# library(BiocManager)
# options(repos = BiocManager::repositories())

## Read the Updated Data
ph_clean_final <- read_csv(here("data", "ph_clean_final.csv"))
ph_clean_final <- ph_clean_final %>%
  arrange(date_time) %>% 
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

lompoc2 <- filter(lompoc, between(date, as.Date("2021-07-25"), as.Date("2021-08-05")))
lompoc3 <- filter(lompoc, between(date, as.Date("2021-08-26"), as.Date("2021-09-27")))
lompoc4 <- filter(lompoc, between(date, as.Date("2021-06-17"), as.Date("2021-06-23")))
alegria4 <- dplyr::filter(ph_clean_final, site=="Alegria") %>% 
  filter(between(date, as.Date("2021-08-07"), as.Date("2021-08-17")))
bodega4 <- dplyr::filter(ph_clean_final, site=="Bodega Bay") %>% 
  filter(between(date, as.Date("2021-07-05"), as.Date("2021-07-09")))

## create dataframe for compare and contrast plots

comdata <- ph_clean_final %>% 
  unite("date_time", "date", "time", sep="\ ") %>%
  mutate(date_time=ymd_hms(date_time)) #apply lubridate to date/time column

comdata$site <- factor(comdata$site, levels=c("Lompoc Landing","Bodega Bay", "Alegria"))

pal <- c(
  "Alegria" = "#D55E00",
  "Lompoc Landing" = "#009E73", 
  "Bodega Bay" = "#0072B2"
)

## import data summary table
data_summary_table <- site_gps %>%
  clean_names() %>%
  select(site, min_temp, avg_temp, max_temp, min_ph, avg_ph, max_ph) %>%
  dplyr::rename("Site"=1,
         "Minimum Temperature"=2,
         "Maximum Temperature"=4,
         "Average Temperature"=3,
         "Minimum pH"=5,
         "Maximum pH"=7,
         "Average pH"=6)

## Filter data for alegria and bodega bay
alegria <- dplyr::filter(ph_clean_final, site=="Alegria")  ## Filtering the data according to different sites
bodega <- dplyr::filter(ph_clean_final, site=="Bodega Bay")


## Create the ui (user interface)
ui <- fluidPage(
  
  dashboardPage(
    
    skin = "blue",
    
    dashboardHeader(title = "Ocean Acidification on the CA Coast",
                    titleWidth = 375),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About Ocean Acidification", tabName = "about_the_intertidal", icon = icon("compass",lib="font-awesome")),
        menuItem("Our Research", tabName = "research", icon = icon("docker",lib="font-awesome")),
        menuItem("Lompoc Landing Data", tabName = "data", icon = icon("anchor",lib="font-awesome")),
        menuItem("Compare and Contrast", tabName = "compare", icon = icon("chart-bar",lib="font-awesome")),
        menuItem("Conclusion", tabName = "conclusions", icon = icon("fish", lib = "font-awesome")),
        menuItem("Acknowledgements", tabName="acknowledgements",icon=icon("trophy",lib="font-awesome"))
      )
    ), ## end dashboardSidebar
    
    dashboardBody(
      tabItems(
        # First tab content: About the Intertidal
        tabItem(tabName = "about_the_intertidal",
                h1("About Ocean Acidification"),
                tabsetPanel(id="intertidal_tab",
                            tabPanel(h4("What is ocean acidification?"),
                                     h4(p("The ocean is a carbon sink that absorbs around 30% of the carbon dioxide (CO2) released to the atmosphere by human activities, such as burning fossil fuels and deforestation.
                                          Ocean pH is normally around 8.1, but the pH drops when CO2 dissolves in seawater.",
                                          br(),
                                          br(),
                                          tags$img(src = "OCcycle.jpeg", style="display: block; margin-left: auto; margin-right: auto;"),
                                          column(width = 12,
                                                 h6(p("Source: Northeast Coastal Acidification Network, http://www.necan.org/overview"),
                                                    style="text-align:center;color:darkgray")),
                                          br(),
                                          br(),
                                          "Ocean acidification (OA) is a chemical change that occurs when seawater absorbs excessive amounts of anthropogenically-produced CO2 from the air.
                                          When CO2 dissolves in water, it releases hydrogen ions that combine with carbonate ions to produce bicarbonate, thereby removing carbonate from seawater.",
                                          br(),
                                          br(),
                                          em("Watch this short video for an overview of OA and its effects:"),
                                          br(),
                                          br(),
                                          HTML('<center><iframe width="560" height="315" src="https://www.youtube.com/embed/gZGj0BbDT38" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></center>')),
                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                            ###
                            tabPanel(h4("The effect on calcifying organisms"),
                                     h4(p("The removal of carbonate and addition of hydrogen ions in seawater through the process of OA has serious consequences for many marine organisms, especially marine calcifiers.
                                       Calcifying marine organisms use carbonate to make shells (calcium carbonate). Therefore, the removal of carbonate from seawater makes it harder for these organisms to form hard, strong shells.
                                       Furthermore, the addition of hydrogen drops the seawater pH and corrodes calcifier's shells.
                                       Corals, crabs, snails, and many other organisms that use carbonate to build their shells and skeletons are thus affected, and the effects cascade across the food chain.",
                                       br(),
                                       br(),
                                       tags$img(src = "corealreef.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height=300,width=500),
                                       column(width = 12,
                                              h6(p("Source: Scientific American, https://www.scientificamerican.com/article/corals-may-get-temporary-reprieve-from-bleaching/"),
                                                 style="text-align:center;color:darkgray")),
                                       br(),
                                       br(),
                                       em("Watch this short video to learn more about the effects of OA on calcifying organisms:"),
                                       br(),
                                       br(),
                                       HTML('<center><iframe width="560" height="315" src="https://www.youtube.com/embed/aG3n1fAa7vk" title="The Acid Test" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></center>')),
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                            ###
                            tabPanel(h4("Along the coastline"),
                                     h4(p("OA occurs at different rates in different ecosystems.
                                          Coastal environments in particular experience more rapid rates of acidification due to local processes, often linked to human activities, that alter water chemistry. 
                                          For example, eutrophication (nutrient runoff) can cause algal blooms that release CO2 when the algae die and decompose.",
                                          br(),
                                          br(),
                                          tags$img(src = "algalbloom.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height=300,width=500),
                                          column(width = 12,
                                                 h6(p("Source: American Rivers, https://www.americanrivers.org/2017/10/ohios-maumee-river-green/"),
                                                    style="text-align:center;color:darkgray")),
                                          br(),
                                          "Not all changes in water chemistry are caused by humans, however.
                                          Coastal upwelling is a process that occurs along the California coastline, and elsewhere globally, whereby cold, nutrient-rich water is transported from the ocean's depths to the ocean's surface via surface winds.
                                          Because the ocean acts as a carbon sink, the transport of water from the depth also brings along more acidic water to the surface and into shallow coastal ecosystems.",
                                          br(),
                                          br(),
                                          tags$img(src = "upwelling.jpg", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                          column(width = 12,
                                                 h6(p("Source: National Oceanic and Atmospheric Administration (NOAA), https://oceanservice.noaa.gov/education/tutorial_currents/03coastal4.html"),
                                                    style="text-align:center;color:darkgray")),
                                          br()),
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                            ### 
                            tabPanel(h4("In the intertidal"),
                                     h4(p("The intertidal zone is a highly variable and dynamic environment.
                                          Organisms that live in the intertidal are subject to the extreme conditions resulting from waves and the daily rise and fall of the tides.
                                          When the tide goes out, organisms must cope with increases in salinity and temperature, reduced oxygen availability, and even exposure to the air.
                                          pH also fluctuates daily in the intertidal zone, thanks to photosynthesis and respiration by marine organisms that changes CO2 concentrations in the tidepools at low tides.",
                                          br(),
                                          br(),
                                          tags$img(src = "intertidal-fig.jpg", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                          column(width = 12,
                                                 h6(p(em("Figure by ",
                                             tags$a(href="https://www.frontiersin.org/articles/10.3389/fmars.2021.667168/full", "Kunze et al 2021"), ".")),
                                                    style="text-align:center;color:darkgray")),
                                          br(),
                                          br(),
                                          "As the figure above demonstrates, organisms in the intertidal zone are affected by both natural and human influences. For example, when the intertidal zone acidifies even further under OA conditions, marine organisms become stressed, which can negatively affect their development, behavior, and physiology.",
                                          br(),
                                          br(),
                                       tags$img(src = "pachy-lol.jpg", style="display: block; margin-left: auto; margin-right: auto;", height="50%",width="50%"),
                                       column(width = 12,
                                              h6(p("Source: Amelia Ritger"),
                                                 style="text-align:center;color:darkgray")),
                                       br()),
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                            ###
                            tabPanel(h4("Your turn"),
                                     h4(p(strong("Using the information you just learned, answer the following questions."
                                     )),
                                     style="text-align:center"),
                                     br(),
                                     h4(p("1. What do you know about ocean acidification? Use a",
                                          tags$a(href="https://www.menti.com/uijfevceik", "Mentimeter"), 
                                          "to submit the words or phrases that come to mind when you think of ocean acidification.",
                                          br(),
                                          br(),
                                          em("View your class's Mentimeter",
                                             tags$a(href="https://www.mentimeter.com/s/29f86468afbfa7aa26f27554857d25d9/974479e8d82c", "here.")),
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     br(),
                                     h4(p("2. What is cause and effect of ocean acidification?",
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     br(),
                                     h4(p("3. What makes the intertidal zone uniquely affected by ocean acidification?",
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")))
                            
                )),
        # our research tab content
        
        tabItem(tabName = "research",
                h1("Our Research"),
                fluidRow(
                  column(5,
                         h3("Research methods"),
                         tabsetPanel(id="about_the_sites_tabs",
                                     tabPanel(h4("Background"),
                                              h4("California has two upwelling regimes, geographically separated by Point Conception:", strong("strong upwelling in the north"), ", where marine environments are subject to persistent colder temperatures and low pH conditions, and", strong("weak upwelling in the south"), ", where marine environments are subject to persistent warmer temperatures and higher pH conditions."),
                                              br(),
                                              tags$img(src = "caupwelling-noaa.jpg", align = "center",height="100%",width="100%"),
                                              br(),
                                              h5(em("California sea surface water temperature")),
                                              br(),
                                              h4("However, the current environmental conditions of the intertidal zone are virtually unexplored in California.")
                                     ),
                                     tabPanel(h4("Question"),
                                              h4(strong("Question:"), "What are the pH and temperature conditions across the California coastline?"),
                                              br(),
                                              h4(strong("Hypothesis:"), "pH will decrease with increasing latitude, and temperature will increase with decreasing latitude."),
                                              br(),
                                              h4(strong("Predictions:"), "Bodega Bay, the northernmost site, will experience the lowest pH and temperatures, whereas Alegria, the southernmost site, will experience the highest pH and temperatures.")
                                     ),
                                     tabPanel(h4("Methods"),
                                              h4(strong("Sensor deployments:"), "pH and temperature sensors were deployed in the rocky intertidal zone at three sites - Bodega Bay, Lompoc Landing, and Alegria - from May 2021 until November 2021. Sensors took temperature and pH measurements every 15 minutes."),
                                              br(),
                                              h4(strong("Site selection:"), "Sites were selected because they had 1) been studied before, which would allow us to build upon historical datasets, 2) capture variation California's two upwelling regimes, and 3) limit public access, which protects the valuable sensor equipment."),
                                              br(),
                                              h4(strong("Site map")),
                                              leafletOutput(outputId = "map", width = "100%", height = 400),
                                              br(),
                                              h4(em("Take a look at",
                                                 tags$a(href="https://www.windy.com/?36.364,-121.838,7,i:pressure", "this website"), "to explore more physical differences between these three sites and across the California coastline"))),
                                     tabPanel(h4("Site pictures"),
                                              "**we will add more photos from each site in the final version of the app**",
                                              h4("Bodega Bay",
                                                 style="text-align:center"),
                                              tags$img(src = "bodega_site.jpg", align = "center",height="100%",width="100%"),
                                              br(),
                                              br(),
                                              h4("Lompoc Landing",
                                                 style="text-align:center"),
                                              tags$img(src = "lol_site.jpg", align = "center",height="100%",width="100%"),
                                              br(),
                                              br(),
                                              h4("Alegria",
                                                 style="text-align:center"),
                                              tags$img(src = "alegria_site.jpg", align = "center",height="100%",width="100%"),
                                     )
                                     #tabPanel(h4("Site map")
                                              #leafletOutput(outputId = "map", width = "100%", height = 600 )
                                              )),
                         
                  column(7, 
                         h3("Your turn"),
                         tabsetPanel(id="our_research_tabs",
                                     tabPanel(h4("Question 1"),
                                              h4(p("Examine the pictures from the three different study sites. What are some visual differences between each environment?",
                                                style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                              column(12, align="right",
                                                     checkboxInput("checkbox1", label = "Show answer", value = FALSE)),
                                                conditionalPanel(
                                                  condition = "input.checkbox1 == 1",
                                                  h4(p(em("A lot more sand in Alegria, more flat than rocky Bodega; Lompoc is structured like a shelf with steps; sensor was exposed at Bodega site"),
                                                    style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")))),
                                     tabPanel(h4("Question 2"),
                                              h4(p("Why were these three sites selected? How are these sites similar? How are they different?",
                                                style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                                column(12, align="right",
                                                       checkboxInput("checkbox2",label = "Show answer", value = FALSE)),
                                                conditionalPanel(
                                                  condition = "input.checkbox2 == 1",
                                                  h4(p(em("Alegria is furthest south (only one south of Point Conception, meaning it has less intense upwelling and higher average pH), followed by Lompoc and Bodega Bay north of Point Conception which are in the same upwelling regime"),
                                                    style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))
                                                )),
                                     tabPanel(h4("Question 3"),
                                              h4(p("What physical conditions unique to the intertidal might affect the temperature and pH data collected by the sensors?",
                                                style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                                column(12, align="right",
                                                       checkboxInput("checkbox3", label = "Show answer", value = FALSE)),
                                                conditionalPanel(
                                                  condition = "input.checkbox3 == 1",
                                                  h4(p(em("zonation, isolation of pools, depth"),
                                                    style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))))
                  )))),
        
        tabItem(tabName = "data",
                h1("Lompoc Landing Data"),
                tabsetPanel(id = "lomdata",
                tabPanel(h4("Question 1"),
                          sidebarPanel(h4("1. What trends do you notice between pH and temperature for the Lompoc site?"),
                                       column(12, align="right",
                                              checkboxInput("checkbox_lompoc1", label = "Show answer", value = FALSE)),
                                       conditionalPanel(
                                         condition = "input.checkbox_lompoc1 == 1",
                                         h4(p(em("answer will go here :)"),
                                              style="text-align:left"))),
                                                    br(),
                                                    dateRangeInput(inputId = "date_range", 
                                                                   label = 'Filter tide by date',
                                                                   start = as.Date('2021-06-14') , end = as.Date('2021-10-06'))
                          ),
                                       
                mainPanel(highchartOutput("ph_ts_plot"))
               ),
                
                tabPanel(
                         h4("Question 2"),
                         sidebarPanel(h4("2. What do you notice about the scale of change for both pH and temp over hours? Days? Weeks/months?"),
                                      column(12, align="right",
                                             checkboxInput("checkbox_lompoc2",label = "Show answer", value = FALSE)),
                                      br(),
                                      conditionalPanel(
                                        condition = "input.checkbox_lompoc2 == 1",
                                        h4(p(em("The temperature tracks with the pH over the scale of days and weeks."),
                                             style="text-align:left"))),
                                      br(),
                                      selectInput(inputId = "ph_temp",
                                                  label = "Select pH or temperature",
                                                  choices = c("Temperature"="temp_c","pH"="p_h"))
                         ),
                         mainPanel(plotOutput(outputId = "q2plot"))),
                tabPanel(
                         h4("Question 3"),
                         sidebarPanel(
                           h4("3. Search up the weather for August 2 and compare it to the Lompoc data. What do you think could’ve caused the spikes in the data? What are some reasons why the temperature might’ve hit an extreme that day?"),
                           column(12, align="right",
                                  checkboxInput("checkbox_lompoc3", label = "Show answer", value = FALSE)),
                           conditionalPanel(
                             condition = "input.checkbox_lompoc3 == 1",
                             h4(p(em("answer will go here :)"),
                                  style="text-align:left"))),
                           br()),
                         mainPanel(highchartOutput("q3plot"))
                         ),
               tabPanel(
                 h4("Question 4"),
                 sidebarPanel(
                   h4("4. We expect Bodega to have the lowest temperature, so what is happening from August 26 — September 27 where Lompoc is colder? Use water temperature data to brainstorm ideas on seasonal temperature variation."),
                   column(12, align="right",
                          checkboxInput("checkbox_lompoc4", label = "Show answer", value = FALSE)),
                   conditionalPanel(
                     condition = "input.checkbox_lompoc4 == 1",
                     h4(p(em("answer will go here :)"),
                          style="text-align:left"))),
                     br()),
                 mainPanel(highchartOutput("q4plot"))
               )
                
                         
                )),
        
        tabItem(tabName = "compare",
                titlePanel("Compare and Contrast: All Sites"),
                h5(em("Figures on this page may take longer to load")),
                tabsetPanel(id = "com",
                            tabPanel(h4("Question 1"),
                                     sidebarPanel(
                                       h4("1. Compare data from the Lompoc site to Alegria and Bodega Bay. What are the overarching trends you can take away from the data?"),
                                              column(12, align="right",
                                                     checkboxInput("checkbox_compare1",label = "Show answer", value = FALSE)),
                                              br(),
                                              br(),
                                              conditionalPanel(
                                                condition = "input.checkbox_compare1 == 1",
                                                h4(p(em("Alegria has the highest pH and temp, then Lompoc, then Bodega Bay"),
                                                     style="text-align:left"))),
                                              br(),
                                              selectInput(inputId = "compare_tab1",
                                                          label = "Select pH or temperature",
                                                          choices = c("Temperature"="temp_c","pH"="p_h"))
                                              ),
                                     mainPanel(
                                              plotOutput(outputId = "tab1_plot"))),
                            
                            tabPanel(h4("Question 2"),
                                     tabsetPanel(
                                       tabPanel(h5("Question"),
                                     sidebarLayout(position = "right",
                                                   sidebarPanel(h3(p(strong(em(("Reminder: Our Research"))))),
                                                                h4(strong("Question:"), "What are the pH and temperature conditions across the California coastline?"),
                                                                br(),
                                                                h4(strong("Hypothesis:"), "pH will decrease with increasing latitude, and temperature will increase with decreasing latitude."),
                                                                br(),
                                                                h4(strong("Predictions:"), "Bodega Bay, the northernmost site, will experience the lowest pH and temperatures, whereas Alegria, the southernmost site, will experience the highest pH and temperatures.")
                                                   ),
                                                   mainPanel(
                                                     br(),
                                                     h4(p("2. Using the data in the 'Data Table' tab, what is the average pH and temperature for each site?",
                                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                                        column(12, align="right",
                                                               checkboxInput("checkbox_compare2", label = "Show answer", value = FALSE)),
                                                        br(),
                                                        br(),
                                                        conditionalPanel(
                                                          condition = "input.checkbox_compare2 == 1",
                                                          h4(p(em("answer will go here :)"),
                                                               style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")))))),
                                     tabPanel(h5("Data Table"),
                                       (DT::dataTableOutput("mytable1")))
                                       )),
                            
                            tabPanel(h4("Question 3"),
                                     fluidRow(
                                     column(width = 5,
                                            h4("Click a site"),
                                            leafletOutput(outputId = "map2", width = "100%", height = 600 )),
                                     column(width = 7,
                                            h4("3. Where do you see the most variation between temperature, tide, and pH? Discuss potential causes for variations in the data.",
                                                 column(12, align="right",
                                                        checkboxInput("checkbox_compare3",label = "Show answer", value = FALSE)),
                                                 br(),
                                                 br(),
                                                 conditionalPanel(
                                                   condition = "input.checkbox_compare3 == 1",
                                                   h4(p(em("Bodega bay dropoff is unclear, might’ve been an upwelling event due to strong winds; sensor is in low tide in Alegria and high tide in Lompoc and Bodega"),
                                                        style="text-align:left"))),
                                                 style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                            br(),
                                            selectInput(inputId = "compare_tab3",
                                                        label = "Select pH or temperature",
                                                        choices = c("Temperature"="temp_c","pH"="p_h")),
                                            selectInput(inputId = "compare_site",
                                                        label = "Please select a site",
                                                        choices = c("Alegria","Lompoc Landing", "Bodega Bay")),
                                            plotOutput("tab3_plot")))),
                            
                            tabPanel(h4("Question 4"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("4. Is there a correlation between tide and temperature? Are there any variations that affect the resulting pH of the site?"),
                                       column(12, align="right",
                                              checkboxInput("compare4", label = "Show answer", value = FALSE)),
                                       br(),
                                       br(),
                                       conditionalPanel(
                                         condition = "input.compare4 == 1",
                                         h4(p(em("answer will go here :)"),
                                              style="text-align:left"))),
                                       radioButtons(inputId = "compare_tab4",
                                                    label = "Please select an example",
                                                    choices = c('Example 1 (Alegria)' = "tab4_alegria",
                                                                'Example 2 (Lompoc Landing)' = "tab4_lompoc",
                                                                'Example 3 (Bodega Bay)' = "tab4_bodega"))),
                                     mainPanel(
                                              imageOutput("tab4_img"))
                                       )))
                            ),
        
        
        # conclusion & global implications tab content
        tabItem(tabName = "conclusions",
                h1("Conclusions"),
                tabsetPanel(id="conclusiontabs",
                            tabPanel(h4("Your turn"),
                                     h4(p("1. What have you learned? Check your comprehension and summarize what you now know by creating a concept map using the following terms. Add to the map as you learn more about OA:",
                                      br(),
                                      br(),
                                      em("Fossil fuels, Eutrophication, Carbon emissions, Bicarbonate, Calcifying organisms, Ocean acidification, Intertidal, pH, Temperature, Tide cycle"),
                                      style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                      br(),
                                      p("2. Consider what you just learned: how would you design a research project as a follow-up to the current scientific study?",
                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                      br(),
                                      h4(p("3. How can the data collected by scientists be used to inform conservation and management efforts in coastal marine ecosystems?",
                                           style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))
)),
                            tabPanel(h4("Learn more"),
                                        h4(p(em("Click on each logo to explore the organization's website"))),
                                     a(img(height="50%", width="50%", src="oaplogo.png"), href="https://oceanacidification.noaa.gov/Home.aspx", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                                     br(),
                                     a(img(height="50%", width="50%", src="ccanlogo.jpg"), href="https://c-can.info/", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                                     br(),
                                     a(img(height="50%", width="50%", src="goa-onlogo.png"), href="http://www.goa-on.org/home.php", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                                     br(),
                                     a(img(height="50%", width="50%", src="OAIElogo.png"), href="https://www.oainfoexchange.org/index.html", style="text-align: center; display: block; margin-left: auto; margin-right: auto")))),
                
        
        # acknowledgements tab content
        tabItem(tabName="acknowledgements",
                h1("Acknowledgements", align = "center"),
                h4(p(strong("This app was developed by:"),
                     style="text-align: center"),
                   fluidRow(
                     column(width = 3,
                            h4(p(("Zoe Fung"),
                                 style="text-align: center"),
                               tags$img(src="zoe.jpg", style="display: block; margin-left: auto; margin-right: auto;", height=150,width=150))),
                     column(width = 3,
                            h4(p(("Aanchal Garg"),
                                 style="text-align: center"),
                            tags$img(src="aanchal.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height=150,width=150))),
                     column(width = 3,
                            h4(p(("Doris Liu"),
                                 style="text-align: center"),
                               tags$img(src="doris.jpg", style="display: block; margin-left: auto; margin-right: auto;", height=150,width=150))),
                     column(width = 3,
                            h4(p(("Amelia Ritger"),
                                 style="text-align: center"),
                               tags$img(src="amelia.jpg", style="display: block; margin-left: auto; margin-right: auto;", height=150,width=150))))),
                   br(),
                   h4(p(strong("With funding from UCSB Associated Students Coastal Fund"),
                        style="text-align: center"),
                      a(img(height="50%",width="50%",
                            src="coastalfund.jpeg"), href="https://coastalfund.as.ucsb.edu"), style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                br(),
                h4(p(strong("Special thanks to:"),
                     br(),
                     em("Robert Goettler, Logan Kozal, Melissa M. Moore, and Markie Wordley"),
                     style = "text-align: center")),
                br(),
                h4(p(strong("Want to take a look at our code?")),
                tags$a(href="https://github.com/dorisliu1104/Outreach-Fall-ShinyApp", "View the Github repository here"),
                style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                br(),
                tags$img(src = "goletapier.JPG", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                column(width = 11,
                       h6(p("Source: Zoe Fung"),
                          style="text-align:right;color:darkgray")),
                br(),
                )))))

## Create the Server
server <- function(input, output) {
  
  ## map tab
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = 7, color = '#2e4057') %>%
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
                                    style = list("font-style" = "italic"))) %>%
      setView(lng = -121.6555, lat =  36.0, zoom = 6)
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
  
  
  # data_q4plot
  output$q4plot <- renderHighchart({
    y7 <- lompoc3$temp_c #set first y axis
    y8 <- lompoc3$p_h  #set second y axis
    y9 <- lompoc3$tide_height #set third y axis
    x <- lompoc3$date_time
    
    highchart() %>% 
      hc_add_series(data = y7, dashStyle="solid", name = "Temperature") %>% #plot temp
      hc_add_series(data = y8, yAxis = 1, name = "pH") %>% #plot pH
      hc_add_series(data = y9, yAxis = 2, name = "Tide") %>% #plot tide height
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

  ## tab 1 plots
  output$tab1_plot <- renderPlot({
    ggplot(comdata, aes(x=date_time, y=get(input$compare_tab1), group=site)) + #plot pH here
      geom_line(aes(color=site, alpha=site), size=0.7) + #make it a line chart
      geom_smooth(aes(color=site), method="loess", span=0.1) + #plot trend line for each site
      scale_color_manual(values = pal) + #color lines by custom site color palette
      scale_x_datetime(breaks = scales::date_breaks("1 week"), 
                       labels = date_format("%m/%d %H:%m")) + #change x axis to make it look cleaner - each tick is one week, display month/day hour/minute
      xlab("Date time") + #change x axis label
      ylab(ifelse(input$ph_temp == "temp_c", "Temperature", "pH")) + #change y axis label
      theme_bw() +
      theme(#legend.position = "none", #remove legend
        axis.text.x=element_text(angle=45, vjust = 1, hjust=1, size=12), #adjust x axis text format
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=12), #adjust y axis text format
        axis.title.y=element_text(size=15)) +
      scale_alpha_manual(values=c(0.5,0.5,1))
  })
  
  # our research slideshows
  output$bodegabay <- renderSlickR({
    imgs <- list.files("www/Bodega", pattern=".jpg", full.names = TRUE)
    slickR(imgs)
  })
  
  output$lompoc <- renderSlickR({
    imgs <- list.files("www/Lompoc", pattern=".jpg", full.names = TRUE)
    slickR(imgs)
  })
  
  output$alegria <- renderSlickR({
    imgs <- list.files("www/Alegria", pattern=".jpg", full.names = TRUE)
    slickR(imgs)
  })
  
  ## tab 2 table output
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(data_summary_table)
  })
  
  gps_radius <- c(15,8,11)
  
  ## tab 3 map
  output$map2 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = ~gps_radius, popup = ~popup_info, color = ~ifelse(input$compare_tab3 == "temp_c", "#D55E00", "#009E73")) %>%
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
  
  # tab 3 plot
  siteFiltered <- reactive({
    ph_clean_final %>% 
    dplyr::filter(site== input$compare_site) %>% 
    unite("date_time", "date", "time", sep="\ ", remove = FALSE) %>%
    mutate(date_time=ymd_hms(date_time))
  })
  
  output$tab3_plot <- renderPlot({
    ggplot(siteFiltered(), aes(x=date_time, y=get(input$compare_tab3)))+ #plot pH here
      geom_line(size = 0.7,color = ifelse(input$compare_tab3 == "temp_c", "#D55E00","#009E73" )) + #make it a line chart
      geom_smooth(method="loess", span=0.1) + #plot trend line for each site
      #scale_color_manual(values = ifelse(input$ph_temp == "temp_c", "#D55E00","#009E73" )) + #color lines by custom site color palette
      scale_x_datetime(breaks = scales::date_breaks("1 week"), 
                       labels = date_format("%m/%d %H:%m")) + #change x axis to make it look cleaner - each tick is one week, display month/day hour/minute
      xlab("Date time") + #change x axis label
      ylab(ifelse(input$compare_tab3 == "temp_c", "Temperature", "pH")) + #change y axis label
      theme_bw() +
      theme(#legend.position = "none", #remove legend
        axis.text.x=element_text(angle=45, vjust = 1, hjust=1, size=12), #adjust x axis text format
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=12), #adjust y axis text format
        axis.title.y=element_text(size=15))
  })
  
  
  #tab4 output
  output$tab4_img <- renderImage({
    filename <- normalizePath(file.path('./www/', paste(input$compare_tab4, ".png", sep="")))
    list(src = filename, height = 400, width = 600)
  }, deleteFile = FALSE
  )
  
  
}

## Combine the UI and the server
shinyApp(ui = ui, server = server)