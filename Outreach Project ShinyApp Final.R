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
library(shinyWidgets)

# #add functionality to publish app
# library(rsconnect)
# library(BiocManager)
# options(repos = BiocManager::repositories())

## Read the Updated Data
ph_clean_final <- read_csv(here("data", "ph_clean_final.csv"))
ph_clean_final <- ph_clean_final %>%
  arrange(date_time) %>% 
  mutate(date=mdy(date)) %>% 
  mutate(date_highchart = as.character(date_time, format="%d %B %Y %H:%M"))

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
  mutate(date_time=ymd_hms(date_time)) %>% 
  arrange(ymd(date))

lompoc_day <- filter(lompoc, between(date, as.Date("2021-07-25"), as.Date("2021-07-26")))
lompoc2 <- filter(lompoc, between(date, as.Date("2021-07-30"), as.Date("2021-08-04")))
## lompoc question 3
lompoc3 <- filter(lompoc, between(date, as.Date("2021-08-26"), as.Date("2021-09-27"))) ## lompoc question 4
lompoc4 <- filter(lompoc, between(date, as.Date("2021-06-19"), as.Date("2021-06-23")))
alegria4 <- dplyr::filter(ph_clean_final, site=="Alegria") %>% 
  filter(between(date, as.Date("2021-08-08"), as.Date("2021-08-12")))
bodega4 <- dplyr::filter(ph_clean_final, site=="Bodega Bay") %>% 
  filter(between(date, as.Date("2021-07-15"), as.Date("2021-07-19")))
datafiles <- list(lompoc_day, lompoc2, lompoc)

## create dataframe for compare and contrast plots

comdata <- ph_clean_final %>% 
  unite("date_time", "date", "time", sep="\ ") %>%
  mutate(date_time=ymd_hms(date_time)) %>% #apply lubridate to date/time column
  mutate(date_clean = format(as.POSIXct(date_time), "%d %B %Y %H:%M"))

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
        menuItem("Research Overview", tabName = "research", icon = icon("docker",lib="font-awesome")),
        menuItem("Lompoc Landing Data", tabName = "data", icon = icon("anchor",lib="font-awesome")),
        menuItem("Compare and Contrast", tabName = "compare", icon = icon("chart-bar",lib="font-awesome")),
        menuItem("Conclusion", tabName = "conclusions", icon = icon("fish", lib = "font-awesome")),
        menuItem("Glossary", tabName = "glossary", icon = icon("book-open", lib="font-awesome")),
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
                                     fluidRow(
                                       column(width = 12,
                                     h4(p("The ocean is a carbon sink that absorbs around 30% of the carbon dioxide (CO2) released to the atmosphere by human activities, such as burning fossil fuels and deforestation.
                                          Ocean pH is normally around 8.1, but the pH drops when CO2 dissolves in seawater.",
                                          br(),
                                          br(),
                                          tags$img(src = "OCcycle2.png", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                          column(width = 12,
                                                 h6(p(em("Figure by ",
                                                         tags$a(href="http://www.necan.org/overview", "Northeast Coastal Acidification Network"), ".")),
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
                                          HTML('<center><iframe width="560" height="315" src="https://www.youtube.com/embed/gZGj0BbDT38" title="YouTube video player" target="_blank" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></center>')),
                                        br(),
                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     column(width = 12,
                                            br(),
                                            box(title = h4(strong("Comprehension check"), align="center"),
                                                solidHeader = T,
                                                collapsible = T,
                                                collapsed = T,
                                                width = "50%",
                                                column(12, align="center",
                                                       h4("Why does the ocean's pH drop when it absorbs carbon dioxide from the atmosphere?")))),
                                     )),
                            ###
                            tabPanel(h4("The effect on calcifying organisms"),
                                     fluidRow(
                                       column(width = 12,
                                     h4(p("The removal of carbonate and addition of hydrogen ions in seawater through the process of OA has serious consequences for many marine organisms, especially marine calcifiers.
                                       Calcifying marine organisms use carbonate to make shells (calcium carbonate). Therefore, the removal of carbonate from seawater makes it harder for these organisms to form hard, strong shells.
                                       Furthermore, the addition of hydrogen drops the seawater pH and corrodes calcifier's shells.
                                       Corals, crabs, snails, and many other organisms that use carbonate to build their shells and skeletons are thus affected, and the effects cascade across the food chain.",
                                       br(),
                                       br(),
                                       tags$img(src = "corealreef.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                       column(width = 12,
                                              h6(p(em("Photo composite by ",
                                                      tags$a(href="https://coralreefwatch.noaa.gov", target="_blank", "National Oceanic and Atmospheric Administration"), ".")),
                                                 style="text-align:center;color:darkgray")),
                                       br(),
                                       br(),
                                       em("Watch this short video to learn more about the effects of OA on calcifying organisms:"),
                                       br(),
                                       br(),
                                       HTML('<center><iframe width="560" height="315" src="https://www.youtube.com/embed/aG3n1fAa7vk" title="The Acid Test" target="_blank" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></center>')),
                                       style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     column(width = 12,
                                            br(),
                                            box(title = h4(strong("Comprehension check"), align="center"),
                                                solidHeader = T,
                                                collapsible = T,
                                                collapsed = T,
                                                width = "50%",
                                                column(12, align="center",
                                                       h4("Why is the removal of carbonate ions from the ocean bad for calcifying marine organisms?")))),
                                     )),
                            ###
                            tabPanel(h4("Along the coastline"),
                                     fluidRow(
                                       column(width = 12,
                                     h4(p("OA occurs at different rates in different ecosystems.
                                          Coastal environments in particular experience more rapid rates of acidification due to local processes, often linked to human activities, that alter water chemistry. 
                                          For example, eutrophication (nutrient runoff) can cause algal blooms that release CO2 when the algae die and decompose.",
                                          br(),
                                          br(),
                                          tags$img(src = "algalbloom.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                          column(width = 12,
                                                 h6(p(em("Photo by ",
                                                         tags$a(href="https://www.americanrivers.org/2017/10/ohios-maumee-river-green/", target="_blank", "Andy Morrison, 2017"), ".")),
                                                    style="text-align:center;color:darkgray")),
                                          br(),
                                          "Not all changes in water chemistry are caused by humans, however.
                                          Coastal upwelling is a process that occurs along the California coastline, and elsewhere globally, whereby cold, nutrient-rich water is transported from the ocean's depths to the ocean's surface via surface winds.
                                          Because the ocean acts as a carbon sink, the transport of water from the depth also brings along more acidic water to the surface and into shallow coastal ecosystems.",
                                          br(),
                                          br(),
                                          tags$img(src = "upwelling.jpg", style="display: block; margin-left: auto; margin-right: auto;", height="90%",width="90%"),
                                          column(width = 12,
                                                 h6(p(em("Figure by ",
                                                         tags$a(href="https://oceanservice.noaa.gov/education/tutorial_currents/03coastal4.html", target="_blank", "National Oceanic and Atmospheric Administration"), ".")),
                                                    style="text-align:center;color:darkgray")),
                                          br()),
                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     column(width = 12,
                                            br(),
                                            box(title = h4(strong("Comprehension check"), align="center"),
                                                solidHeader = T,
                                                collapsible = T,
                                                collapsed = T,
                                                width = "50%",
                                                column(12, align="center",
                                                       h4("What happens to sea surface temperature and pH during upwelling?")))),
                                     )),
                            ### 
                            tabPanel(h4("In the intertidal"),
                                     fluidRow(
                                       column(width = 12,
                                     h4(p("The intertidal zone is a highly variable and dynamic environment.
                                          Organisms that live in the intertidal are subject to the extreme conditions resulting from waves and the daily rise and fall of the tides.
                                          When the tide goes out, organisms must cope with increases in salinity and temperature, reduced oxygen availability, and even exposure to the air.
                                          pH also fluctuates daily in the intertidal zone, thanks to photosynthesis and respiration by marine organisms that changes CO2 concentrations in the tidepools at low tides.",
                                          br(),
                                          br(),
                                          tags$img(src = "intertidal-fig.jpg", style="display: block; margin-left: auto; margin-right: auto;", height="90%",width="90%"),
                                          column(width = 12,
                                                 h6(p(em("Figure by ",
                                                         tags$a(href="https://www.frontiersin.org/articles/10.3389/fmars.2021.667168/full", target="_blank", "Kunze et al 2021"), ".")),
                                                    style="text-align:center;color:darkgray")),
                                          br(),
                                          br(),
                                          "As the figure above demonstrates, organisms in the intertidal zone are affected by both natural and anthropogenic (human) influences. For example, when the intertidal zone acidifies even further under OA conditions, marine organisms become stressed, which can negatively affect their development, behavior, and physiology.",
                                          br(),
                                          br(),
                                          tags$img(src = "pachy-lol.jpg", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                          column(width = 12,
                                                 h6(p(em("Photo by Amelia Ritger")),
                                                    style="text-align:center;color:darkgray")),
                                          br()),
                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     column(width = 12,
                                            br(),
                                            box(title = h4(strong("Comprehension check"), align="center"),
                                                solidHeader = T,
                                                collapsible = T,
                                                collapsed = T,
                                                width = "50%",
                                                column(12, align="center",
                                                       h4("Examine the above photo of the shore crab (", em("Pachygraspus crasspies"), "). Describe two natural and two anthropogenic factors affecting the crab in the intertidal zone.")))),
                                     )),
                            ###
                            tabPanel(h4("Your turn"),
                                     h4(p(strong("Using the information you just learned, answer the following questions."
                                     )),
                                     style="text-align:center"),
                                     br(),
                                     h4(p("1. What do you know about ocean acidification? Use a",
                                          tags$a(href="https://www.menti.com/uijfevceik", target="_blank", "Mentimeter"), 
                                          "to submit the words or phrases that come to mind when you think of ocean acidification.",
                                          br(),
                                          br(),
                                          em("View your class's Mentimeter",
                                             tags$a(href="https://www.mentimeter.com/s/29f86468afbfa7aa26f27554857d25d9/974479e8d82c", target="_blank", "here.")),
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     br(),
                                     h4(p("2. What is causing ocean acidification? What are its effects?",
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     br(),
                                     h4(p("3. What makes the intertidal zone uniquely affected by ocean acidification?",
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")))
                            
                )),
        # our research tab content
        
        tabItem(tabName = "research",
                h1("Research Overview"),
                fluidRow(
                  column(6,
                         h3("Research methods"),
                         tabsetPanel(id="about_the_sites_tabs",
                                     tabPanel(h4("Background"),
                                              h4("California has two upwelling regimes, geographically separated by Point Conception:", strong("strong upwelling in the north"), ", where marine environments are subject to persistent colder temperatures and low pH conditions, and", strong("weak upwelling in the south"), ", where marine environments are subject to persistent warmer temperatures and higher pH conditions."),
                                              br(),
                                              h4("However, the current environmental conditions of the intertidal zone are virtually unexplored in California."),
                                              br(),
                                              tags$img(src = "caupwelling-noaa.jpg", align = "center",height="100%",width="100%"),
                                              br(),
                                              h5(p(em("California sea surface water temperature, by",
                                                      tags$a(href="https://www.ospo.noaa.gov/Products/ocean/sst/contour/", target="_blank", "National Oceanic and Atmospheric Administration"), "."))),
                                     ),
                                     tabPanel(h4("Question"),
                                              h4(strong("Question:"), "What are the pH and temperature conditions across the California coastline?"),
                                              br(),
                                              h4(strong("Hypothesis:"), "pH will decrease with increasing latitude, and temperature will increase with decreasing latitude."),
                                              br(),
                                              h4(strong("Predictions:"), "Bodega Bay, the northernmost site, will experience the lowest pH and temperatures, whereas Alegria, the southernmost site, will experience the highest pH and temperatures.")
                                     ),
                                     tabPanel(h4("Methods"),
                                              h4(strong("Sensor deployments:"), "pH and temperature sensors were deployed in the rocky intertidal zone at three sites - Bodega Bay, Lompoc Landing, and Alegria - from May 2021 until November 2021. Sensors collected temperature and pH measurements every 15 minutes."),
                                              br(),
                                              h4(strong("Site selection:"), "Sites were selected because they 1) had been studied before, which would allow us to build upon historical datasets, 2) capture variation in oceanographic conditions attributed to California's two upwelling regimes, and 3) limit public access, which protects the valuable sensor equipment."),
                                              br(),
                                              h4(strong("Site map")),
                                              leafletOutput(outputId = "map", width = "100%", height = 400),
                                              br(),
                                              h4(em("Take a look at",
                                                    tags$a(href="https://www.windy.com/?36.364,-121.838,7,i:pressure", target="_blank", "Windy.com"), "to explore more physical differences between these three sites and across the California coastline"))),
                                     tabPanel(h4("Site pictures"),
                                              fluidRow(
                                                column=12,
                                                align="center",
                                              h4("Bodega Bay",
                                                 style="text-align:center"),
                                              HTML('<left><img src="bodega_site.jpg" width="45%"></left>'),
                                              HTML('<right><img src="bodega-sun.jpg" width="45%"></right>'),
                                              br(),
                                              br(),
                                              p(" ", style = "margin-bottom: -10px;"),
                                              actionButton('bodega1', 'Zoom in on Bodega Bay', class="btn-xs", style='font-size:75%'),
                                              br(),
                                              br(),
                                              h4("Lompoc Landing",
                                                 style="text-align:center"),
                                              HTML('<left><img src="lol_site.jpg" width="45%"></left>'),
                                              HTML('<right><img src="lol-horizon.jpg" width="45%"></right>'),
                                              br(),
                                              br(),
                                              p(" ", style = "margin-bottom: -10px;"),
                                              actionButton('lompoc1', 'Zoom in on Lompoc Landing', class="btn-xs", style='font-size:75%'),
                                              br(),
                                              br(),
                                              h4("Alegria",
                                                 style="text-align:center"),
                                              HTML('<left><img src="alegria2_site.jpg" width="45%"></left>'),
                                              HTML('<right><img src="alg-horizon.jpg" width="45%"></right>'),
                                              br(),
                                              br(),
                                              p(" ", style = "margin-bottom: -10px;"),
                                              actionButton('alegria1', 'Zoom in on Alegria', class="btn-xs", style='font-size:75%'),
                                     ))
                                     #tabPanel(h4("Site map")
                                     #leafletOutput(outputId = "map", width = "100%", height = 600 )
                         )),
                  
                  column(6, 
                         h3("Your turn"),
                         tabsetPanel(id="our_research_tabs",
                                     tabPanel(h4("Question 1"),
                                              column(width = 12,
                                                     br(),
                                                     box(title = h4(p("Examine the pictures from the three study sites. What are some visual differences between each environment?"),
                                                                    h5(em("Click the '+' to reveal some of our answers."), align="right"),
                                                                    style="color:black"),
                                                         solidHeader = T,
                                                         collapsible = T,
                                                         collapsed = T,
                                                         width = "50%",column(12, align="center",
                                                                              h4(em("Alegria is fairly flat overall, with a gentle slope throughout. The sensor at Alegria is fairly low in the intertidal zone, but still underwater at low tide."),
                                                                                 br(),
                                                                                 br(),
                                                                                 em("Lompoc Landing has a shelf-like geological structure that creates large, shallow tidepools. The sensor at Lompoc Landing is fairly high in the intertidal zone, but still underwater at low tide."),
                                                                                 br(),
                                                                                 br(),
                                                                                 em("Bodega Bay has the most contoured, rugose intertidal structure, with a lot of surface variation. The sensor at Bodega Bay is exposed to air at low tide."))))),
                                              ),
                                     tabPanel(h4("Question 2"),
                                              column(width = 12,
                                                     br(),
                                                     box(title = h4(p("Why were these three sites selected? How are these sites similar? How are they different?"),
                                                                    h5(em("Click the '+' to reveal some of our answers."), align="right"),
                                                                    style="color:black"),
                                                         solidHeader = T,
                                                         collapsible = T,
                                                         collapsed = T,
                                                         width = "50%",column(12, align="center",
                                                                              h4(em("Sites were selected because they have historical pH data, they are located in regions North and South of Point Conception, and they are in locations that keep the sensors safe from vandalism."),
                                                                                 br(),
                                                                                 br(),
                                                                                 em("Similarities: Lompoc Landing and Alegria are geographically near each other, and so they may be exposed to similar atmospheric conditions. Bodega Bay and Lompoc Landing are both in the strong upwelling regime, and so they may experience more similar oceanographic conditions."),
                                                                                 br(),
                                                                                 br(),
                                                                                 em("Differences: Alegria is the furthest south and it is south of Point Conception, meaning it is exposed to less intense upwelling and likely has less acidic conditions than the other two sites. We expect that although Alegria is closer to Lompoc Landing than Bodega Bay, Point Conception will create significant differences in the oceanographic conditions at the two locations."))))),
                                              ),
                                     tabPanel(h4("Question 3"),
                                              column(width = 12,
                                                     br(),
                                                     box(title = h4(p("What physical conditions unique to the intertidal might affect the temperature and pH data collected by the sensors?"),
                                                                    h5(em("Click the '+' to reveal some of our answers."), align="right"),
                                                                    style="color:black"),
                                                         solidHeader = T,
                                                         collapsible = T,
                                                         collapsed = T,
                                                         width = "50%",column(12, align="center",
                                                                              h4(em("Vertical zonation: at high tide, locations higher in the intertidal are exposed to air longer, or separated from the ocean longer, and would get warmer and have lower pH."),
                                                                                 br(),
                                                                                 br(),
                                                                                 em("Isolation of pools: locations that are more isolated from the ocean at low tide would get warmer and have lower pH than locations more connected to the ocean at low tide."),
                                                                                 br(),
                                                                                 br(),
                                                                                 em("Depth: at low tide, deeper pools would get less warm than shallow pools of the same surface area and at the same height in the intertidal zone."))))),
                                              )
                         )))),
        
        tabItem(tabName = "data",
                h1("Lompoc Landing Data"),
                tabsetPanel(id = "lomdata",
                            tabPanel(h4("Data overview"),
                                     fluidRow(
                                       column(width = 12, align="center",
                                              h4(p("In this section, you will explore the data we collected from the sensor deployed at Lompoc Landing. Because the sensor deployed at Lompoc Landing was high in the intertidal zone and always underwater, these data provide an excellent opportunity to learn about the range of environmental conditions organisms must navigate in the intertidal zone.",
                                          br(),
                                          br(),
                                          "Each question will have an associated graph (such as the one below), which you can interact with to analyze different aspects to the data.",
                                          br(),
                                          br(),
                                          "Move your cursor over each of the lines to see the value of each variable at any particular point in time.",
                                          highchartOutput("lompoc_intro", width="75%"),
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                          h4(p("The X axis will show the date (or date and time), and the Y axis will show the variable(s) of interest. Time is in military time (e.g., 8:00 = 8:00 AM and 20:00 = 8:00 PM), temperature is in degrees Celsius, and tide values are height in feet. The date increases the further you move right along the X axis, and the variable value increases the further you move up along the Y axis. More negative tide heights are lower tides, and lower pH values are more acidic conditions.",
                                           style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))
                                     ))),
                            tabPanel(h4("Question 1"),
                                     sidebarPanel(h4(strong("1. Describe the trends between pH, temperature, and tide heights at Lompoc Landing. How does time of day influence the patterns?")),
                                                  #"Here is a graph of the pH, temperature, and tide heights at Lompoc Landing from June until October 2021. Change the dates to view the data at different time scales.",
                                                  h5(em("Hint: Filter the data to look at a few days early in a month, and a few days later in the month. At high tide, what is happening to the temperature? The pH? Conversely, at low tide, what is happening to the temperature? The pH? Keep an eye on the time - low tides at nighttime may show very different trends than low tides midday!")),
                                                  column(12, align="right",
                                                         checkboxInput("checkbox_lompoc1", label = "Show answer", value = FALSE)),
                                                  br(),
                                                  dateRangeInput(inputId = "date_range", 
                                                                 label = 'Filter by date',
                                                                 start = as.Date('2021-06-14'),
                                                                 end = as.Date('2021-10-06'),
                                                                 min = as.Date('2021-06-14'),
                                                                 max = as.Date('2021-10-06')),
                                                  conditionalPanel(
                                                    condition = "input.checkbox_lompoc1 == 1",
                                                    h5(p(em("Answer: At low tide, pH drops and temperature starts to increase. Temperature only decreases once the tide comes back in. pH tends to drop at low tide regardless of time of day, while the greatest temperature spikes occur when low tide happens when the sun is out."),
                                                         style="text-align:left")))
                                     ),
                                     mainPanel(highchartOutput("ph_ts_plot"))
                            ),
                            
                            tabPanel(
                              h4("Question 2"),
                              sidebarPanel(h4(strong("2. What do you notice about the scale of change for pH and temperature over days? Weeks? Months?")),
                                           h5(em("Hint: Look at the direction of the blue trend line - is it generally increasing? Decreasing? Staying flat? How does the blue trend line compare to the raw data (red for temperature, green for pH)?")),
                                           column(12, align="right",
                                                  checkboxInput("checkbox_lompoc2",label = "Show answer", value = FALSE)),

                                           br(),
                                           selectInput(inputId = "ph_temp",
                                                       label = "Select a variable",
                                                       choices = c("Temperature"="temp_c","pH"="p_h")),
                                           selectInput(inputId = "week_day",
                                                       label = "Select a time scale",
                                                       choices = c("days", "weeks" , "months")),
                                           conditionalPanel(
                                             condition = "input.checkbox_lompoc2 == 1",
                                             h5(p(em("Answer: The greatest variation in pH and temperature is observed at the daily scale. At the weekly scale, temperature and pH don't generally change a lot. On the scale of months, it appears that there are time periods, on the scale of weeks, where pH or temperature is generally high or generally low. Also, looking at the monthly scale makes the daily variation all the more apparent!"),
                                                  style="text-align:left")))
                              ),
                              mainPanel(plotOutput(outputId = "q2plot"))),
                            tabPanel(
                              h4("Question 3"),
                              sidebarPanel(
                                h4(strong("3.", 
                                   tags$a(href="https://www.wunderground.com/history/daily/us/ca/santa-maria/KSMX/date/2021-8-1", target="_blank", "Look up the weather"), "for August 1st and August 3rd, 2021, and compare it with the data collected by the sensor at Lompoc Landing around those same dates. What do you think could have caused the differences in seawater temperature and pH between those two dates?")),
                                h5(em("Hint: Look at air temperature and wind speed when the temperature was at its peak on each day.")),
                                column(12, align="right",
                                       checkboxInput("checkbox_lompoc3", label = "Show answer", value = FALSE)),
                                br(),
                                conditionalPanel(
                                  condition = "input.checkbox_lompoc3 == 1",
                                  h5(p(em("Answer: Warmer air temperatures could mean warmer waters for a tidepool. Stronger winds could mean higher waves crashing into tidepools, even at low tide. On August 1st, the air temperature was higher (72F compared to 67F) and the wind speed was slightly stronger (14 mph vs 10 mph). The air temperature difference was likely a contributing factor to the difference in maximum water temperature measured between both days."),
                                       style="text-align:left")))),
                              mainPanel(highchartOutput("q3plot"))
                            )
                            # tabPanel(
                            #   h4("Question 4"),
                            #   sidebarPanel(
                            #     h4("4. We expect Bodega to have the lowest temperature, so what is happening from August 26 — September 27 where Lompoc is colder? Use water temperature data to brainstorm ideas on seasonal temperature variation."),
                            #     column(12, align="right",
                            #            checkboxInput("checkbox_lompoc4", label = "Show answer", value = FALSE)),
                            #     conditionalPanel(
                            #       condition = "input.checkbox_lompoc4 == 1",
                            #       h4(p(em("answer will go here :)"),
                            #            style="text-align:left"))),
                            #     br()),
                            #   mainPanel(highchartOutput("q4plot"))
                            # )
                            
                            
                )),
        
        tabItem(tabName = "compare",
                h1("Compare and Contrast: All Sites", align = "left"),
                h5(em("Please be patient: figures on this page may take longer to load")),
                tabsetPanel(id = "com",
                            tabPanel(h4("Data overview"),
                                     fluidRow(
                                       column(width = 12, align="center",
                                              h4(p("In this section, we will now zoom out and explore the data we collected from the sensors deployed at all three sites: Alegria, Lompoc Landing, and Bodega Bay.",
                                                   #leafletOutput(outputId = "map_intro", width = "75%", height = 300 ),
                                                   style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                              a(img(height="50%", width="50%", src="sitemap.png"), style="text-align: center; display: block; margin-left: auto; margin-right: auto;"),
                                              h4(p("As you learned in the Research Overview, the California coastline has two upwelling regimes: strong upwelling North of Point Conception, and weak upwelling South of Point Conception. This means that we expect Alegria to have generally higher pH conditions than both Lompoc Landing and Bodega Bay. Likewise, because of the latitudinal gradient of temperature along the coastline, we expect Bodega Bay to have the coldest water temperatures and Alegria to have the warmest water temperatures.",
                                                   br(),
                                                   br(),
                                                   "Work through the following questions to learn if the sensor data follows our expectations!",
                                                   style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))
                                       ))),
                            tabPanel(h4("Question 1"),
                                     fluidRow(
                                       column(width = 5,
                                              h5(em("Click a circle to view the site's average conditions."),
                                                 style="text-align:center"),
                                              leafletOutput(outputId = "map3", width = "100%", height = 600 )),
                                       column(width = 7,
                                              h4(p(strong("1. Compare the data at all three sites. What general patterns do you see in the data?"),
                                                   h5(em("Hint: On the map to the left, click on each site's bubble to view the overall average values for each variable. On the plot below, examine the trend lines (bold) of each site to compare overall trends in the data, and examine the raw data (transparent lines) of each site to compare finer details in the data, such as maximum and minimum values for each variable.")),
                                                 h5(selectInput(inputId = "compare_tab1",
                                                             label = "Select a variable",
                                                             choices = c("Temperature"="temp_c","pH"="p_h"))))),
                                                 style="text-align:left;background-color:white;padding:15px;border-radius:10px"),
                                              column(7, align="right",
                                                     checkboxInput("checkbox_compare1",label = "Show answer", value = FALSE),
                                                     style="color:black"),
                                              br(),
                                       column(7,
                                              plotOutput("tab1_plot"),
                                              conditionalPanel(
                                                condition = "input.checkbox_compare1 == 1",
                                                h5(p(em("Answer: Looking at the average values at each site on the map, it appears that the data follow the predictions: Alegria is on average warmer and has a higher pH, and Bodega Bay is on average colder and has a lower pH.  Looking at the trend lines on the plot, the answer isn't as clearcut for pH, and even temperature."),
                                                     br(),
                                                     br(),
                                                     em("For temperature, the trendlines generally follow the predictions (Alegria is higher than Lompoc and Bodega Bay), but sometimes Lompoc Landing is colder than Bodega Bay and warmer than Alegria. For pH, Alegria is sometimes the most acidic location, and Bodega Bay is sometimes the least acidic location!"),
                                                     style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")))
                                              ))),
                            
                            tabPanel(h4("Question 2"),
                                     fluidRow(
                                       column(width = 8,
                                              h4(p(strong("2. Look at the following table and consider: How do these data align with the study predictions?"),
                                                   h5(em("Hint: If you do not remember the study predictions, take a look at the box on the right.")))),
                                                   style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                       
                                              column(8, align="right",
                                                     checkboxInput("checkbox_compare2", label = "Show answer", value = FALSE),
                                              (div(DT::dataTableOutput("mytable1", width = "100%"), style = "font-size: 90%; width: 100%")),
                                              h6(p("Move your cursor to scroll across the table")),
                                              conditionalPanel(
                                                condition = "input.checkbox_compare2 == 1",
                                                h5(p(em("The data do align with the study predictions. Bodega Bay, the furthest north, has the lowest minimum temperature, lowest average temperature, and lowest average pH. Alegria, the furthest south, has the highest average temperature and highest average pH."),
                                                     br(),
                                                     br(),
                                                     em("However, there is nuance to this conclusion. For example, Lompoc Landing has the highest maximum temperature and the lowest minimum pH, and Bodega Bay has the highest maximum pH!"),
                                                     style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")))),
                                       
                                       column(width = 4,
                                              box(title = "Reminder: Research Overview",
                                                  solidHeader = T,
                                                  collapsible = T,
                                                  collapsed = T,
                                                  width = "50%",
                                                  h5(strong("Question:"), "What are the pH and temperature conditions across the California coastline?"),
                                                  br(),
                                                  h5(strong("Hypothesis:"), "pH will decrease with increasing latitude, and temperature will increase with decreasing latitude."),
                                                  br(),
                                                  h5(strong("Predictions:"), "Bodega Bay, the northernmost site, will experience the lowest pH and temperatures, whereas Alegria, the southernmost site, will experience the highest pH and temperatures."))),
                                       
                                    

                                       )),
          
                             # sidebarLayout(position = "right",
                                     #               sidebarPanel(h3(p(strong(em(("Reminder: Our Research"))))),
                                     #                            h4(strong("Question:"), "What are the pH and temperature conditions across the California coastline?"),
                                     #                            br(),
                                     #                            h4(strong("Hypothesis:"), "pH will decrease with increasing latitude, and temperature will increase with decreasing latitude."),
                                     #                            br(),
                                     #                            h4(strong("Predictions:"), "Bodega Bay, the northernmost site, will experience the lowest pH and temperatures, whereas Alegria, the southernmost site, will experience the highest pH and temperatures.")
                                     #                          ),
                                     #               mainPanel(
                                     #                 br(),
                                     #                 h4(p("2. Look at the following table and consider: How do these data align with the study predictions?",
                                     #                      style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     #                 column(12, align="right",
                                     #                        checkboxInput("checkbox_compare2", label = "Show answer", value = FALSE)),
                                     #                 conditionalPanel(
                                     #                   condition = "input.checkbox_compare2 == 1",
                                     #                   h4(p(em("answer will go here :)"),
                                     #                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"))),
                                     #                 (div(DT::dataTableOutput("mytable1"), style = "font-size: 90%; width: 75%")))
                                     # )),
                                   
                
                            tabPanel(h4("Question 3"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4(strong("3. What site has the most variation in temperature and pH? What are some possible causes for such variation?"),
                                            h5(em("Hint: Each site has the same y axis for each variable. Toggle between each site to examine which site has the greatest difference between high and low peaks."))),
                                         column(12, align="right",
                                                checkboxInput("checkbox_compare3", label = "Show answer", value = FALSE)),
                                         br(),
                                         br(),
                                         selectInput(inputId = "compare_tab3",
                                                    label = "Select a variable",
                                                    choices = c("Temperature"="temp_c","pH"="p_h")),
                                         selectInput(inputId = "compare_site",
                                                     label = "Select a site",
                                                     choices = c("Alegria","Lompoc Landing", "Bodega Bay")),
                                         conditionalPanel(
                                           condition = "input.checkbox_compare3 == 1",
                                           h5(p(em("Answer: Lompoc Landing has the most daily variation in both temperature and pH. This could be due to the sensor placement at Lompoc Landing - since the sensor was located high in the intertidal zone, it may have had more time to experience temperature rises and pH drops before the tide returned. The temporally variable spikes in pH and temperature at other sites could be due to local conditions, such as heatwaves, or strong winds and waves."),
                                                style="text-align:left")))
                                              ),
                                         mainPanel(plotOutput("tab3_plot"))
                                         )),
                            
                            tabPanel(h4("Question 4"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4(strong("4. Is there a relationship between tide and temperature? How are tide and temperature related to pH at each site?"),
                                            h5(em("Hint: Toggle between each site, and make note of: When temperature is going up, what is happening to the pH? Conversely, when temperature is going down, what is happening to the pH? Does one measurement generally start to rise or drop before the other?"))),
                                         column(12, align="right",
                                                checkboxInput("compare4", label = "Show answer", value = FALSE)),
                                         br(),
                                         br(),
                                         radioButtons(inputId = "compare_tab4",
                                                      label = "Select an example",
                                                      choices = c('Example 1 (Alegria)',
                                                                  'Example 2 (Lompoc Landing)',
                                                                  'Example 3 (Bodega Bay)')),
                                         conditionalPanel(
                                           condition = "input.compare4 == 1",
                                           h5(p(em("Answer: Although each site has differences in the shape of the patterns of both temperature and pH, temperature and pH have an overall positive relationship across sites. When accounting for the tide height, the time of the lowest pH and temperature varies between locations: at Alegria, they track closely with tide and pH, and the lowest temperatures/pH occur around low tide; at Lompoc Landing, the lowest temperatures/pH occur just after low tide; and at Bodega Bay, the lowest temperatures/pH occur just before low tide."),
                                                style="text-align:left")))),
                                       mainPanel(
                                        highchartOutput("tab4_plot") )
                                     )))
        ),
        
        
        # conclusion & global implications tab content
        tabItem(tabName = "conclusions",
                h1("Final thoughts"),
                tabsetPanel(id="conclusiontabs",
                            tabPanel(h4("Summary"),
                                     h4(p("During this learning module, you have learned about one monitoring effort by scientists studying the progression of ocean acidification along the California coastline.",
                                        br(),
                                        br(),
                                        "To evaluate how California is being affected by ocean acidification, as well as other climate change-related impacts, we must continue to track pH and temperature (among other environmental factors, such as salinity and dissolved oxygen). Long-term monitoring efforts by scientists support effective management and conservation of marine ecosystems, in addition to helping us adapt to the consequences of ocean acidification now and in the future."),
                                        style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                     tags$img(src = "alg_sunset-wide.png", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                     column(width = 12,
                                            h6(p(em("Photo by Amelia Ritger")),
                                               style="text-align:center;color:darkgray")),
                                     br()
                            ),
                            tabPanel(h4("Your turn"),
                                     h4(p("1. What did you learn about OA? Check your comprehension and summarize what you now know by creating a concept map using the terms below. Add to the map as you continue to learn about OA. ",
                                          br(),
                                          br(),
                                          em("Anthropogenic, Bicarbonate, Calcifying organisms, Carbon emissions, Fossil fuels, Intertidal, Ocean acidification, pH, Temperature, Tide cycle, Upwelling"),
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                        br(),
                                        p("2. Consider what you just learned: How would you design a follow-up study? What is your question? What are your methods?",
                                          style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px"),
                                        br(),
                                        h4(p("3. How can the data collected by scientists be used to inform the conservation and management of marine ecosystems, such as the intertidal zone?",
                                             style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),
                                     tags$img(src = "pisaster_alegria.jpg", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                                     column(width = 12,
                                            h6(p(em("Photo by Amelia Ritger")),
                                               style="text-align:center;color:darkgray")),
                                     br())),
                            tabPanel(h4("Learn more"),
                                     h4(p(em("Click on each logo to explore the organization's website"))),
                                     a(img(height="50%", width="50%", src="oaplogo.png"), href="https://oceanacidification.noaa.gov/Home.aspx", target="_blank", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                                     br(),
                                     a(img(height="50%", width="50%", src="sbck.jpg"), href="https://www.sbck.org/our-work/field-work/climate-change/ocean-acidification/", target="_blank", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                                     br(),
                                     a(img(height="50%", width="50%", src="ccanlogo.jpg"), href="https://c-can.info/", target="_blank", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                                     br(),
                                     a(img(height="50%", width="50%", src="goa-onlogo.png"), href="http://www.goa-on.org/home.php", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                                     br(),
                                     a(img(height="50%", width="50%", src="OAIElogo.png"), href="https://www.oainfoexchange.org/index.html", target="_blank", style="text-align: center; display: block; margin-left: auto; margin-right: auto")))),

      # glossary tab content
      tabItem(tabName="glossary",
              h1("Glossary", align = "left"),
              h4(p(strong("Acidification"),
                   br(),
                   "A reduction in the pH of the ocean over an extended period of time, caused primarily by uptake of carbon dioxide (CO2) from the atmosphere."),
                 br(),
                 p(strong("Anthropogenic"),
                   br(),
                   "Referring to environmental change caused or influenced by people, either directly or indirectly."),
                 br(),
                 p(strong("Eutrophication"),
                   br(),
                   "A process that occurs when an aquatic environment becomes enriched with nutrients, setting off a chain reaction that increases the amount of plant and algae growth. The excess algae and plant matter eventually decompose and are digested by baceria that produce large amounts of carbon dioxide and generate low-oxygen (hypoxic) waters that can kill fish and seagrass and reduce essential fish habitats."),
                 br(),
                 p(strong("Fossil fuels"),
                  br(),
                 "High-carbon materials such as coal, crude oil, and natural gas that formed from the fossilized, buried remains of plants and animals that lived millions of years ago."),
                 br(),
                 p(strong("pH"),
                 br(),
                 "A measure of the relative amount of free hydrogen and hydroxyl ions in water. Water that has more free hydrogen ions is acidic (0-6), whereas water that has more free hydroxyl ions is basic (8-14). pH is reported in 'logarithmic units', and so each unit increase or decrease represents a 10-fold change in the acidity/basicness of the water. The average ocean pH is 8.1 (basic)."),
                 br(),
                 p(strong("Upwelling"),
                   br(),
                   "A process in which water from the ocean's depth rises toward the surface due to winds blowing across the ocean's surface. Water that rises to the surface as a result of upwelling is typically colder, more acidic, and rich in nutrients."),
                 br(),
                 p(strong("Zonation"),
                   br(),
                   "The creation of distinct vertical zones in the intertidal region due to the motion of the tides. There are four main zones: the splash zone, the high intertidal zone, and mid intertidal zone, and the low intertidal zone. Each zone has different organisms uniquely adapted to the environmental conditions of the intertidal."),
              style="text-align:left;color:black;background-color:white;padding:15px;border-radius:10px")),        
        
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
                               tags$img(src="dorisliu.jpg", style="display: block; margin-left: auto; margin-right: auto;", height=150,width=150))),
                     column(width = 3,
                            h4(p(("Amelia Ritger"),
                                 style="text-align: center"),
                               tags$img(src="amelia.jpg", style="display: block; margin-left: auto; margin-right: auto;", height=150,width=150))))),
                br(),
                h4(p(strong("With funding from UCSB Associated Students Coastal Fund"),
                     style="text-align: center"),
                   a(img(height="50%",width="50%",
                         src="coastalfund.jpeg"), href="https://coastalfund.as.ucsb.edu"), target="_blank", style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                br(),
                h4(p(strong("Special thanks to:"),
                     br(),
                     em("Robert Goettler, Logan Kozal, Melissa M. Moore, and Markie Wordley"),
                     style = "text-align: center")),
                br(),
                h4(p(strong("Do you want to look at the code for this app?")),
                   tags$a(href="https://github.com/dorisliu1104/Outreach-Fall-ShinyApp", target="_blank", "View the Github repository"),
                   style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                   br(),
                   h4(p(strong("Do you have additional questions about the app?")),
                      tags$a(href="mailto:aritger@ucsb.edu", "Email Amelia Ritger"),
                   style="text-align: center; display: block; margin-left: auto; margin-right: auto"),
                br(),
                tags$img(src = "goletapier.JPG", style="display: block; margin-left: auto; margin-right: auto;", height="75%",width="75%"),
                column(width = 12,
                       h6(p(em("Photo by Zoe Fung")),
                          style="text-align:center;color:darkgray")),
                br()
        )))))

## Create the Server
server <- function(input, output) {
  
  ## about the intertidal
  observeEvent(input$about_1, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'After watching the video...')
  })
  
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
  
  # how to lompoc plot
  output$lompoc_intro <- renderHighchart({
    y13 <- lompoc2$temp_c #set first y axis
    y14 <- lompoc2$p_h  #set second y axis
    y15 <- lompoc2$tide_height #set third y axis
    x <- lompoc2$date_time
    
    highchart() %>% 
      hc_add_series(data = y13, dashStyle="solid", name = "Temperature") %>% #plot temp
      hc_add_series(data = y14, yAxis = 1, name = "pH") %>% #plot pH
      hc_add_series(data = y15, yAxis = 2, name = "Tide") %>% #plot tide height
      hc_yAxis_multiples(
        list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temperature (C)")), #label/colorize temp y axis
        list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
        list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide (ft)"))) %>% #label/colorize tide y axis
      hc_xAxis(title = "Date", categories = lompoc2$date_highchart, tickInterval = 7 * 24 * 1 * 2) %>% #label x axis
      hc_colors(c("#D55E00", #set specific colors for points (note same color order as y axis)
                  "#009E73",
                  "#0072B2"))
  })
  
  
  dateFiltered <- reactive({
    ph_clean_final %>%
      filter(site == "Lompoc Landing") %>%
      filter(date>=input$date_range[1] & date<input$date_range[2]) %>%
      mutate(date_highchart = format(as.POSIXct(date_time), "%d %B %Y %H:%M"))

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
        list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temperature (C)")), #label/colorize temp y axis
        list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
        list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide (ft)"))) %>% #label/colorize tide y axis
      hc_xAxis(title = "Date", categories = dateFiltered()$date_highchart, tickInterval = 24) %>% #label x axis
      hc_colors(c("#D55E00", #set specific colors for points (note same color order as y axis)
                  "#009E73",
                  "#0072B2"))
  })
  # lompoc reactive
  
  lompoc_reactive <- reactive({
    if(input$week_day == "days") {
      dt <- lompoc_day
    }else if(input$week_day == "weeks") {
      dt <- lompoc2
    } else if(input$week_day == "months") {
      dt <- lompoc
    } 
    return(dt)
  })
  
  
  # data_q2_plot
  output$q2plot <- renderPlot({
    ggplot(lompoc_reactive(), aes(x=date_time, y=get(input$ph_temp)))+ #plot pH here
      geom_line(size = 0.7,color = ifelse(input$ph_temp == "temp_c", "#D55E00","#009E73" )) + #make it a line chart
      geom_smooth(method="loess", span=0.1) + #plot trend line for each site
      #scale_color_manual(values = ifelse(input$ph_temp == "temp_c", "#D55E00","#009E73" )) + #color lines by custom site color palette
      #scale_x_datetime(breaks = scales::date_breaks("1 week"), 
      #labels = date_format("%m/%d %H:%m")) + #change x axis to make it look cleaner - each tick is one week, display month/day hour/minute
      xlab("Date and time") + #change x axis label
      ylab(ifelse(input$ph_temp == "temp_c", "Temperature (C)", "pH")) + #change y axis label
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
        list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temperature (C)")), #label/colorize temp y axis
        list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
        list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide (ft)"))) %>% #label/colorize tide y axis
      hc_xAxis(title = "Date", categories = lompoc2$date_highchart, tickInterval = 7 * 24 * 1 * 2) %>% #label x axis
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
        list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temperature (C)")), #label/colorize temp y axis
        list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
        list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide (ft)"))) %>% #label/colorize tide y axis
      hc_xAxis(title = "Date", categories = lompoc3$date_highchart, tickInterval = 7 *24*4*2) %>% #label x axis
      hc_colors(c("#D55E00", #set specific colors for points (note same color order as y axis)
                  "#009E73",
                  "#0072B2"))
  })
  
  ## compare and contrast
  
  ## tab 1 plots
  output$tab1_plot <- renderPlot({
    ggplot(comdata, aes(x=date_time, y=get(input$compare_tab1), group=site)) + #plot pH here
      geom_line(aes(color=site),alpha=0.4, size=0.7, show.legend = F) + #make it a line chart
      geom_smooth(aes(color=site), method="loess", span=0.1) + #plot trend line for each site
      scale_color_manual(values = pal) + #color lines by custom site color palette
      scale_x_datetime(breaks = scales::date_breaks("2 weeks"), 
                       labels = date_format("%d %b")) + #change x axis to make it look cleaner - each tick is one week, display month/day hour/minute
      xlab("Date") + #change x axis label
      ylab(ifelse(input$ph_temp == "temp_c", "Temperature (C)", "pH")) + #change y axis label
      theme_bw() +
      theme(#legend.position = "none", #remove legend
        axis.text.x=element_text(angle=45, vjust = 1, hjust=1, size=12), #adjust x axis text format
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=12), #adjust y axis text format
        axis.title.y=element_text(size=15),
        legend.title = element_blank())
      #scale_alpha_manual(values=c(0.5,0.5,1))
  })
  
  # output$map_intro <- renderLeaflet({
  #   leaflet() %>% 
  #     addTiles() %>% 
  #     addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = 7, color = '#2e4057') %>%
  #     addLabelOnlyMarkers(
  #       lng = -125.5921856, lat = 38.31875756,
  #       label = "Bodega Bay",
  #       labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "15px",
  #                                   style = list("font-style" = "italic"))) %>%
  #     addLabelOnlyMarkers(
  #       lng = -121.3505135, lat = 34.70743071,
  #       label = "Lompoc Landing",
  #       labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "15px",
  #                                   style = list("font-style" = "italic"))) %>%
  #     addLabelOnlyMarkers(
  #       lng = -121.1519116, lat = 34.19907704,
  #       label = "Alegria",
  #       labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "15px",
  #                                   style = list("font-style" = "italic")))
  #   
  # }) 
  
  # tab 1 map output
  output$map3 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, radius = ~gps_radius, popup = ~popup_info, color = ~ifelse(input$compare_tab1 == "temp_c", "#D55E00", "#009E73")) %>%
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
  # tab 2 our research slideshows
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
    DT::datatable(data_summary_table,  options = list(dom = 't', scrollX = TRUE),
                  rownames= FALSE)
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
      dplyr::filter(site == input$compare_site) %>% 
      unite("date_time", "date", "time", sep="\ ", remove = FALSE) %>%
      mutate(date_time=ymd_hms(date_time))
  })
  
  output$tab3_plot <- renderPlot({
    p <- ggplot(siteFiltered(), aes(x=date_time, y=get(input$compare_tab3)))+ #plot pH here
      geom_line(size = 0.7,color = ifelse(input$compare_tab3 == "temp_c", "#D55E00","#009E73" )) + #make it a line chart
      geom_smooth(method="loess", span=0.1) + #plot trend line for each site
      #scale_color_manual(values = ifelse(input$ph_temp == "temp_c", "#D55E00","#009E73" )) + #color lines by custom site color palette
      #scale_x_datetime(breaks = scales::date_breaks("1 week"), 
                       #labels = date_format("%m/%d %H:%m")) + #change x axis to make it look cleaner - each tick is one week, display month/day hour/minute
      xlab("Month") + #change x axis label
      ylab(ifelse(input$compare_tab3 == "temp_c", "Temperature (C)", "pH")) + #change y axis label
      #scale_y_continuous(limits = ifelse(input$compare_tab3 == "temp_c", tab3_temp, tab3_ph)) +
      theme_bw() +
      theme(#legend.position = "none", #remove legend
        axis.text.x=element_text(angle=45, vjust = 1, hjust=1, size=12), #adjust x axis text format
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=12), #adjust y axis text format
        axis.title.y=element_text(size=15))
    
    if(input$compare_tab3 == "temp_c"){
      p + scale_y_continuous(limits=c(5,25)) 
    }
    else{
      p + scale_y_continuous(limits=c(7.2,8.5)) 
    }
    
  })
  
  
  #tab4 output
  tab4_reactive <- reactive({
    if(input$compare_tab4 == "Example 1 (Alegria)") {
      data <- alegria4
    }else if(input$compare_tab4 == "Example 2 (Lompoc Landing)") {
      data <- lompoc4
    } else if(input$compare_tab4 == "Example 3 (Bodega Bay)") {
      data <- bodega4
    } 
    return(data)
  })
  
  
  
  output$tab4_plot <- renderHighchart({ 
    y10 <- tab4_reactive()$temp_c #set first y axis
    y11 <- tab4_reactive()$p_h  #set second y axis
    y12 <- tab4_reactive()$tide_height #set third y axis
    x <- tab4_reactive()$date_time
    
    highchart() %>% 
      hc_add_series(data = y10, dashStyle="solid", name = "Temperature") %>% #plot temp
      hc_add_series(data = y11, yAxis = 1, name = "pH") %>% #plot pH
      hc_add_series(data = y12, yAxis = 2, name = "Tide") %>% #plot tide height
      hc_yAxis_multiples(
        list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temperature (C)")), #label/colorize temp y axis
        list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
        list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide (ft)"))) %>% #label/colorize tide y axis
      hc_xAxis(title = "Date", categories = tab4_reactive()$date_highchart, tickInterval = 7*24*1*1) %>% #label x axis
      hc_colors(c("#D55E00", #set specific colors for points (note same color order as y axis)
                  "#009E73",
                  "#0072B2"))
    })
  
  # our research photos
  observeEvent(input$bodega1,{
    showModal(modalDialog(
      title = "Bodega Bay",
      size = "l",
      HTML('<img src="bodega_site.jpg" width=100% />'),
      h4(p("Overview of the rocky intertidal zone at Bodega Bay.")),
      br(),
      br(),
      HTML('<img src="bodega-sun.jpg" width=100% />'),
      h4(p("Zooming in on the pH sensor deployed at Bodega Bay, at low tide."))
    ))
  })
  
  observeEvent(input$lompoc1,{
    showModal(modalDialog(
      title = "Lompoc Landing",
      size = "l",
      HTML('<img src="lol_site.jpg" width=100% />'),
      h4(p("Overview of the rocky intertidal zone at Lompoc Landing.")),
      br(),
      br(),
      HTML('<img src="lol-horizon.jpg" width=100% />'),
      h4(p("Zooming in on the pH sensor deployed at Lompoc Landing, at low tide."))
    ))
  })
  
  observeEvent(input$alegria1,{
    showModal(modalDialog(
      title = "Bodega Bay",
      size = "l",
      HTML('<img src="alegria2_site.jpg" width=100% />'),
      h4(p("Overview of the rocky intertidal zone at Alegria.")),
      br(),
      br(),
      HTML('<img src="alg-horizon.jpg" width=100% />'),
      h4(p("Zooming in on the pH sensor deployed at Alegria, at low tide."))
    ))
  })

  
}

## Combine the UI and the server
shinyApp(ui = ui, server = server)