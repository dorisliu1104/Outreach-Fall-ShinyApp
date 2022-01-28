library(tidyverse)
library(lubridate)
library(here)

ph_clean <- read_csv(here("data", "ph_clean_1121.csv"))

ph_clean <- ph_clean %>%
  mutate(date=mdy(date))

f = "%m/%d/%Y"  
ph_clean$SetDateMonth <- format(as.POSIXct(ph_clean$date, format = f), "%m")

view(ph_clean)

#ph_clean_updated$date =  as.Date(ph_clean_updated$date, format = "%m-%d-%Y")

dateFiltered <- ph_clean %>%
  filter(site == "Lompoc Landing") %>%
  filter(date >= mdy("06/18/2021"),
         date <= mdy("07/08/2021"))

#plot
ph_ts_plot <- ggplot(dateFiltered, aes(x=date_time, y=tide)) +
  geom_line() +  
  xlab("Severity of incident") + 
  ylab("Number of incidents")

ph_ts_plot

ls <- ggplot(subset(ph_clean, site %in% "Lompoc Landing"), 
             aes(tide, p_h, color = SetDateMonth)) +
  geom_point() +
  ggtitle("Lompoc Landing")

ls

library(leaflet)
library(dplyr)
library(readxl)
site_gps <- read_excel("data/site gps.xlsx")
View(site_gps)
site_gps <- site_gps %>% 
  mutate(popup_info = paste(site, "<br/>", "Average ph", Avg_ph, "<br/>", "Average temperature", Avg_temp, "<br/>", "Average tide", Avg_tide ))
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, popup = ~popup_info)

devtools::install_github("UrbanInstitute/urbnmapr")

max(Alegria$p_h)
max(Bodega_Bay$p_h)
max(Lompoc_Landing$p_h)


tabPanel("Picture of Sites",
         h3("Pic of Alegria"),
         tags$img(src = "alegria.jpeg", align = "center",height = 300, width = 300)),
tabPanel("Questions",
         h3("Pic of Lompoc Landing"),
         tags$img(src = "lompoc.jpeg", align = "center",height = 300, width = 300)),
tabPanel("Bodega Bay",
         h3("Pic of Bodega Bay"),
         tags$img(src = "bodega.jpeg", align = "center",height = 300, width = 300))





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




# compare & contrast tab content 
tabItem(tabName = "compare_and_contrast",
        h1("Compare and Contrast (Alegria, Lompoc, and Bodega Bay Results)"),
        sidebarPanel("Time Series Trend Visualization",  ##Adding a title within the sidebar
                     selectInput(inputId = "site",   
                                 label = "Visualization of sites(you can choose multiple sites)",
                                 choices = c("Alegria","Lompoc Landing", "Bodega Bay" )),
                     dateRangeInput(inputId = "date_range", 
                                    label = 'Filter tide by date',
                                    start = as.Date('2021-06-18') , end = as.Date('2021-10-08'))
                     
        ),
        mainPanel("Some text for the main panel",
                  plotOutput(outputId="ph_ts_plot")),
        sidebarPanel("Alegria Visualization",
                     selectInput(inputId = "x3",
                                 label = "X variable",
                                 choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" )),
                     selectInput(inputId = "y3",
                                 label = "Y variable",
                                 choices = c("Temperature" = "temp_durafet_c", "Ph" = "p_h", "Tide" = "tide" ))
                     
        ),
        mainPanel("Some text for the main panel",
                  plotOutput(outputId="scatterplot_alegria")),
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
),

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





