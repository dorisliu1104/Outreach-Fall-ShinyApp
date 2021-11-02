library(shiny) 
library(leaflet)
library(shinydashboard)
library(readxl)
library(dygraphs)
library(tidyverse)
library(lubridate)
library(xts)
library(reshape)

## Read the Updated Data
ph_clean_updated <- read_excel("data/ph_clean_updated.xls")
View(ph_clean_updated)

## Date filtering
ph_clean_updated$date =  as.Date(ph_clean_updated$date, format = "%Y-%m-%d")

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
      menuItem("Scatterplot",tabName = "scatter", icon = icon("chart-bar", lib = "font-awesome"))
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
              h2("Lorem ipsum dolor sit amet"),
              tags$img(src = "sc1.jpeg", align = "center",height = 72, width = 72),
              h3("Lorem ipsum dolor sit amet"),
              tags$img(src = "th.jpeg", align = "center",height = 72, width = 72),
      ),
      
      # Time Series tab content 
      tabItem(tabName = "TS",
              sidebarPanel("Time Series Trend Visualization",  ##Adding a title within the sidebar
                           selectInput(inputId = "site",   
                                       label = "Visualization of sites(you can choose multiple sites)",
                                       choices = c("Alegria","Lompoc Landing", "Bodega Bay" )),
                           dateRangeInput(inputId = 'dateRange', 
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
              
              
    )
  )
))

## Create the Server
server <- function(input, output) {

  ## time series tab
  site_select <- reactive ({
    ph_clean_updated %>% 
      filter(site == input$site)
    })

  output$ph_ts_plot <- renderPlot({
  ggplot(data = site_select(), 
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
}

## Combine the UI and the server
shinyApp(ui = ui, server = server)