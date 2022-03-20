library(shiny) 
library(leaflet)
library(shinydashboard)

## Create the ui (user interface)
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "Outreach Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Map", tabName = "map", icon = icon("thumbtack")),
      menuItem("Tables", tabName = "table", icon = icon("table")),
      menuItem("Graphs", tabName = "graphs", icon = icon("stats"))
    )
  ), ## end dashboardSidebar
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              tags$video(src = "Soaring Over UC Santa Barbara.mp4", width = "500px", height = "350px",
                         type = "video/mp4", controls = "controls")
      ),

      tabItem(tabName = "description",
              h2("Lorem ipsum dolor sit amet"),
              tags$img(src = "sc1.jpeg", align = "center",height = 72, width = 72),
              h3("Lorem ipsum dolor sit amet"),
              tags$img(src = "th.jpeg", align = "center",height = 72, width = 72),
              ),
      
    )
  )
)

 
server <- function(input, output) {
  }
  
shinyApp(ui = ui, server = server)
