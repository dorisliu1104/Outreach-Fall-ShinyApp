library(highcharter)
ph_clean_final<- read_csv(here("data", "ph_clean_final.csv"))
lompoc <- dplyr::filter(ph_clean_final, site=="Lompoc Landing")

y1 <- lompoc$temp_c #set first y axis
y2 <- lompoc$p_h  #set second y axis
y3 <- lompoc$tide_height #set third y axis
x <- lompoc$date_time #set x axis

highchart() %>% 
  hc_add_series(data = y1, dashStyle="solid") %>% #plot temp
  hc_add_series(data = y2, yAxis = 1) %>% #plot pH
  hc_add_series(data = y3, yAxis = 2) %>% #plot tide height
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor='#D55E00', title=list(text="Temp")), #label/colorize temp y axis
    list(lineWidth = 3, lineColor="#009E73", title=list(text="pH")), #label/colorize pH y axis
    list(lineWidth = 3, lineColor="#0072B2", title=list(text="Tide"))) %>% #label/colorize tide y axis
  hc_xAxis(title = "Date", categories = x, breaks=10) %>% #label x axis
  hc_colors(c("#D55E00", #set specific colors for points (note same color order as y axis)
              "#009E73",
              "#0072B2"))

br(),
p("Q2. What do you notice about the scale of change for both pH and temp over hours? Days? Weeks/months?"),
br(),
p("Q3. Search up the weather for August 2 and compare it to the Lompoc data. What do you think could’ve caused the spikes in the data? What are some reasons why the temperature might’ve hit an extreme that day? "),
br(),
p("Q4. What would normal data collection weather be like compared to the extremes?"))
