# Code to plot up all sites together 
library(scales) #for loess plot axis edits

##set custom color for sites
pal <- c(
  "Alegria" = "#D55E00",
  "Lompoc Landing" = "#009E73", 
  "Bodega Bay" = "#0072B2"
)

##set order in which each site is plotted = the last site name is going to be the last set of points plotted, so in front of all other sites
df$site <- factor(df$site, levels=c("Lompoc Landing","Bodega Bay", "Alegria"))

##generate figure
ggplot(df, aes(x=date_time, y=p_h, group=site)) + #plot pH here
  geom_line(aes(color=site, alpha=site), size=0.7) + #make it a line chart
  #geom_smooth(aes(color=site), method="loess", span=0.1) + #plot trend line for each site
  scale_color_manual(values = pal) + #color lines by custom site color palette
  scale_x_datetime(breaks = scales::date_breaks("1 week"), 
                   labels = date_format("%m/%d %H:%m")) + #change x axis to make it look cleaner - each tick is one week, display month/day hour/minute
  xlab("Date time") + #change x axis label
  ylab("pH") + #change y axis label
  theme_bw() +
  theme(#legend.position = "none", #remove legend
    axis.text.x=element_text(angle=45, vjust = 1, hjust=1, size=12), #adjust x axis text format
    axis.title.x=element_text(size=15),
    axis.text.y=element_text(size=12), #adjust y axis text format
    axis.title.y=element_text(size=15)) +
  scale_alpha_manual(values=c(0.5,0.5,1)) #make focal site more opaque, make all other site points more transparent


# Code to plot up LOL temp and pH on the same figure - a quick Google shows that it IS possible to incorporate highchart figures into Shiny!
library(highcharter) #create highcart to interact with data

##For highchart, set variable names
y1 <- lol$temp_c #set first y axis
y2 <- lol$p_h #set second y axis
y3 <- lol$tide_height #set third y axis
x <- lol$date_time #set x axis

##Plot it up in an interactive figure, a highchart
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

