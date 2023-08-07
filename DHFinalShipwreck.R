# loading the required libraries
library(readxl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(dplyr)
library(viridis)
library(ggthemes)
library(htmltools)
library(plotly)
library(ggforce)
library(ggplot2)
library(jpeg)
library(grid)
library(ggimage)
library(dbplyr)
library(tidyr)
library(janitor)


# importing the data 
AWOIS_Obstructions <- read_excel("AWOIS_Obstructions.xls")
AWOIS_Wrecks <- read_excel("AWOIS_Wrecks.xls")
ENC_Wrecks <- read_excel("ENC_Wrecks.xls")


# building our dataset

all_records = AWOIS_Wrecks %>% select( c("VESSLTERMS", "FEATURE_TYPE", "LATDEC", "LONDEC", "DEPTH", "YEARSUNK", "HISTORY"))

all_records = rbind(all_records, AWOIS_Obstructions %>% select( c("VESSLTERMS", "FEATURE_TYPE", "LATDEC", "LONDEC", "DEPTH", "YEARSUNK", "HISTORY")))

ENC_Wrecks = ENC_Wrecks %>% rename(  VESSLTERMS=vesslterms, FEATURE_TYPE=feature_type, LATDEC=latdec , LONDEC=londec , DEPTH=depth, YEARSUNK=yearsunk, HISTORY=history)

all_records = rbind(all_records, ENC_Wrecks %>% select(c("VESSLTERMS", "FEATURE_TYPE", "LATDEC", "LONDEC", "DEPTH", "YEARSUNK", "HISTORY")))

# Here, we're going to clean up the descriptions. 

all_records = all_records %>% mutate(FEATURE_TYPE = ifelse(as.character(FEATURE_TYPE) == "Wrecks - Visible", "Wreck - Visible", as.character(FEATURE_TYPE)))
all_records = all_records %>% mutate(FEATURE_TYPE = ifelse(as.character(FEATURE_TYPE) == "Wrecks - Submerged, dangerous", "Wreck - Visible", as.character(FEATURE_TYPE)))
all_records = all_records %>% mutate(FEATURE_TYPE = ifelse(as.character(FEATURE_TYPE) == "Wrecks - Submerged, nondangerous", "Wreck - Submerged, nondangerous", as.character(FEATURE_TYPE)))
all_records = all_records %>% mutate(FEATURE_TYPE = ifelse(is.na(as.character(FEATURE_TYPE)) == TRUE, "NA", as.character(FEATURE_TYPE)))

# If we do a unique() on FEATURE_TYPE, we should have no duplicated categories.
unique(all_records$FEATURE_TYPE) # There are 11 unique values. 

choiceValues = c(
  "wreckSubmergedDangerous",
  "wreckSubmergedNotDangerous",
  "wreckVisibleNotDangerous",
  "rockCovered",
  "rockAwash",
  "obstructionSubmerged",
  "obstructionVisible",
  "obstructionAwash",
  "distributed",
  "notCharted",
  "NA")

uniqueFeatureTypes = unique(all_records$FEATURE_TYPE)

# Since there are 11 categories, choose 11 colors. 
palette <- colorFactor(c("darkgreen",   
                         #"darkgreen",
                         #"deepskyblue4",  
                         "deepskyblue4",
                         #"deeppink",
                         "deeppink",
                         "darkblue",
                         "coral1",
                         "chartreuse",
                         "aquamarine",
                         "darkorchid4",
                         "firebrick2",         
                         "gold1",
                         #"gray0",
                         "cornflowerblue"), 
                       domain=c(
                         "Wreck - Submerged, dangerous to surface navigation",
                         #"Wrecks - Submerged, dangerous",
                         "Wreck - Visible", 
                         #"Wrecks - Visible",
                         "Wreck - Submerged, nondangerous",
                         #"Wrecks - Submerged, nondangerous",
                         "Rock - Covered at low water",                       
                         "Rock - Awash", 
                         "Obstruction - Submerged",
                         "Obstruction - Visible at high water",              
                         "Obstruction - Covers/uncovers (awash)",             
                         "distributed remains of wreck", 
                         "Not Charted",                                     
                         "NA"
                       ))
###############################################
###############################################
#####
#####  UI
#####
###############################################
###############################################

ui <- fluidPage(
  theme=shinytheme("superhero"),
  
  tags$audio(src = "ocean_waves.wav", type = "audio/wav", autoplay = NA, controls = NA),
  
  titlePanel("Ship Wrecks and Obstructions"),
  
  # Sidebar panel for INPUTS.
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        "year",
        label="Select Year Sunk Range",
        min=min(all_records$YEARSUNK, na.rm=TRUE),
        max=max(all_records$YEARSUNK, na.rm=TRUE),
        value=c(min(all_records$YEARSUNK, na.rm=TRUE),max(all_records$YEARSUNK, na.rm=TRUE)),
        sep = ""),
      
      checkboxGroupInput(
        "type",
        label = "Select the items you're interested in",
        choiceNames = c(
          "wreck - submerged, dangerous to surface navigation",
          "wreck - submerged, not dangerous to surface navigation",
          "wreck - visible, not dangerous to surface navigation",
          "rock - covered at low water",
          "rock - awash",
          "obstruction - submerged",
          "obstruction - visible at high water",
          "obstruction - covers / uncovers, awash",
          "distributed remains of wreck",
          "not charted",
          "NA"),
        choiceValues = c(
          "wreckSubmergedDangerous",
          "wreckSubmergedNotDangerous",
          "wreckVisibleNotDangerous",
          "rockCovered",
          "rockAwash",
          "obstructionSubmerged",
          "obstructionVisible",
          "obstructionAwash",
          "distributed",
          "notCharted",
          "NA"),
        selected = c("wreckSubmergedDangerous",
                     "wreckSubmergedNotDangerous",
                     "wreckVisibleNotDangerous")
      ), # Closes out checkbox group
    ), #closes sidebarPanel
    mainPanel(
      img(src = "shipwreckbanner.jpg", height = 140, width = 1200),
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", 
                           strong("Wrecks and Obstructions Introduction"),
                           br(),
                           p("The Coast Survey's Automated Wreck and 
                             Obstruction Information System (AWOIS) database
                             was used for this project visualizations and 
                             analysis. AWOIS is a valuable tool and information 
                             source for marine archaeologists and historians, 
                             fishermen, divers, salvage operators, and others 
                             in the marine community. It contains information 
                             on over 10,000 submerged wrecks and obstructions in the coastal waters 
                             of the United States. The information includes 
                             latitude and longitude of each feature along 
                             with a brief historic description."),
                           p("My goal is to succesfully provide you with 
                           visualizations of the different shipwrecks, 
                           obstruction types and their locations in the US over 
                             the years. I hope you are able to find each tab fun 
                             and informative."),
                           p("For further information, visit the ",
                             a("NOAA Homepage", 
                               href = "https://nauticalcharts.noaa.gov/data/wrecks-and-obstructions.html"))),
                  tabPanel("Map", p("This interactive map contains the latitude 
                                    and longitude of each wreck and obstruction 
                                    given by the database. You can filter the 
                                    type of wreck or obstruction by using the 
                                    checkboxes on the side panel and also zoom 
                                    in and out to look at different locations 
                                    within the US. Furthermore, once you hover 
                                    over each point you can see the name of the 
                                    vessel (If any) and year sunk (if any). 
                                    Once you click on a point, you are able to 
                                    read a brief historic description (if any) 
                                    about that particular event. If you would 
                                    like to change the map style, on the top 
                                    right corner you can hover over the stacked 
                                    tiles and click on “Open street map”."), 
                           leafletOutput("shipwreckMap")),
                  tabPanel("Plot 1", p("This visualization uses time as a 
                                       continuous scale within a Cartesian 
                                       coordinate system and divides it into 
                                       yearly buckets. You can filter the data 
                                       by using the year slider and focus on a 
                                       specific time period. For each year range, 
                                       you can see how many events per type of event happened. 
                                       Furthermore, this plot 
                                       contains different color hues to 
                                       represent the type of events and also 
                                       uses positon as a visual cue. Both color 
                                       and position make it easier to spot 
                                       trends/clusters and even outliers of the 
                                       events per year. (Please note that some 
                                       event types do not contain information 
                                       about the type of event or/either the 
                                       year sunk)."), plotlyOutput("plot1")),
                  tabPanel("Plot 2", p("This visualization provides an easier 
                                       way to look at data over time. It contains 
                                       the count of events and uses time as a 
                                       continuous scale within a Cartesian 
                                       coordinate system. You can filter the 
                                       data by using the checkboxes in the side 
                                       panel to focus on specific event(s) and
                                       also use the year slider to focus on a 
                                       timeframe. Furthermore, this plot contains 
                                       different color hues to represent the 
                                       type of events and also uses length, 
                                       which provides you with context of the 
                                       data. (Please note that some 
                                       event types do not contain information 
                                       about the type of event or/either the 
                                       year sunk)."),
                           plotlyOutput("plot2")),
                  tabPanel("HeatMap", p("This heatmap visualization provides 
                                        a great way to visualize the data, 
                                        containing the count of events, as a 
                                        “whole”. This visualizations uses a 
                                        Cartesian coordinate system and uses 
                                        both wide color hues and saturation by 
                                        the number of occurrences per event 
                                        within the whole database.  Heatmaps 
                                        are a lot more visual than standard 
                                        graphs, which can make them easier to 
                                        analyze and visualize at a glance. 
                                        This makes them more accessible, 
                                        particularly for people who are not 
                                        accustomed to analyzing large amounts 
                                        of data. (Please note that some 
                                       event types do not contain information 
                                       about the type of event)."), 
                           plotOutput("heatmap1"))
    )
    
  ), 
 ),#closes sidebarLayout
 )#closes fluid page

###############################################
###############################################
#####
#####  SERVER
#####
###############################################
###############################################

server=function(input, output) {
  
  data = reactive({
    
    # 
    itemsOfInterest = c(
      # case_when(input$type == "wreckSubmergedDangerous" ~ 
      #             c("Wrecks - Submerged, dangerous")),
      
      case_when(input$type == "wreckSubmergedDangerous" ~          
                  c("Wreck - Submerged, dangerous to surface navigation")),
      
      case_when(input$type == "wreckSubmergedNotDangerous" ~
                  c("Wreck - Submerged, nondangerous")),
      
      # case_when(input$type == "wreckSubmergedNotDangerous" ~
      #             c("Wrecks - Submerged, nondangerous")),
      
      case_when(input$type == "wreckVisibleNotDangerous" ~
                  c("Wreck - Visible")),
      
      # case_when(input$type == "wreckVisibleNotDangerous" ~
      #             c("Wrecks - Visible")),
      # 
      case_when(input$type == "rockCovered" ~
                  c("Rock - Covered at low water")),
      
      case_when(input$type == "rockAwash" ~
                  c("Rock - Awash")),
      
      case_when(input$type == "obstructionSubmerged" ~
                  c("Obstruction - Submerged")),
      
      case_when(input$type == "obstructionVisible" ~
                  c("Obstruction - Visible at high water")),
      
      case_when(input$type == "obstructionAwash" ~
                  c("Obstruction - Covers/uncovers (awash)")),
      
      case_when(input$type == "distributed" ~
                  c("distributed remains of wreck")),
      
      case_when(input$type == "notCharted" ~
                  c("Not Charted")),
      
      case_when(input$type == "NA" ~
                  c("NA"))
      
    )
    all_records %>% filter(FEATURE_TYPE %in% itemsOfInterest) 
  })
  
  output$userSelections = reactive({
    paste("The user selected the following options:", input$type)
  })
  
  output$items = reactive({
    paste("the unique items in the output dataset are",
          unique(data()$FEATURE_TYPE ))
  })
  
  output$nrows = reactive ({
    paste("the data frame has ", nrow(data()), " rows.")
  })
  #DATA FOR BARPLOT
  dataforbarplot = reactive({
    data() %>% filter(
      (YEARSUNK >= input$year[1]) &
        (YEARSUNK <= input$year[2])
    ) %>% group_by(YEARSUNK, FEATURE_TYPE) %>% tally()
    #tabyl(YEARSUNK,FEATURE_TYPE)
  })
  #BARPLOT GRAPH
  ex.plot2 = reactive({
    ggplotly(
      ggplot(data=dataforbarplot(), aes(x=FEATURE_TYPE, y=n, group=YEARSUNK,fill=FEATURE_TYPE,  text=YEARSUNK))+
        geom_col(position="dodge", stat="identity",  color="black")+
        #fill="pink",
        ggtitle("Number of Wrecks and Obstructions over Time")+
        theme_economist()+
        theme(axis.text.x=element_blank(),axis.ticks.x = element_blank(),
              plot.title = element_text(size=20, face="bold")),
      
      #theme(axis.text.x=element_text(angle=90)),
      #geom_text(aes(label=YEARSUNK))+
      #coord_flip(),
      tooltip = "text"
    ) %>% layout(legend = list( x= 0.10, y=-0.8),
                 annotations = 
                   list(x = 1, y = -0.8, text = "Data Source: NOOAA Website", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=15, color="red")),
                 autosize = F, width = 1200, height = 700)
  })
  #DATA FOR SCATTERPLOT
  dataForScatterplot = reactive({
    data() %>% filter(
      (YEARSUNK >= input$year[1]) &
        (YEARSUNK <= input$year[2]) 
    )
  })#SCATTERPLOT
  ex.plot = reactive({
    ggplotly(
      ggplot(data=dataForScatterplot(),aes( x = YEARSUNK, y = FEATURE_TYPE, text = paste(YEARSUNK,FEATURE_TYPE), fill=FEATURE_TYPE))+
        geom_point(position = "jitter")+
        ggtitle("Type of Wrecks and Obstructions per YearSunk")+
        theme(axis.ticks.y=element_blank(),
              axis.text.y = element_blank(),
              plot.title = element_text(size=20, face="bold"),
              legend.position = c(0,1),
              panel.background = element_rect(fill = "#BFD5E3", 
                                              color = "6D9EC1", 
                                              size = 2, linetype = "solid")),
      tooltip = "text"
    ) %>% layout(legend=list(orientation = "h", x=0.10,y=-0.4),
                 annotations = list(x = 1, y = -0.4, text = 
                                      "Data Source: NOAA Website",
                                    showarrow = F, xref='paper', 
                                    yref='paper', xanchor='right', 
                                    yanchor='auto', xshift=0, 
                                    yshift=0, font=list(size=15, color = "red")),
                 autosize = F, width = 1200, height = 800)
     
    
  })
  dataForHeatmap = all_records %>% group_by(FEATURE_TYPE) %>% tally()
  nrows = 10
  var = all_records$FEATURE_TYPE
  df = expand.grid(y = 1:nrows, x = 1:nrows)
  categ_table = round(table(var)* ((nrows*nrows)/(length(var))))
  categ_table[1] =1
  df$category = factor(rep(names(categ_table), categ_table))
  
  output$plot1=renderPlotly(ex.plot())
  output$plot2=renderPlotly(ex.plot2())
  output$heatmap1=renderPlot({
    ggplot(df, aes(x=x, y = y, fill=category))+
      geom_tile( size = 0.5)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand= c(0,0), trans='reverse')+
      ggtitle("Number of Wrecks and Obstructions within the database")+
      theme(axis.text.x=element_blank(),axis.ticks.x = element_blank(),
            axis.title.x=element_blank(),axis.title.y = element_blank(),
            plot.title = element_text(size=30, face="bold"),
            axis.text.y=element_blank(),axis.ticks.y = element_blank())
  })
  output$datatable1 <- renderTable(dataForTable())
  
  output$shipwreckMap = renderLeaflet({
    leaflet(options=leafletOptions(preferCanvas = TRUE)) %>%
      
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI world imagery", 
                       options = providerTileOptions(updateWhenZooming = FALSE,
                                                     updateWhenIdle = TRUE)) %>%
      
      addProviderTiles(providers$OpenStreetMap, group = "Open Street Map",
                       options = providerTileOptions(updateWhenZooming = FALSE,
                                                     updateWhenIdle = TRUE)) %>%
      
      addLayersControl(baseGroups = c("ESRI world imagery", "Open Street Map")) %>%
      setView(lat = 27.5, lng = -92.5, zoom = 6) %>% 
      addCircleMarkers(data=data(), 
                       lat = ~LATDEC, 
                       lng = ~LONDEC,
                       color = ~palette(FEATURE_TYPE),
                       radius = 3,
                       stroke = FALSE,
                       fillOpacity = 1,
                       labelOptions = labelOptions(noHide = FALSE),
                       label = ~htmlEscape(paste("name:", VESSLTERMS, "year sunk:", YEARSUNK)),
                       
                       # (if (is.na(~VESSLTERMS)==FALSE) {
                       # get(VESSLTERMS)
                       #   } else {
                       # "UNKNOWN"}),
                       # 
                       # "<br>",
                       # 
                       # (if (is.na(~YEARSUNK)==FALSE) {
                       #   ~YEARSUNK
                       # } else {
                       #   "UNKNOWN"})
                       # ),
                       
                       popup = ~htmlEscape(paste(HISTORY)))
    
    # ~as.character(
    # if (is.na(VESSLTERMS)==FALSE) {
    # VESSLTERMS
    #  } else {
    #  paste("UNKNOWN")
  })
  #     )
  # })
  
}

shinyApp(ui, server)
