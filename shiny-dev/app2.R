#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpart)
library(leaflet)
library(ggplot2)
library(htmltools)
library(sf)

# Load the once per session stuff here; most efficient outside of server/ui functions
load("rules.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shopping Cart Recommendations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("model",
                        "ML Model:",
                        choices=c("Apriori", "ECLAT", "FP-Growth")),
            sliderInput("confidence",
                        "Confidence:",
                        min = 0.5,
                        max = 1,
                        value = 0.7),
            sliderInput("support",
                        "Support:",
                        min = 0.005,
                        max = 0.03,
                        value = 0.01),
            sliderInput("lift",
                        "Lift:",
                        min = 1,
                        max = 40,
                        value = 40),
            sliderInput("count",
                        "Count:",
                        min = 0,
                        max = 600,
                        value = 600),
            actionButton("recomButton",
                         "Generate Recommendations")
        ),

        # Show beautiful visuals to the right of the sidepanel!
        mainPanel(
            
            # Can use tags$xxx() to represent xxx html tags, HTML("html stuff") to interpret HTML
            tags$p("Check out our:",
                   tags$a(href = "https://github.com/patrick-osborne/CSML1000-Group_10-Assignment_2/", "Github")),
            
            # UI receive and output the leaflet
            "SHOPPING CART HERE LOL"
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Use reactive() to have immediate update from UI interaction, with caching
    crashMonth <- reactive({input$crashmonth})
    crashDay <- reactive({input$crashday})
    crashHour <- reactive({input$crashhour})
    crashDayOfWeek <- reactive({input$crashdayofweek})
    
    # Render the map for the first time
    output$mymap <- renderLeaflet({
        leaflet(precinctMap) %>%
            addTiles() %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addPolygons(color = "#777777", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        popup = ~htmlEscape(paste("Precinct #:", precinct))) %>%
            addProviderTiles("CartoDB.Positron")})
        
    # React to a the button click
    observeEvent(input$predictbutton, {
                            # Make a dataframe from the inputs. Week has no consequence to this test model I think, using 1. 
                            precinct <- as.character(precinctNum$Precinct.No)
                            month <- as.integer(crashMonth())
                            week <- as.integer(1)
                            day <- as.integer(crashDay())
                            weekday = as.integer(crashDayOfWeek())
                            hour = as.integer(crashHour())
                            predictMe <- data.frame(precinct, month, week, day, weekday, hour, stringsAsFactors = FALSE)
                            predictedVals <- predict(regTreeModel, predictMe)
                            predictedVals <- predictedVals * 100
                            
                            # The prediction logic and output to UI.
                            # "Breaks are not unique" here would mean that the probability is the same throughout; it complains.
                            binpal <- colorBin("YlOrRd", predictedVals, 3, pretty = TRUE)
                            output$mymap <- renderLeaflet({
                                leaflet(precinctMap) %>%
                                clearShapes() %>%
                                addTiles() %>%
                                setView(-74.00, 40.71, zoom = 10) %>%
                                addPolygons(color = ~binpal(predictedVals), weight = 1, smoothFactor = 0.5,
                                            opacity = 1.0, fillOpacity = 0.5,
                                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                bringToFront = TRUE),
                                            popup = ~htmlEscape(paste("Precinct #:", precinct))) %>%
                                addLegend(pal = binpal, values = ~predictedVals, 
                                          title = "Crash Probability", 
                                          labFormat = labelFormat(suffix = "%"),
                                          opacity = 1) %>%
                                addProviderTiles("CartoDB.Positron")})
                            }
    )
    # The prediction logic and output to UI.
    # THIS: leafletProxy doesn't work for some reason. Since this doesn't work, the app is less performant.
    # leafletProxy("mymap", precinctMap) %>%
    #     clearShapes() %>%
    #     addPolygons(color = "#BBBBBB", weight = 1, smoothFactor = 0.5,
    #                 opacity = 1.0, fillOpacity = 0.5,
    #                 fillColor = ~colorBin("YlOrRd",c(1,100), bins = 3, pretty = FALSE),
    #                 highlightOptions = highlightOptions(color = "white", weight = 2,
    #                                                     bringToFront = TRUE))
}

# Run the application 
shinyApp(ui = ui, server = server)
