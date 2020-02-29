#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(htmltools)
library(DT)
library(arules)

# Load the once per session stuff here; most efficient outside of server/ui functions
load("rules.RData")
load("inventory.RData")
cartitems <- inventory
emptyCartDataframe <- inventory[0,]
assoc.rules.DF <- DATAFRAME(association.rules, separate=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        
        titlePanel("Shopping Cart Recommendations"),
        tags$p("Check out our:",
               tags$a(href = "https://github.com/patrick-osborne/CSML1000-Group_10-Assignment_2/", "Github")),
        tabsetPanel(type = "tabs",
                    #Manager's Console Code Block ----
                    tabPanel("Data Analyst's Console", 
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput("model",
                                                 "Association Rule Mining Model:",
                                                 choices=c("Apriori", "ECLAT", "FP-Growth")),
                                     sliderInput("support",
                                                 "Support:",
                                                 min = 0.005,
                                                 max = 0.0401,
                                                 value = 0.01),
                                     sliderInput("confidence",
                                                 "Confidence:",
                                                 min = 0.5,
                                                 max = 1,
                                                 value = 0.7),
                                     sliderInput("lift",
                                                 "Lift:",
                                                 min = 4.5,
                                                 max = 104,
                                                 value = 40),
                                     sliderInput("count",
                                                 "Count:",
                                                 min = 104,
                                                 max = 825,
                                                 value = 600),

                                 ),
                                 
                                 # Show beautiful visuals to the right of the sidepanel!
                                 mainPanel(
                                     h3("Identified Rules"),
                                     DTOutput ("console")
                                 )
                             )
                    ),
                    
                    #User's Shopping Cart Code Block ----
                    tabPanel("Shopping Cart",
                             
                             sidebarLayout(
                                 sidebarPanel(
                                     pickerInput("cartSelect",
                                                 label="Select Items:",
                                                 choices=inventory$Description,
                                                 multiple=TRUE,
                                                 options=list('live-search' = TRUE
                                                 )
                                     ),
                                     actionButton("addCart",
                                                  "Add Items to Cart")
                                 ),
                                 
                                 # Show beautiful visuals to the right of the sidepanel!
                                 mainPanel(
                                     h3("Shopping Cart"),
                                     DTOutput ("cart"),
                                     h3("Usually Bought With:"),
                                     DTOutput ("recom")
                                 )
                             )
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$console <- renderDataTable(assoc.rules.DF, rownames=FALSE)
    output$cart <- renderDataTable(emptyCartDataframe, 
                                   rownames=FALSE, 
                                   options=list(pageLength=9))
    output$recom <- renderDataTable(emptyCartDataframe, 
                                    rownames=FALSE, 
                                    options=list(pageLength=9))
    
    observeEvent(input$support, {
        output$console <- renderDataTable(assoc.rules.DF[assoc.rules.DF$support>input$support, ],
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
    })
    
    observeEvent(input$confidence, {
        output$console <- renderDataTable(assoc.rules.DF[assoc.rules.DF$confidence>input$confidence, ], 
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
    })
    
    observeEvent(input$lift, {
        output$console <- renderDataTable(assoc.rules.DF[assoc.rules.DF$lift>input$lift, ], 
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
    })
    
    observeEvent(input$count, {
        output$console <- renderDataTable(assoc.rules.DF[assoc.rules.DF$count>input$count, ], 
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
    })
    
    observeEvent(input$addCart, {
        #subsets the full inventory to what is selected for the cart. this dataframe is used to display the user cart.
        (cartitems <- subset(cartitems, cartitems$Description %in% input$cartSelect))
        
        #passes the user cart to "cart"
        output$cart <- renderDataTable(cartitems, rownames=FALSE)
        
        #only runs if the user has added something to the cart. Otherwise it blows up.
        if(is.null(input$cartSelect)){
        }else{
            #generate a subset of the full list of rules, where the lhs matches an exact subset of the cart and the rhs
            #does not match an exact subset of the cart (rhs is not already in cart)
            rules.sub <- subset(association.rules, subset = lhs %oin% input$cartSelect & !(rhs %oin% input$cartSelect))
            #checks if there are any rules genereated. it BLOWS UP if there aren't. Don't ask how long that took to figure out.
            if(length(rules.sub) > 0){
                #convert that rules subset into a dataframe (DATAFRAME() is from arules)
                rules.subDF <- DATAFRAME(rules.sub, separate=TRUE)
                #output the data frame of recommendations to recom
                output$recom <- renderDataTable(rules.subDF, rownames=FALSE)
            }else{
                output$recom <- renderDataTable(NULL, rownames=FALSE)
                #PUT IN NO OTHER ITEMS THING
            }
        }
        
        # save(rules.subDF, file="testRECOM.RData")
    })
    
    # List of the items to be added to cart
    observeEvent(input$addToCartButton, {
        itemsToCart <- input$cart_cell_clicked
        output$msg <- renderText({paste("You have selected", itemsToCart$value)})
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
