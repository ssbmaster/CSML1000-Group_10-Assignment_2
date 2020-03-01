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
library(htmltools)
library(DT)
library(arules)
library(stringr)

# Load the once per session stuff here; most efficient outside of server/ui functions
load("rules.RData")
load("ECLATrules.RData")
load("inventory.RData")
cartitems <- inventory
emptyCartDataframe <- inventory[0,]
recItemsDF <- inventory[0,]
assoc.rules.DF <- DATAFRAME(association.rules, separate=TRUE)
aprioriVar <- TRUE

# Define UI for application
ui <- fluidPage(
    fluidRow(
        titlePanel("Shopping Cart Recommendations"),
        tags$p("Check out our:",
               tags$a(href = "https://github.com/patrick-osborne/CSML1000-Group_10-Assignment_2/", "Github")),
        tabsetPanel(type = "tabs",
                    # Data Analysts's Console Code Block ----
                    tabPanel("Data Analyst's Console", 
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput("model",
                                                 "Association Rule Mining Model:",
                                                 choices=c("Apriori", "ECLAT", "FP-Growth (Not Available)")),
                                     sliderInput("support",
                                                 "Support:",
                                                 min = 0.005,
                                                 max = 0.0401,
                                                 value = 0),
                                     sliderInput("confidence",
                                                 "Confidence:",
                                                 min = 0.5,
                                                 max = 1,
                                                 value = 0),
                                     sliderInput("lift",
                                                 "Lift:",
                                                 min = 4.5,
                                                 max = 104,
                                                 value = 0),
                                     sliderInput("count",
                                                 "Count:",
                                                 min = 0,
                                                 max = 825,
                                                 value = 0)
                                 ),
                                 
                                 # Show beautiful visuals to the right of the sidepanel!
                                 mainPanel(
                                     h3("Identified Rules"),
                                     DTOutput ("console")
                                 )
                             )
                    ),
                    
                    # User's Shopping Cart Code Block ----
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
                                                  "Add Items to Cart"),
                                     actionButton("clearCart",
                                                  "Clear Cart")
                                 ),
                                 
                                 # Show beautiful visuals to the right of the sidepanel!
                                 mainPanel(
                                     h3("Shopping Cart"),
                                     DTOutput ("cart"),
                                     h3("Usually Bought With:"),
                                     DTOutput ("recom"),
                                     textOutput("emptyMessage")
                                 )
                             )
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    output$emptyMessage <- renderText({ "Add items to your cart to get some recommendations." })
    output$console <- renderDataTable(assoc.rules.DF, rownames=FALSE)
    #shows an empty shopping cart (table)
    output$cart <- renderDataTable(emptyCartDataframe, 
                                   rownames=FALSE, 
                                   options=list(pageLength=9))
    #below is commented out because there is no value in showing an empty recommendations table.
    # output$recom <- renderDataTable(emptyCartDataframe, 
    #                                rownames=FALSE, 
    #                                options=list(pageLength=9))
    
    observeEvent(input$model, {
        if(input$model == "ECLAT"){
            assoc.rules.DF <- DATAFRAME(eclat.rules, separate=TRUE)
            output$console <- renderDataTable(assoc.rules.DF,
                                              rownames=FALSE, 
                                              options=list(pageLength=9))
            aprioroVAR = FALSE
        }else{
            assoc.rules.DF <- DATAFRAME(association.rules, separate=TRUE)
            output$console <- renderDataTable(assoc.rules.DF[assoc.rules.DF$support>input$support, ],
                                              rownames=FALSE, 
                                              options=list(pageLength=9))
        }
        
    })
    
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
        # Subsets the full inventory to what is selected for the cart. this dataframe is used to display the user cart.
        (cartitems <- subset(cartitems, cartitems$Description %in% input$cartSelect))
        
        # Passes the user cart to "cart"
        output$cart <- renderDataTable(cartitems, rownames=FALSE)
        
        # Only runs if the user has added something to the cart. Otherwise it blows up.
        if(!is.null(input$cartSelect)){
            # Generate a subset of the full list of rules, where the lhs matches an exact subset of the cart and the rhs
            # Does not match an exact subset of the cart (rhs is not already in cart)
            if(aprioriVar == TRUE){
                rules.sub <- subset(eclat.rules, subset = lhs %oin% input$cartSelect & !(rhs %oin% input$cartSelect))
            }else{
                rules.sub <- subset(association.rules, subset = lhs %oin% input$cartSelect & !(rhs %oin% input$cartSelect))
            }
            
            # Checks if there are any rules genereated. it BLOWS UP if there aren't. Don't ask how long that took to figure out.
            if(length(rules.sub) > 0){
                # Convert that rules subset into a dataframe (DATAFRAME() is from arules)
                rules.subDF <- DATAFRAME(rules.sub, separate=TRUE)
                trim(rules.subDF$RHS)
                # Match the RHS up with inventory to get the StockCode and UnitPrice
                # Line immediately below not working properly...so commented out
                # recItemsDF <- inventory[trim(gsub("[{]|[}]", "", rules.subDF$RHS)) == trim(inventory$Description), ]
                
                # But this way works...hurray for brute force programming skillz...but this is quite a bit slower
                for (j in 1:length(rules.subDF$RHS)) {
                    for (i in 1:length(inventory$Description)) {
                        if (gsub("[{]|[}]", "", rules.subDF$RHS[j]) == inventory$Description[i]) {
                            recItemsDF <- rbind(recItemsDF, inventory[i,])
                        }
                    }
                }

                # Output the data frame of recommendations to recom
                output$recom <- renderDataTable(recItemsDF, rownames=FALSE)
                output$emptyMessage <- renderText({ "" })
            }else{
                output$recom <- renderDataTable(NULL, rownames=FALSE)
                output$emptyMessage <- renderText({ "Your cart looks great! We don't have any recommendations." })
            }
        }
        
        # save(rules.subDF, file="testRECOM.RData")
    })
    
    observeEvent(input$clearCart, {
        output$cart <- renderDataTable(emptyCartDataframe, 
                                       rownames=FALSE, 
                                       options=list(pageLength=9))
        output$recom <- renderDataTable(NULL, rownames=FALSE)
        output$emptyMessage <- renderText({ "Add items to your cart to get some recommendations." })
        updatePickerInput(
            session, 
            "cartSelect", 
            selected = "")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
