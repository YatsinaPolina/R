#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
tit <- read.csv("Titanic.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Titanic Passengers"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("bar",
                           "Barplot:",
                           names(tit[c(3,4,6)]) )
        ),
        column(4,
               selectInput("hist",
                           "Histogram:",
                           names(tit[c(7, 11)]))
        ),
        
    ),
    fluidRow(
        column(4,
               selectInput("col1",
                           "Color 1:",
                           choices = c(`Plum`= "plum",
                                       `Light Blue`= "lightblue",
                                       `Light Green`= "lightgreen"
                           ))
        ),
        column(4,
               selectInput("col2",
                           "Color 2:",
                           choices = c(`Light Pink`= "lightpink",
                                       `Salmon`= "salmon",
                                       `Steel Blue`= "steelblue"))
        ),
        
    ),
    # Create a new row for the plot.
    mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                   plotOutput("distPlot1"), 
                                   plotOutput("distPlot2"))))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Filter data based on selections
    output$distPlot1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x   <- tit
        if(input$bar == "Survived"){
            x <- tit$Survived
        }
        if(input$bar == "Pclass"){
            x <- tit$Pclass
        }
        if(input$bar == "Sex"){
            x <- tit$Sex
        }
        x
        
        barplot(table(x), col = input$col1, main = c("Amount of ",input$bar),
                ylab = "Count", xlab = input$bar
        )
    })
    
    output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- tit
        if(input$hist == "Age"){
            x <- tit$Age
        }
        if(input$hist == "Fare"){
            x <- tit$Fare
        }
        x
        
        hist(x, col = input$col2, border = 'navy', main = c("Variety of", input$hist),
             ylab = "Count", xlab = input$hist  )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

