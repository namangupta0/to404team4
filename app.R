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
library(dplyr)
    
masterdata <- read.csv("new_MASTER_01_data.csv", stringsAsFactors = FALSE)
masterdata <- sample_frac(masterdata, 0.1)
masterdata$gender <- as.factor(masterdata$gender)
masterdata$gender <- as.factor(ifelse(masterdata$gender == "0", "Unknown", ifelse(masterdata$gender == "1", "Male", "Female")))
masterdata[,15:48] <- NULL
masterdata$X <- NULL
masterdata$tripduration <- NULL
masterdata$start.station.id <- NULL
masterdata$start.station.name <- NULL
masterdata$start.station.longitude <- NULL
masterdata$start.station.latitude <- NULL
masterdata$end.station.id <- NULL
masterdata$end.station.name <- NULL
masterdata$end.station.longitude <- NULL
masterdata$end.station.latitude <- NULL
masterdata$bikeid <- NULL

ui <- fluidPage(
    titlePanel("Sample Set"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("birth.year", "Birth  Year", 1886, 2003, c(1900,2000)),
            radioButtons("usertype", "User type",
                         choices = c("Customer", "Subscriber"),
                         selected = "Subscriber"),
            uiOutput("genderOutput")
        ),
        mainPanel(
            plotOutput("coolplot"),
            br(), br(),
            tableOutput("results")
        )
    )
)

server <- function(input, output) {
    output$genderOutput <- renderUI({
        selectInput("gender", "Gender", 
                    sort(unique(masterdata$gender)), 
                    selected = "Male")
    })  
    
    filtered <- reactive({
        if (is.null(input$birth.year)) {
            return(NULL)
        }    
        
        masterdata %>%
            filter(birth.year >= input$birth.year[1],
                   birth.year <= input$birth.year[2],
                   usertype == input$usertype,
                   gender == input$gender
            )
    })
    
    output$coolplot <- renderPlot({
        if (is.null(filtered())) {
            return()
        }
        ggplot(filtered(), aes(birth.year)) +
            geom_histogram()
    })
    
    output$results <- renderTable({
        filtered()
    })
}

shinyApp(ui = ui, server = server)
