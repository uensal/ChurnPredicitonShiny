#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(data.table)

# load data
customer <- fread("data/data_customer.csv")
personal <- fread("data/data_personal.csv")

customer_personal <- merge(customer, personal, by = "CustomerId", all=FALSE)

customer_personal[,Exited := as.factor(Exited)]
customer_personal[,Gender := as.factor(Gender)]

formula <- Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard+ IsActiveMember + EstimatedSalary
model <- glm(formula = formula,
             family="binomial", data=customer_personal)

customer_personal[, ChurnProb := predict(model, customer_personal, type="response")]

d <- customer_personal[order(-ChurnProb),] 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ChurnProb"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        numericInput("customerId", label = h3("Get churn probability of customer"), value=d[1,CustomerId]),
        hr(),
        fluidRow(column(3, verbatimTextOutput("value_churn_prob_for_customer"))),
        
        selectInput("customerIdSelect", label = h3("Select customer id with dropdown"), 
                    choices = d[,CustomerId], 
                    selected = 1),
        hr(),
        fluidRow(column(3, verbatimTextOutput("value_churn_prob_for_customer_selected"))),
        
        ),
        
        mainPanel(
            leafletOutput("mymap", height = "800px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        zips <- as.matrix(d[1:100, list(zip_longitude, zip_latitude)])
        map <- leaflet()
        map <- addTiles(map) 
        map <- addMarkers(map, 
                          data = zips, 
                          clusterOptions = markerClusterOptions())
        map <- setView(map, lat= 43, lng= -79, zoom = 3) # North America
        # See: https://rstudio.github.io/leaflet/markers.html
    })
    
    output$value_churn_prob_for_customer <- renderPrint({
        d[CustomerId==input$customerId,ChurnProb]
    })
    
    output$value_churn_prob_for_customer_selected <- renderPrint({
        d[CustomerId==input$customerIdSelect,ChurnProb]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
