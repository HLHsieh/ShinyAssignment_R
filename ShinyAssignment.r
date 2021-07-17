#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    
    # Application title
    titlePanel("Regression analysis"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            # Input: Select a file ----
            fileInput("file1", label = "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", label = "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", label = "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),

            # Horizontal line ----
            tags$hr(),
                        
            # Input: Select quotes ----
            radioButtons("quote", label = "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", label = "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Horizontal line ----
            tags$hr(),
            tags$hr(),
            
            # Action Button for linear model
            actionButton("lmPlot", label = "Linear Model"),
            
        ),

        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           tableOutput("contents"),
           plotOutput("lmPlot"),
           textOutput("summary")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # load data
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    
    # plot scatter
    output$scatterPlot <- renderPlot({
        plot( y ~ x, data = dataInput())
        ## plot(dataInput()$x, dataInput()$y, xlab = "x", ylab = "y")
    })
    
    
    # print data
    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
    }) 
    
    
    # reactive expression for linear model
    linear_regressor <- eventReactive(input$lmPlot, {
        ## linear_regressor <- lm(y ~ x, data = dataInput())
        lmPlot <- lm(y ~ x, data = dataInput())
    })
    
    
    # plot scatter with predicting value
    output$lmtPlot <- renderPlot({
        plot( y ~ x, data = dataInput())
        abline(linear_regressor(), col = "red", lwd = 2)
    })
    
    
    
    # output$summary <- renderPrint({
    #     lmtPlot <- lm(y ~ x, data = dataInput())
    #     attributes(summary(lmPlot))
    # })
    

    

        
}

# Run the application 
shinyApp(ui = ui, server = server)
