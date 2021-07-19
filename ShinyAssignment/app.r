# Load package
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
            actionButton("lmPlot", label = "Linear Model")
            
        ),

        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           tableOutput("contents"),
           verbatimTextOutput("summary")
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

    
    # reactive attributes
    data <- reactiveValues(lines = NULL, printSummary = FALSE) 
     
    
    # plot scatter with predicting value
    output$scatterPlot <- renderPlot({
        ## plot(dataInput()$x, dataInput()$y, xlab = "x", ylab = "y")
        plot( y ~ x, data = dataInput())
        abline(data$lines, col = "red", lwd = 2)
    })
    
    
    # plot scatter with predicting value
    output$lmPlot <- renderPlot({
        plot( y ~ x, data = dataInput())
        abline(linear_regressor(), col = "red", lwd = 2)
    })
    
    
    # reactive manipulation
    observeEvent(input$lmPlot, {
        data$lines <- lm(y ~ x, data = dataInput()) 
        data$printSummary <- TRUE 
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
    
    
    # output the summarty
    output$summary <- renderPrint({
        lmPlot <- lm(y ~ x, data = dataInput())
        cor <- cor(dataInput()$x, dataInput()$y, method="pearson")
        summaryObject <- summary(lmPlot)
        
        
        if (data$printSummary == TRUE) {
            
            # attributes(summaryObject)
            # print(summaryObject)
            
            cat("Model Summary", "\n")
            cat(paste("Slope: ", round(summaryObject$coefficients[2],4)), "\n")
            cat(paste("Intercept: ", round(summaryObject$coefficients[1], 4)), "\n")
            cat(paste("Pearson correlation: ", round(cor, 4)), "\n")
            cat(paste("r squared: ", round(summaryObject$r.squared, 4)), "\n")
            
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

