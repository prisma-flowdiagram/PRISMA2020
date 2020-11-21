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
ui <- shinyUI(navbarPage("PRISMA Flow Chart",

    # Tab 1
    tabPanel("Data upload",
        fluidRow(
            column(3, offset = 1,
            fileInput("data", "Select PRISMA.csv file",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                      ),
            ),
        fluidRow(
            column(3, offset =1,
            checkboxInput("interactive", "Make plot interactive", TRUE),
            verbatimTextOutput("value")
            )
        )
    ),

        # Show a plot of the generated distribution
        fluidRow(
            column(12, 
            tableOutput("contents")
            )
        )
    ),
    
    tabPanel("Flow chart",
             shinyjs::useShinyjs(),
             DiagrammeR::grVizOutput("plot")
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Preview uploaded data
    output$contents <- renderTable({
        
        req(input$data)
        
        data <- read.csv(input$data$datapath)
        df <- data[,3:7]
        
        return(df)
        
    })
    
    # Settings (to be expanded)
    interactive <- renderText({ input$interactive })
    
    # Display the plot
    output$plot <- DiagrammeR::renderDiagrammeR({
        inFile <- input$data
        if (is.null(inFile)) {
            return(NULL)
        }
        plot <- PRISMA_flowchart(previous_studies = inFile$previous_studies,
                                 previous_reports = inFile$previous_reports,
                                 register_results = inFile$register_results,
                                 database_results = inFile$database_results,
                                 website_results = inFile$website_results,
                                 organisation_results = inFile$organisation_results,
                                 citations_results = inFile$citations_results,
                                 duplicates = inFile$duplicates,
                                 excluded_automatic = inFile$excluded_automatic,
                                 excluded_other = inFile$excluded_other,
                                 records_screened = inFile$records_screened,
                                 records_excluded = inFile$records_excluded,
                                 dbr_sought_reports = inFile$dbr_sought_reports,
                                 dbr_notretrieved_reports = inFile$dbr_notretrieved_reports,
                                 other_sought_reports = inFile$other_sought_reports,
                                 other_notretrieved_reports = inFile$other_notretrieved_reports,
                                 dbr_assessed = inFile$dbr_assessed,
                                 dbr_excluded = inFile$dbr_excluded,
                                 other_assessed = inFile$other_assessed,
                                 other_excluded = inFile$other_excluded,
                                 new_studies = inFile$new_studies,
                                 new_reports = inFile$new_reports,
                                 total_studies = inFile$total_studies,
                                 total_reports = inFile$total_reports,
                                 interactive = TRUE,
                                 tooltips = inFile$tooltips,
                                 urls = inFile$urls,
                                 previous = TRUE)
        return(plot)
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
