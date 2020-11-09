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
    
    # Build the plot
    #attach(data)
    
    plot <- PRISMA_flowchart(previous_studies = previous_studies,
                             previous_reports = previous_reports,
                             register_results = register_results,
                             database_results = database_results,
                             website_results = website_results,
                             organisation_results = organisation_results,
                             citations_results = citations_results,
                             duplicates = duplicates,
                             excluded_automatic = excluded_automatic,
                             excluded_other = excluded_other,
                             records_screened = records_screened,
                             records_excluded = records_excluded,
                             dbr_sought_reports = dbr_sought_reports,
                             dbr_notretrieved_reports = dbr_notretrieved_reports,
                             other_sought_reports = other_sought_reports,
                             other_notretrieved_reports = other_notretrieved_reports,
                             dbr_assessed = dbr_assessed,
                             dbr_excluded = dbr_excluded,
                             other_assessed = other_assessed,
                             other_excluded = other_excluded,
                             new_studies = new_studies,
                             new_reports = new_reports,
                             total_studies = total_studies,
                             total_reports = total_reports,
                             interactive = TRUE,
                             tooltips = tooltips,
                             urls = urls)
    
    # Display the plot
    output$plot <- DiagrammeR::renderDiagrammeR(plot)

    
}

# Run the application 
shinyApp(ui = ui, server = server)
