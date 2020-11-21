library(shiny)
library(rsvg)

prisma_pdf <- function(x, filename = "prisma.pdf") {
    utils::capture.output({
        rsvg::rsvg_pdf(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                       file = filename)
    })
    invisible()
}
prisma_png <- function(x, filename = "prisma.png") {
    utils::capture.output({
        rsvg::rsvg_png(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                       file = filename)
    })
    invisible()
}

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
             DiagrammeR::grVizOutput("plot1"),
             br(),
             br(),
             fluidRow(
                 column(12, offset = 1,
                 downloadButton('PRISMAflowchartPDF', 'Download PDF'),
                 downloadButton('PRISMAflowchartPNG', 'Download PNG')
                 ))
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
    
    #reactive plot
    plot <- reactive({
        req(input$data)
        data <- read.csv(input$data$datapath)
        data <- read_PRISMAdata(data)
        plot <- PRISMA_flowchart(previous_studies = data$previous_studies,
                                 previous_reports = data$previous_reports,
                                 register_results = data$register_results,
                                 database_results = data$database_results,
                                 website_results = data$website_results,
                                 organisation_results = data$organisation_results,
                                 citations_results = data$citations_results,
                                 duplicates = data$duplicates,
                                 excluded_automatic = data$excluded_automatic,
                                 excluded_other = data$excluded_other,
                                 records_screened = data$records_screened,
                                 records_excluded = data$records_excluded,
                                 dbr_sought_reports = data$dbr_sought_reports,
                                 dbr_notretrieved_reports = data$dbr_notretrieved_reports,
                                 other_sought_reports = data$other_sought_reports,
                                 other_notretrieved_reports = data$other_notretrieved_reports,
                                 dbr_assessed = data$dbr_assessed,
                                 dbr_excluded = data$dbr_excluded,
                                 other_assessed = data$other_assessed,
                                 other_excluded = data$other_excluded,
                                 new_studies = data$new_studies,
                                 new_reports = data$new_reports,
                                 total_studies = data$total_studies,
                                 total_reports = data$total_reports,
                                 interactive = FALSE,
                                 previous = TRUE,
                                 other = TRUE)
    })
    
    
    # Display the plot
    output$plot1 <- DiagrammeR::renderDiagrammeR({
        plot <- plot()
    })

    output$PRISMAflowchartPDF <- downloadHandler(
        filename = "prisma.pdf",
        content = function(file){
            prisma_pdf(plot(), 
                       file)
            }
        )
    output$PRISMAflowchartPNG <- downloadHandler(
        filename = "prisma.png",
        content = function(file){
            prisma_png(plot(), 
                       file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
