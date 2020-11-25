library(shiny)
library(rsvg)
library(DT)
library(rio)

source("functions.R")

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("PRISMA Flow Chart",

    # Tab 1 ----
    tabPanel("Data upload",
         fluidRow(
            column(10, offset = 1,
                   'Systematic reviews should be described in a high degree of methodological detail. ', tags$a(href="http://prisma-statement.org/", "The PRISMA Statement"), 
                   'calls for a high level of reporting detail in systematic reviews and meta-analyses. An integral part of the methodological description of a review 
                   is a flow diagram/chart.',
                   br(),
                   br(),
                   'This tool allows you to produce a flow chart for your own review that conforms to ', tags$a(href="https://osf.io/preprints/metaarxiv/v7gm2/", "the PRISMA2020 Statement."), 
                   'You can provide the numbers and texts for the boxes in the CSV template below. Upload your own version and select whether to include the "previous" and 
                   "other" studies arms, then proceed to the "Flow chart" tab to see and download your figure.',
                   br(),
                   br(),
                   "At present, this version of the tool doesn't support embedding tooltips and hyperlinks in the plot. For this functionality, please use the", 
                   tags$a(href="https://github.com/nealhaddaway/PRISMA2020", "PRISMA2020 flow chart R package on Github."),
                   br(),
                   br(),
                   'Please let us know if you have any feedback or if you encounter an error by sending an email to ', tags$a(href="mailto:neal.haddaway@sei.org", "neal.haddaway@sei.org"),
                   br(),
                   br(),
                   hr()),
               
            column(12, offset = 1,
                   tags$a(href="https://ndownloader.figshare.com/files/25593458", "Download template CSV file here"),
                   br(),
                   br()),
            
            column(3, offset = 1,
            fileInput("data", "Upload your PRISMA.csv file",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                      ),
            ),
        fluidRow(
            column(6, offset =1,
            checkboxInput("previous", "Include 'previous' studies", TRUE),
            checkboxInput("other", "Include 'other' searches for studies", TRUE),
            verbatimTextOutput("value")
            )
        ),
        fluidRow(
                 DT::dataTableOutput("mytable"))
    ),

        # Show a plot of the generated distribution
        fluidRow(
            column(10, offset = 1,
            br(),
            hr(),
            'Credits:',
            br(),
            'Neal R Haddaway (creator)', br(),
            'Matthew J Page (advisor)', br(),
            'Luke McGuinness (advisor)', br(),
            'Jack Wasey (advisor)', br(),
            br(),
            tags$a(href="https://github.com/nealhaddaway/PRISMA2020", tags$img(height = 40, width = 40, src = "https://pngimg.com/uploads/github/github_PNG40.png")), 
            'Created November 2020'
            )
        )
    ),
    
    # Tab 2 ----
    tabPanel("Flow chart",
             shinyjs::useShinyjs(),
             DiagrammeR::grVizOutput(outputId = "plot1", width = "100%", height = "700px"),
             br(),
             br(),
             fluidRow(
                 column(12, offset = 5,
                 downloadButton('PRISMAflowchartPDF', 'Download PDF'),
                 downloadButton('PRISMAflowchartPNG', 'Download PNG')
                 ))
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Define reactive values
    rv <- reactiveValues()
    
# Data Handling ----
    
    # Use template data to populate editable table
    observe({
    rv$data <- read.csv("www/PRISMA.csv", stringsAsFactors = FALSE)
    })
    
    # Import data from fileInput(inputID = "data") once data is uploaded
    # This will replace the template data 
    observeEvent(input$data, {
    rv$data <- read.csv(input$data$datapath, stringsAsFactors = FALSE)
    })
    
# Editable table ----    
    
    # Define basic table
    output$mytable <- DT::renderDataTable(
      # Show only cells that user should be editing. The number of columns show affects the column offset later.
      rv$data[,4:8], 
      # Only allow users to edit cells, not column headings
      editable = list(target = 'cell'),
      server = TRUE,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        dom = 't',
        ordering = F,
        columnDefs = list(list(
          className = 'dt-center', targets = "_all"
        )),
        paging = FALSE
      )
    )
    
    # Define table proxy
    proxy = dataTableProxy('mytable')
    
    # Update reactive dataset on cell edit
    observeEvent(input$mytable_cell_edit, {
      info <- input$mytable_cell_edit
      # Define edited row
      i <- info$row 
      # Define edited column (column index offset by 4, because you are hiding
      # the rownames column and the first 3 columns of the data)
      j <- info$col + 4L
      # Define value of edit
      v <- info$value
      
      # Pass edited value to appropriate cell of data stored in rv$data
      rv$data[i, j] <- coerceValue(v, rv$data[i, j])
      
      # Replace data in table with updated data stored in rv$data
      replaceData(proxy,
                  rv$data,
                  resetPaging = FALSE,
                  rownames = FALSE)  # important
    })
    
    
# Reactive plot ----
    
    # Create plot
    plot <- reactive({
        data <- read_PRISMAdata(rv$data)
        attach(data)
        plot <- PRISMA_flowchart(data,
                                 interactive = FALSE,
                                 previous = input$previous,
                                 other = input$other)
    })
    
    
    # Display plot
    output$plot1 <- DiagrammeR::renderDiagrammeR({
        plot <- plot()
    })
    
    
# Handle downloads ----
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


