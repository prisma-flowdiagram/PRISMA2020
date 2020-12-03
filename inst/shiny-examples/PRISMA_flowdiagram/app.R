library(shiny)
library(rsvg)
library(DT)
library(rio)

source("functions.R")

template <- read.csv("www/PRISMA.csv",stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("PRISMA Flow Diagram",
                         
                         # Tab 1 ----
                         tabPanel("Home",
                                  fluidRow(
                                    column(10, offset = 1,
                                           'Systematic reviews should be described in a high degree of methodological detail. ', tags$a(href="http://prisma-statement.org/", "The PRISMA Statement"), 
                                           'calls for a high level of reporting detail in systematic reviews and meta-analyses. An integral part of the methodological description of a review 
                   is a flow diagram.',
                                           br(),
                                           br(),
                                           'This tool allows you to produce a flow diagram for your own review that conforms to ', tags$a(href="https://osf.io/preprints/metaarxiv/v7gm2/", "the PRISMA2020 Statement."), 
                                           'You can provide the numbers and texts for the boxes in the CSV template below. Upload your own version and select whether to include the "previous" and 
                   "other" studies arms, then proceed to the "Flow diagram" tab to see and download your figure.',
                                           br(),
                                           br(),
                                           "At present, this version of the tool doesn't support embedding tooltips and hyperlinks in the plot. For this functionality, please use the", 
                                           tags$a(href="https://github.com/nealhaddaway/PRISMA2020", "PRISMA2020 flow diagram R package on Github."),
                                           br(),
                                           br(),
                                           'Please let us know if you have any feedback or if you encounter an error by sending an email to ', tags$a(href="mailto:neal.haddaway@sei.org", "neal.haddaway@sei.org"),
                                           br(),
                                           br(),
                                           hr()),
                                    
                                    column(12, offset = 1,
                                           # tags$a(href="https://ndownloader.figshare.com/files/25593458", "Download template CSV file here"),
                                           br(),
                                           br()),
                                    
                                    column(3, offset = 1,
                                    ),
                                    fluidRow(
                                    ),
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
                         tabPanel("Create flow diagram",
                                  shinyjs::useShinyjs(),
                                  sidebarLayout(
                                    sidebarPanel(style = "overflow-y:scroll; max-height: 900px; position:relative;",
                                           h3("Main options"),
                                           splitLayout(selectInput("previous", "Previous studies", choices = c('Not included', 'Included')),
                                                       selectInput("other", "Other searches for studies", choices = c('Included', 'Not included')),
                                                       tags$head(tags$style(HTML("
                                                       .shiny-split-layout > div {
                                                       overflow: visible;
                                                       }
                              ")))
                                                       ),
                                           hr(),
                                           
                                           h3("Identification"),
                                           conditionalPanel(
                                             condition = "input.previous == 'Included'",
                                             splitLayout(textInput("previous_studies", label = "Previous studies", value = template$n[5]),
                                             textInput("previous_reports", label = "Previous reports", value = template$n[5]))),
                                           splitLayout(textInput("database_results", label = "Databases", value = template$n[5]),
                                                       textInput("register_results", label = "Registers", value = template$n[5])),
                                           conditionalPanel(
                                             condition = "input.other == 'Included'",
                                             splitLayout(textInput("website_results", label = "Websites", value = template$n[5]),
                                                         textInput("organisation_results", label = "Organisations", value = template$n[5])),
                                             textInput("citations_results", label = "Citations", value = template$n[5])
                                             ),
                                           textInput("duplicates", label = "Duplicates removed", value = template$n[5]),
                                           splitLayout(textInput("excluded_automatic", label = "Automatically excluded", value = template$n[5]),
                                                       textInput("excluded_other", label = "Other exclusions", value = template$n[5])),
                                           h3("Screening"),
                                           splitLayout(textInput("records_screened", label = "Records screened", value = template$n[5]),
                                                       textInput("records_excluded", label = "Records excluded", value = template$n[5])),
                                           splitLayout(textInput("dbr_sought_reports", label = "Reports sought", value = template$n[5]),
                                                       textInput("dbr_notretrieved_reports", label = "Reports not retrieved", value = template$n[5])),
                                           conditionalPanel(
                                             condition = "input.other == 'Included'",
                                             splitLayout(textInput("other_sought_reports", label = "Other reports sought", value = template$n[5]),
                                                         textInput("other_notretrieved_reports", label = "Other reports not retrieved", value = template$n[5]))
                                           ),
                                           splitLayout(textInput("dbr_assessed", label = "Reports assessed", value = template$n[5]),
                                                       textInput("dbr_excluded", label = "Reports excluded", value = template$n[5])),
                                           conditionalPanel(
                                             condition = "input.other == 'Included'",
                                             splitLayout(textInput("other_assessed", label = "Other reports assessed", value = template$n[5]),
                                                         textInput("other_excluded", label = "Other reports excluded", value = template$n[5]))
                                           ),
                                           h3("Included"),
                                           splitLayout(textInput("new_studies", label = "New studies", value = template$n[5]),
                                                       textInput("new_reports", label = "New reports", value = template$n[5])),
                                           conditionalPanel(
                                             condition = "input.previous == 'Included'",
                                             splitLayout(textInput("total_studies", label = "Total studies", value = template$n[5]),
                                                         textInput("total_reports", label = "Total reports", value = template$n[5]))
                                           ),
                                           hr(),
                                           
                                           h3("Download"),
                                           downloadButton('PRISMAflowdiagramPDF', 'Download PDF'),
                                           downloadButton('PRISMAflowdiagramPNG', 'Download PNG')
                                    ), 
                                    mainPanel(
                                           DiagrammeR::grVizOutput(outputId = "plot1", width = "100%", height = "700px"))
                                  ))
                         ))



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Define reactive values
  rv <- reactiveValues()
  
  # Data Handling ----
  
  # Use template data to populate editable table
  observe({
    rv$data <- template
  })
  
  # Text box
  observeEvent(input$previous_studies,{
    rv$data[which(rv$data$data == "previous_studies"), "n"] <- input$previous_studies
  })
  observeEvent(input$previous_reports,{
    rv$data[which(rv$data$data == "previous_reports"), "n"] <- input$previous_reports
  })
  observeEvent(input$register_results,{
    rv$data[which(rv$data$data == "register_results"), "n"] <- input$register_results
  })
  observeEvent(input$database_results,{
    rv$data[which(rv$data$data == "database_results"), "n"] <- input$database_results
  })
  
  observeEvent(input$website_results,{
    rv$data[which(rv$data$data == "website_results"), "n"] <- input$website_results
  })
  observeEvent(input$organisation_results,{
    rv$data[which(rv$data$data == "organisation_results"), "n"] <- input$organisation_results
  })
  observeEvent(input$citations_results,{
    rv$data[which(rv$data$data == "citations_results"), "n"] <- input$citations_results
  })
  
  observeEvent(input$duplicates,{
    rv$data[which(rv$data$data == "duplicates"), "n"] <- input$duplicates
  })
  observeEvent(input$excluded_automatic,{
    rv$data[which(rv$data$data == "excluded_automatic"), "n"] <- input$excluded_automatic
  })
  observeEvent(input$excluded_other,{
    rv$data[which(rv$data$data == "excluded_other"), "n"] <- input$excluded_other
  })
  observeEvent(input$records_screened,{
    rv$data[which(rv$data$data == "records_screened"), "n"] <- input$records_screened
  })
  observeEvent(input$records_excluded,{
    rv$data[which(rv$data$data == "records_excluded"), "n"] <- input$records_excluded
  })
  observeEvent(input$dbr_sought_reports,{
    rv$data[which(rv$data$data == "dbr_sought_reports"), "n"] <- input$dbr_sought_reports
  })
  observeEvent(input$dbr_notretrieved_reports,{
    rv$data[which(rv$data$data == "dbr_notretrieved_reports"), "n"] <- input$dbr_notretrieved_reports
  })
  observeEvent(input$other_sought_reports,{
    rv$data[which(rv$data$data == "other_sought_reports"), "n"] <- input$other_sought_reports
  })
  observeEvent(input$other_notretrieved_reports,{
    rv$data[which(rv$data$data == "other_notretrieved_reports"), "n"] <- input$other_notretrieved_reports
  })
  observeEvent(input$dbr_assessed,{
    rv$data[which(rv$data$data == "dbr_assessed"), "n"] <- input$dbr_assessed
  })
  observeEvent(input$dbr_excluded,{
    rv$data[which(rv$data$data == "dbr_excluded"), "n"] <- input$dbr_excluded
  })
  observeEvent(input$other_assessed,{
    rv$data[which(rv$data$data == "other_assessed"), "n"] <- input$other_assessed
  }) 
  observeEvent(input$other_excluded,{
    rv$data[which(rv$data$data == "other_excluded"), "n"] <- input$other_excluded
  })   
  
  observeEvent(input$new_studies,{
    rv$data[which(rv$data$data == "new_studies"), "n"] <- input$new_studies
  })
  observeEvent(input$new_reports,{
    rv$data[which(rv$data$data == "new_reports"), "n"] <- input$new_reports
  })
  observeEvent(input$total_studies,{
    rv$data[which(rv$data$data == "total_studies"), "n"] <- input$total_studies
  })
  observeEvent(input$total_reports,{
    rv$data[which(rv$data$data == "total_reports"), "n"] <- input$total_reports
  })
  
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
    if (input$previous == 'Included'){
      include_previous = TRUE
    } else {
      include_previous = FALSE
    }
    if (input$other == 'Included'){
      include_other = TRUE
    } else {
      include_other = FALSE
    }
    plot <- PRISMA_flowdiagram(data,
                             interactive = FALSE,
                             previous = include_previous,
                             other = include_other)
  })
  
  
  # Display plot
  output$plot1 <- DiagrammeR::renderDiagrammeR({
    plot <- plot()
  })
  
  
  # Handle downloads ----
  output$PRISMAflowdiagramPDF <- downloadHandler(
    filename = "prisma.pdf",
    content = function(file){
      prisma_pdf(plot(), 
                 file)
    }
  )
  output$PRISMAflowdiagramPNG <- downloadHandler(
    filename = "prisma.png",
    content = function(file){
      prisma_png(plot(), 
                 file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


