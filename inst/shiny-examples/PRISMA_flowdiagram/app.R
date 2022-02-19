library(shiny)
library(shinyjs)
library(rsvg)
library(DT)
library(rio)
library(devtools)
# This will enable us to host the latest version on shinyapps.io once
# The new function names are merged.
# This can be removed once we are on CRAN
# If the library is already installed, this won't do anything
if(!require(PRISMA2020)) {
  devtools::install_github("nealhaddaway/PRISMA2020")
}
library(PRISMA2020)


template <- read.csv("www/PRISMA.csv", stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- tagList(
  tags$head(tags$script(src = "labels.js")),
  navbarPage("PRISMA Flow Diagram",
                         # Tab 1 ----
                         tabPanel("Home",
                                  fluidRow(
                                    column(10, offset = 1,
                                           'Systematic reviews should be described in a high degree of methodological detail. ', tags$a(href="http://prisma-statement.org/", "The PRISMA Statement"), 
                                           'calls for a high level of reporting detail in systematic reviews and meta-analyses. An integral part of the methodological description of a review 
                   is a flow diagram.',
                                           br(),
                                           br(),
                                           'This tool allows you to produce a flow diagram for your own review that conforms to ', tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003583", "the PRISMA2020 Statement."), 
                                           'You can provide the numbers in the data entry section of the \'Create flow diagram\' tab. Alternatively, to allow for more customisation, you can use the template file below.',
                                           br(),
                                           br(),
                                           "This tool also allows you to download an interactive HTML version of the plot, alongside several other common formats.",
                                           br(),
                                           br(),
                                           "We also provide an R package:",
                                           tags$a(href="https://github.com/nealhaddaway/PRISMA2020", "PRISMA2020 flow diagram R package on Github."),
                                           br(),
                                           br(),
                                           'Please let us know if you have any feedback or if you encounter an error by creating an', tags$a(href="https://github.com/nealhaddaway/PRISMA2020/issues", "issue on GitHub"),
                                           br(),
                                           br(),
                                           tags$a(href="PRISMA.csv", "Download the template CSV file", download=NA, target="_blank"),
                                           br(),
                                           br(),
                                           'Upload your edited file here:',
                                           br(),
                                           fileInput("data_upload", "Choose CSV File",
                                                     multiple = FALSE,
                                                     accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")),
                                           # actionButton("reset_data_upload", "Click to clear uploaded data",
                                           # style="color: #fff; background-color: #e86868; border-color: #e86868"),
                                           # br(),
                                           hr(),
                                           'Please cite as:',
                                           br(),
                                           'Neal R Haddaway, Chris C Pritchard, Luke A McGuinness. (2021). PRISMA2020: R package and ShinyApp for producing PRISMA 2020 compliant flow diagrams. Zenodo.', 
                                           tags$a(href="http://doi.org/10.5281/zenodo.4287834", "http://doi.org/10.5281/zenodo.4287834"),
                                           br(),
                                           tags$a(href="Haddaway_Pritchard_and_McGuinness2021.ris", "Download citation (.ris)", download=NA, target="_blank")
                                    )
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  fluidRow(
                                    column(10, offset = 1,
                                           br(),
                                           'Credits:',
                                           br(),
                                           'Neal R Haddaway (creator, author)', br(),
                                           'Luke A McGuinness (coder, author)', br(),
                                           'Chris C Pritchard (coder, author)', br(),
                                           'Matthew J Page (advisor)', br(),
                                           'Jack Wasey (advisor)', br(),
                                           br(),
                                           tags$a(href="https://github.com/nealhaddaway/PRISMA2020", tags$img(height = 40, width = 40, src = "https://pngimg.com/uploads/github/github_PNG40.png")), 
                                           'Created November 2020, Updated July 2021'
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
                                                 
                                                 actionButton("reset", "Click to reset"),
                                                 div(id = "inputs",
                                                 h3("Identification"),
                                                 uiOutput("selection")
                                                 ),
                                                 hr(),
                                                 
                                                 h3("Download"),
                                                 downloadButton('PRISMAflowdiagramPDF', 'PDF'),
                                                 downloadButton('PRISMAflowdiagramPNG', 'PNG'),
                                                 downloadButton('PRISMAflowdiagramSVG', 'SVG'),
                                                 downloadButton('PRISMAflowdiagramHTML', 'Interactive HTML'),
                                                 downloadButton('PRISMAflowdiagramZIP', 'Interactive HTML (ZIP)')
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
    if (is.null(input$data_upload)) {
      # Create inital value that is passed to UI
      rv$data_initial <- template
      # Create version that is edited and passed to graphing function
      rv$data <- template
    } else {
      # Create inital value that is passed to UI
      rv$data_initial <- read.csv(input$data_upload$datapath)
      # Create version that is edited and passed to graphing function
      rv$data <- read.csv(input$data_upload$datapath)
    }
  })
  
  # Reset to upload button
  observeEvent(input$reset,{
    if (is.null(input$data_upload)) {
      # Create version that is edited and passed to graphing function
      rv$data <- template
    } else {
      # Create version that is edited and passed to graphing function
      rv$data <- read.csv(input$data_upload$datapath)
    }
    
    shinyjs::reset("inputs")
  })
  
  # Reset to blank button
  observeEvent(input$reset_data_upload,{
    shinyjs::reset("data_upload")
  })
  
  # Set up default values in data entry boxes
  output$selection <- renderUI({
    tagList(conditionalPanel(
      condition = "input.previous == 'Included'",
      splitLayout(textInput("previous_studies", label = "Previous studies", value = rv$data_initial$n[2]),
                  textInput("previous_reports", label = "Previous reports", value = rv$data_initial$n[3]))),
    splitLayout(textInput("database_results", label = "Databases", value = rv$data_initial$n[5]),
                textInput("register_results", label = "Registers", value = rv$data_initial$n[6])),
    conditionalPanel(
      condition = "input.other == 'Included'",
      splitLayout(textInput("website_results", label = "Websites", value = rv$data_initial$n[8]),
                  textInput("organisation_results", label = "Organisations", value = rv$data_initial$n[9])),
      textInput("citations_results", label = "Citations", value = rv$data_initial$n[10])
    ),
    textInput("duplicates", label = "Duplicates removed", value = rv$data_initial$n[11]),
    splitLayout(textInput("excluded_automatic", label = "Automatically excluded", value = rv$data_initial$n[12]),
                textInput("excluded_other", label = "Other exclusions", value = rv$data_initial$n[13])),
    h3("Screening"),
    splitLayout(textInput("records_screened", label = "Records screened", value = rv$data_initial$n[14]),
                textInput("records_excluded", label = "Records excluded", value = rv$data_initial$n[15])),
    splitLayout(textInput("dbr_sought_reports", label = "Reports sought", value = rv$data_initial$n[16]),
                textInput("dbr_notretrieved_reports", label = "Reports not retrieved", value = rv$data_initial$n[17])),
    conditionalPanel(
      condition = "input.other == 'Included'",
      splitLayout(textInput("other_sought_reports", label = "Other reports sought", value = rv$data_initial$n[18]),
                  textInput("other_notretrieved_reports", label = "Other reports not retrieved", value = rv$data_initial$n[19]))
    ),
    splitLayout(textInput("dbr_assessed", label = "Reports assessed", value = rv$data_initial$n[20]),
                textInput("dbr_excluded", label = "Reports excluded", value = rv$data_initial$n[21])),
    conditionalPanel(
      condition = "input.other == 'Included'",
      splitLayout(textInput("other_assessed", label = "Other reports assessed", value = rv$data_initial$n[22]),
                  textInput("other_excluded", label = "Other reports excluded", value = rv$data_initial$n[23]))
    ),
    h3("Included"),
    splitLayout(textInput("new_studies", label = "New studies", value = rv$data_initial$n[24]),
                textInput("new_reports", label = "New reports", value = rv$data_initial$n[25])),
    conditionalPanel(
      condition = "input.previous == 'Included'",
      splitLayout(textInput("total_studies", label = "Total studies", value = rv$data_initial$n[26]),
                  textInput("total_reports", label = "Total reports", value = rv$data_initial$n[27]))
    ))
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
    data <- PRISMA2020::PRISMA_data(rv$data)
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
    shinyjs::runjs(paste0('
       const nodeMap = new Map([["node1","',rv$data[which(rv$data$data == "identification"), "boxtext"],'"], ["node2","',rv$data[which(rv$data$data == "screening"), "boxtext"],'"], ["node3","',rv$data[which(rv$data$data == "included"), "boxtext"],'"]])
       createLabels(nodeMap)
    '))
    plot <- PRISMA2020::PRISMA_flowdiagram(data,
                               fontsize = 12,
                               font = "Helvetica",
                               interactive = TRUE,
                               previous = include_previous,
                               other = include_other,
                               side_boxes = TRUE)
    
  })

  # Display plot
  output$plot1 <- DiagrammeR::renderDiagrammeR({
    plot <- plot()
  })
  
  
  # Handle downloads ----
  output$PRISMAflowdiagramPDF <- downloadHandler(
    filename = "prisma.pdf",
    content = function(file){
      PRISMA2020::PRISMA_save(plot(), 
                 filename = file, filetype = "PDF")
    }
  )
  output$PRISMAflowdiagramPNG <- downloadHandler(
    filename = "prisma.png",
    content = function(file){
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "PNG")
    }
  )
  output$PRISMAflowdiagramSVG <- downloadHandler(
    filename = "prisma.svg",
    content = function(file){
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "SVG")
    }
  )
  output$PRISMAflowdiagramHTML <- downloadHandler(
    filename = "prisma.html",
    content = function(file){
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "html")
    }
  )
  output$PRISMAflowdiagramZIP <- downloadHandler(
    filename = "prisma.zip",
    content = function(file){
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "zip")
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)