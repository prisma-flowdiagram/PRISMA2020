library(shiny)
library(shinyjs)
library(rsvg)
library(DT) #nolint
library(rio)
library(devtools)
library(PRISMA2020) #nolint

template <- read.csv("www/PRISMA.csv", stringsAsFactors = FALSE) #nolint
the_options <- c(
  "Not Included",
  "Included",
  "Not Included",
  "Not Included"
)
names(the_options) <- c(
  "previous",
  "other",
  "dbDetail",
  "regDetail"
)

# Define UI for application that draws a histogram
ui <- tagList( #nolint
  tags$head(
    tags$script(
      src = "labels.js"
    ),
    tags$style(
      type = "text/css",#nolint
      "body {padding-top: 70px;}", #nolint
      "body {padding-bottom: 50px;}" #nolint
    ),
    tags$link(
      rel = "shortcut icon", #nolint
      href = "favicon.ico" #nolint
    ),
    # the below enables us to utilise analytics when pushing to shinyapps.io.
    # if self hosting you can insert your own analytics code here
    # it is your responsibility to ensure compliance with regulations such as
    # the EU GDPR. we use a self-hosted version of umami,
    # configured not to store any cookies or personally identifiable data.
    # We also respect the "do-not-track" header.
    analytics <- if (Sys.getenv("PRISMA_ANALYTICS") == TRUE) { #nolint
      tags$script(
        src = "https://umami.christopherpritchard.co.uk/umami.js", # nolint
        "async",
        "defer",
        "data-website-id" = "72f80a48-0dea-4914-9619-465de3df82a4", # nolint
        "data-do-not-track" = "true", # nolint
        "data-host-url" = "https://umami.christopherpritchard.co.uk", # nolint
        "data-domains" = "estech.shinyapps.io" # nolint
      )
    },
    kofi_load <- if (Sys.getenv("KOFI_DONATE") == TRUE) { #nolint
      tags$script(
        src='https://storage.ko-fi.com/cdn/scripts/overlay-widget.js' # nolint
      )
    },
    kofi_show <- if (Sys.getenv("KOFI_DONATE") == TRUE) { #nolint
      tags$script(
        src = "kofi.js"
      )
    }
  ),
  navbarPage(
    "PRISMA Flow Diagram",
    position = "fixed-top",
    # Tab 1 ----
    tabPanel("Home",
      fluidRow(
        column(10, offset = 1,
          h4("To get started, click \"Create flow diagram\" above,
          or read the instructions below for more information."),
          br(),
          "Systematic reviews should be described in
          a high degree of methodological detail. ",
          tags$a(
            href = "http://prisma-statement.org/",
            "The PRISMA Statement"
          ),
          "calls for a high level of reporting detail in
          systematic reviews and meta-analyses. An integral
          part of the methodological description of a review
          is a flow diagram.",
          br(),
          br(),
          "This tool allows you to produce a flow diagram
          for your own review that conforms to ",
          tags$a(
            href = "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003583", # nolint
            "the PRISMA2020 Statement."
          ),
          "You can provide the numbers in the data entry section
          of the 'Create flow diagram' tab.
          These numbers will be initialised to any values provided in the
          URL query string. For example, if you provide the URL path:
          '?website_results=100&organisation_results=200', this will initialise
          the website results to 100 and the organisation results to 200.
          The name of the query string parameter should match the name of the
          'data' column in the template file below. The additional arguments
          \"previous\", \"other\", \"dbDetail\", and \"regDetail\" can be
          used to set the initial main options for further customisation.
          Alternatively, you can use the template file below to specify any
          values, and to change somé of the labels within the diagram.",
          br(),
          br(),
          "This tool also allows you to download an interactive HTML
          version of the plot, alongside several other common formats.",
          br(),
          br(),
          "We also provide an R package:",
          tags$a(
            href = "https://github.com/prisma-flowdiagram/PRISMA2020",
            "PRISMA2020 flow diagram R package on Github."
          ),
          br(),
          br(),
          "Please let us know if you have any feedback or
          if you encounter an error by creating an",
          tags$a(
            href = "https://github.com/prisma-flowdiagram/PRISMA2020/issues",
            "issue on GitHub"
          ),
          br(),
          br(),
          tags$a(
            href = "PRISMA.csv",
            "Download the template CSV file",
            download = NA,
            target = "_blank"
          ),
          br(),
          br(),
          "Upload your edited file here:",
          br(),
          fileInput(
            "data_upload",
            "Choose CSV File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/plain",
              ".csv"
            )
          ),
          hr(),
          "Please cite as:",
          br(),
          "Haddaway, N. R., Page, M. J., Pritchard, C. C., &
          McGuinness, L. A. (2022). PRISMA2020: An R package
          and Shiny app for producing PRISMA 2020-compliant flow
          diagrams, with interactivity for optimised digital transparency
          and Open Synthesis Campbell Systematic Reviews, 18, e1230.",
          tags$a(
            href = "https://doi.org/10.1002/cl2.1230",
            "https://doi.org/10.1002/cl2.1230"
          ),
          br(),
          tags$a(
            href = "Haddaway_et_al_2022.ris",
            "Download citation (.ris)",
            download = NA,
            target = "_blank"
          )
        )
      ),
      fluidRow(
        column(
          10,
          offset = 1,
          br(),
          "Credits:",
          br(),
          "Neal R Haddaway (creator, author)",
          br(),
          "Luke A McGuinness (coder, author)",
          br(),
          "Chris C Pritchard (coder, author)",
          br(),
          "Matthew J Page (advisor)",
          br(),
          "Jack Wasey (advisor)",
          br(),
          br(),
          tags$a(
            href = "https://github.com/prisma-flowdiagram/PRISMA2020",
            tags$img(
              height = 40,
              width = 40,
              src = "https://pngimg.com/uploads/github/github_PNG40.png"
            )
          ),
          "Created November 2020, Updated June 2022"
        )
      ),
      kofi_load,
      kofi_show
    ),
    # Tab 2 ----
    tabPanel(
      "Create flow diagram",
      shinyjs::useShinyjs(),
      sidebarLayout(
        sidebarPanel(
          style = "overflow-y:scroll; max-height: 900px; position:relative;",
          tags$head(
            tags$style(
              HTML(
                ".shiny-split-layout > div { overflow: visible; }"
              )
            )
          ),
          div(
            id = "options",
            uiOutput("options")
          ),
          hr(),
          actionButton(
            "reset",
            "Click to reset"
          ),
          hr(),
          div(
            id = "inputs",
            uiOutput("selection")
          ),
          hr(),
          h3("Download"),
          downloadButton(
            "PRISMAflowdiagramPDF",
            "PDF"
          ),
          downloadButton(
            "PRISMAflowdiagramPNG",
            "PNG"
          ),
          downloadButton(
            "PRISMAflowdiagramSVG",
            "SVG"
          ),
          downloadButton(
            "PRISMAflowdiagramHTML",
            "Interactive HTML"
          ),
          downloadButton(
            "PRISMAflowdiagramZIP",
            "Interactive HTML (ZIP)"
          )
        ),
        mainPanel(
          DiagrammeR::grVizOutput(
            outputId = "plot1",
            width = "100%",
            height = "700px"
          )
        )
      )
    ),
    # the below analytics information should be updated to reflect
    # analytics that you use. It is your responsibility to ensure
    # compliance with regulations such as the EU GDPR
    # this section will not be shown unless the PRISMA_ANALYTICS
    # environment variable is set at runtime
    anaytics_info <- if (Sys.getenv("PRISMA_ANALYTICS") == TRUE) {
      tabPanel(
        "Privacy & Impact",
        tags$script(
          "async",
          "src" = "https://badge.dimensions.ai/badge.js",
          "charset" = "utf-8"
        ),
        tags$script(
          "type" = "text/javascript",
          "src" = "https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js"
        ),
        fluidRow(
          column(
            width = 10,
            offset = 1,
            "We use",
            tags$a(
              href = "https://umami.is",
              "Umami"
            ),
            "analytics to identify how our website
            is used and accessed. We do not collect any
            personally identifiable data, nor do we use cookies
            or local browser storage. All data collected for this purpose
            is anonymised. We also respect the 'do-not-track' header that can
            be set within your browser preferences.",
            br(),
            br(),
            "The site's usage can be viewed",
            tags$a(
              href =
                "https://umami.christopherpritchard.co.uk/share/DaPFWd0Q/Prisma%20Flow%20Diagram", #nolint
              "on the public dashboard."
            ),
            br(),
            br(),
            "RStudio collects data in line with their",
            tags$a(
              href = "https://www.rstudio.com/legal/privacy-policy/",
              "Privacy Policy"
            ),
            "for the neccesary functioning of their cloud products,
            including our hosting provider,",
            tags$a(
              href = "https://shinyapps.io",
              "shinyapps.io"
            ),
            br(),
            br(),
            hr(),
            "Our",
            tags$a(
              href = "https://doi.org/10.1002/cl2.1230",
              "article"
            ),
            "metrics are:",
            br()
          ),
        ),
        fluidRow(
          column(1, offset = 1,
            tags$div(
              "class" = "__dimensions_badge_embed__",
              "data-doi" = "10.1002/cl2.1230",
              "data-legend" = "hover-right",
              "data-style" = "small_circle",
              "width" = "64"
            )
          ),
          column(1, offset = 0,
            tags$div(
              "class" = "altmetric-embed",
              "data-badge-type" = "donut",
              "data-doi" = "10.1002/cl2.1230",
              "width" = "64"
            )
          ),
        ),
        fluidRow(
          column(10, offset = 1,
            br(),
            "We also published a",
            tags$a(
              href = "https://doi.org/10.1002/cl2.1230",
              "preprint."
            ),
            "Our metrics for the preprint are:",
            br(),
            br()
          )
        ),
        fluidRow(
          column(1, offset = 1,
            tags$div(
              "class" = "__dimensions_badge_embed__",
              "data-doi" = "10.1101/2021.07.14.21260492",
              "data-legend" = "hover-right",
              "data-style" = "small_circle",
              "width" = "64"
            )
          ),
          column(1, offset = 0,
            tags$div(
              "class" = "altmetric-embed",
              "data-badge-type" = "donut",
              "data-doi" = "10.1101/2021.07.14.21260492",
              "width" = "64"
            )
          ),
        )
      )
    }
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Define reactive values
  rv <- shiny::reactiveValues()
  # Data Handling ----
  # Use template data to populate editable table
  observe({
    if (is.null(input$data_upload)) {
      # Override default template with query string parameters if present
      query <- parseQueryString(session$clientData$url_search)
      if (length(query) > 0) {
        if ("previous" %in% names(query)) {
          if (query$previous == 1) {
            the_options["previous"] <- "Included"
          } else if (query$previous == 0) {
            the_options["previous"] <- "Not Included"
          }
        }
        if ("other" %in% names(query)) {
          if (query$other == 1) {
            the_options["other"] <- "Included"
          } else if (query$other == 0) {
            the_options["other"] <- "Not Included"
          }
        }
        if ("dbDetail" %in% names(query)) {
          if (query$dbDetail == 1) {
            the_options["dbDetail"] <- "Included"
          } else if (query$dbDetail == 0) {
            the_options["dbDetail"] <- "Not Included"
          }
        }
        if ("regDetail" %in% names(query)) {
          if (query$regDetail == 1) {
            the_options["regDetail"] <- "Included"
          } else if (query$regDetail == 0) {
            the_options["regDetail"] <- "Not Included"
          }
        }
        for (i in seq_len(nrow(template))) {
          if (!is.null(query[[template[i, "data"]]])) {
            template[i, "n"] <- query[[template[i, "data"]]]
          }
        }
      }
      # Create inital value that is passed to UI
      rv$data_initial <- template
      rv$opts_initial <- the_options
      # Create version that is edited and passed to graphing function
      rv$data <- template
      rv$opts <- the_options
    } else {
      # Create inital value that is passed to UI
      rv$data_initial <- read.csv(input$data_upload$datapath)
      rv$opts_initial <- the_options
      # Create version that is edited and passed to graphing function
      rv$data <- read.csv(input$data_upload$datapath)
      rv$opts <- the_options
    }
  })

  # Reset to upload button
  observeEvent(
    input$reset, {
      if (is.null(input$data_upload)) {
        # Create version that is edited and passed to graphing function
        rv$data <- template
      } else {
        # Create version that is edited and passed to graphing function
        rv$data <- read.csv(input$data_upload$datapath)
      }
      shinyjs::reset("inputs")
      shinyjs::reset("options")
    }
  )
  # Reset to blank button
  observeEvent(
    input$reset_data_upload, {
      shinyjs::reset("data_upload")
    }
  )
  # Set up default options
  output$options <- renderUI({
    tagList(
      h3("Main options"),
      splitLayout(
        selectInput(
          "previous",
          "Previous studies",
          choices = c(
            "Not Included",
            "Included"
          ),
          selected = rv$opts_initial["previous"]
        ),
        selectInput(
          "other",
          "Other searches for studies",
          choices = c(
            "Not Included",
            "Included"
          ),
          selected = rv$opts_initial["other"]
        )
      ),
          splitLayout(
            selectInput(
              "dbDetail",
              "Individual databases",
              choices = c(
                "Not Included",
                "Included"
              ),
              selected = rv$opts_initial["dbDetail"]
            ),
            selectInput(
              "regDetail",
              "Individual registers",
              choices = c(
                "Not Included",
                "Included"
              ),
              selected = rv$opts_initial["regDetail"]
            )
          )
    )
  })
  # Set up default values in data entry boxes
  output$selection <- renderUI({
    tagList(
      h3("Identification"),
      conditionalPanel(
        condition = "input.previous == 'Included'",
        splitLayout(
          textInput(
            "previous_studies",
            label = "Previous studies",
            value = rv$data_initial[
              which(rv$data_initial$data == "previous_studies"),
                "n"
            ]
          ),
          textInput(
            "previous_reports",
            label = "Previous reports",
            value = rv$data_initial[
              which(rv$data_initial$data == "previous_reports"),
              "n"
            ]
          )
        )
      ),
      splitLayout(
        textInput(
          "database_results",
          label = "Databases",
          value = rv$data_initial[
            which(rv$data_initial$data == "database_results"),
            "n"
          ]
        ),
        textInput(
          "register_results",
          label = "Registers",
          value = rv$data_initial[
            which(rv$data_initial$data == "register_results"),
            "n"
          ]
        )
      ),
      conditionalPanel(
        condition = "
          input.dbDetail == 'Included' || input.regDetail == 'Included'
        ",
        splitLayout(
          textInput(
            "database_specific_results",
            label = "Specific Database Results",
            value = rv$data_initial[
              which(rv$data_initial$data == "database_specific_results"),
              "n"
            ]
          ),
          textInput(
            "register_specific_results",
            label = "Specific Register Results",
            value = rv$data_initial[
              which(rv$data_initial$data == "register_specific_results"),
              "n"
            ]
          )
        )
      ),
      conditionalPanel(
        condition = "input.other == 'Included'",
        splitLayout(
          textInput(
            "website_results",
            label = "Websites",
            value = rv$data_initial[
              which(rv$data_initial$data == "website_results"),
              "n"
            ]
          ),
          textInput(
            "organisation_results",
            label = "Organisations",
            value = rv$data_initial[
              which(rv$data_initial$data == "organisation_results"), "n"
            ]
          )
        ),
        textInput(
          "citations_results",
          label = "Citations",
          value = rv$data_initial[
            which(rv$data_initial$data == "citations_results"),
            "n"
          ]
        )
      ),
      textInput(
        "duplicates",
        label = "Duplicates removed",
        value = rv$data_initial[
          which(rv$data_initial$data == "duplicates"),
          "n"
        ]
      ),
      splitLayout(
        textInput(
          "excluded_automatic",
          label = "Automatically excluded",
          value = rv$data_initial[
            which(rv$data_initial$data == "excluded_automatic"),
            "n"
          ]
        ),
        textInput(
          "excluded_other",
          label = "Other exclusions",
          value = rv$data_initial[
            which(rv$data_initial$data == "excluded_other"),
            "n"
          ]
        )
      ),
      h3("Screening"),
      splitLayout(
        textInput(
          "records_screened",
          label = "Records screened",
          value = rv$data_initial[
            which(rv$data_initial$data == "records_screened"),
            "n"
          ]
        ),
        textInput(
          "records_excluded",
          label = "Records excluded",
          value = rv$data_initial[
            which(rv$data_initial$data == "records_excluded"),
            "n"
          ]
        )
      ),
      splitLayout(
        textInput(
          "dbr_sought_reports",
          label = "Reports sought",
          value = rv$data_initial[
            which(rv$data_initial$data == "dbr_sought_reports"),
            "n"
          ]
        ),
        textInput(
          "dbr_notretrieved_reports",
          label = "Reports not retrieved",
          value = rv$data_initial[
            which(rv$data_initial$data == "dbr_notretrieved_reports"),
            "n"
          ]
        )
      ),
      conditionalPanel(
        condition = "input.other == 'Included'",
        splitLayout(
          textInput(
            "other_sought_reports",
            label = "Other reports sought",
            value = rv$data_initial[
              which(rv$data_initial$data == "other_sought_reports"),
              "n"
            ]
          ),
          textInput(
            "other_notretrieved_reports",
            label = "Other reports not retrieved",
            value = rv$data_initial[
              which(rv$data_initial$data == "other_notretrieved_reports"),
              "n"
            ]
          )
        )
      ),
      splitLayout(
        textInput(
          "dbr_assessed",
          label = "Reports assessed",
          value = rv$data_initial[
            which(rv$data_initial$data == "dbr_assessed"),
            "n"
          ]
        ),
        textInput(
          "dbr_excluded",
          label = "Reports excluded",
          value = rv$data_initial[
            which(rv$data_initial$data == "dbr_excluded"),
            "n"
          ]
        )
      ),
      conditionalPanel(
        condition = "input.other == 'Included'",
        splitLayout(
          textInput(
            "other_assessed",
            label = "Other reports assessed",
            value = rv$data_initial[
              which(rv$data_initial$data == "other_assessed"),
              "n"
            ]
          ),
          textInput(
            "other_excluded",
            label = "Other reports excluded",
            value = rv$data_initial[
              which(rv$data_initial$data == "other_excluded"),
              "n"
            ]
          )
        )
      ),
      h3("Included"),
      splitLayout(
        textInput(
          "new_studies",
          label = "New studies",
          value = rv$data_initial[
            which(rv$data_initial$data == "new_studies"),
            "n"
          ]
        ),
        textInput(
          "new_reports",
          label = "New reports",
          value = rv$data_initial[
            which(rv$data_initial$data == "new_reports"),
            "n"
          ]
        )
      ),
      conditionalPanel(
        condition = "input.previous == 'Included'",
        splitLayout(
          textInput(
            "total_studies",
            label = "Total studies",
            value = rv$data_initial[
              which(rv$data_initial$data == "total_studies"),
              "n"
            ]
          ),
          textInput(
            "total_reports",
            label = "Total reports",
            value = rv$data_initial[
              which(rv$data_initial$data == "total_reports"),
              "n"
            ]
          )
        )
      )
    )
  })
  # Text box
  observeEvent(input$previous_studies, {
    rv$data[
      which(rv$data$data == "previous_studies"),
      "n"
    ] <- input$previous_studies
  })
  observeEvent(input$previous_reports, {
    rv$data[
      which(rv$data$data == "previous_reports"),
      "n"
    ] <- input$previous_reports
  })
  observeEvent(input$register_results, {
    rv$data[
      which(rv$data$data == "register_results"),
      "n"
    ] <- input$register_results
  })
  observeEvent(input$database_results, {
    rv$data[
      which(rv$data$data == "database_results"),
      "n"
    ] <- input$database_results
  })
  observeEvent(input$database_specific_results, {
    rv$data[
      which(rv$data$data == "database_specific_results"),
      "n"
    ] <- input$database_specific_results
  })
  observeEvent(input$register_specific_results, {
    rv$data[
      which(rv$data$data == "register_specific_results"),
      "n"
    ] <- input$register_specific_results
  })
  observeEvent(input$website_results, {
    rv$data[
      which(rv$data$data == "website_results"),
      "n"
    ] <- input$website_results
  })
  observeEvent(input$organisation_results, {
    rv$data[
      which(rv$data$data == "organisation_results"),
      "n"
    ] <- input$organisation_results
  })
  observeEvent(input$citations_results, {
    rv$data[
      which(rv$data$data == "citations_results"),
      "n"
    ] <- input$citations_results
  })
  observeEvent(input$duplicates, {
    rv$data[
      which(rv$data$data == "duplicates"),
      "n"
    ] <- input$duplicates
  })
  observeEvent(input$excluded_automatic, {
    rv$data[
      which(rv$data$data == "excluded_automatic"),
      "n"
    ] <- input$excluded_automatic
  })
  observeEvent(input$excluded_other, {
    rv$data[
      which(rv$data$data == "excluded_other"),
      "n"
    ] <- input$excluded_other
  })
  observeEvent(input$records_screened, {
    rv$data[
      which(rv$data$data == "records_screened"),
      "n"
    ] <- input$records_screened
  })
  observeEvent(input$records_excluded, {
    rv$data[
      which(rv$data$data == "records_excluded"),
      "n"
    ] <- input$records_excluded
  })
  observeEvent(input$dbr_sought_reports, {
    rv$data[
      which(rv$data$data == "dbr_sought_reports"),
      "n"
    ] <- input$dbr_sought_reports
  })
  observeEvent(input$dbr_notretrieved_reports, {
    rv$data[
      which(rv$data$data == "dbr_notretrieved_reports"),
      "n"
    ] <- input$dbr_notretrieved_reports
  })
  observeEvent(input$other_sought_reports, {
    rv$data[
      which(rv$data$data == "other_sought_reports"),
      "n"
    ] <- input$other_sought_reports
  })
  observeEvent(input$other_notretrieved_reports, {
    rv$data[
      which(rv$data$data == "other_notretrieved_reports"),
      "n"
    ] <- input$other_notretrieved_reports
  })
  observeEvent(input$dbr_assessed, {
    rv$data[
      which(rv$data$data == "dbr_assessed"),
      "n"
    ] <- input$dbr_assessed
  })
  observeEvent(input$dbr_excluded, {
    rv$data[
      which(rv$data$data == "dbr_excluded"),
      "n"
    ] <- input$dbr_excluded
  })
  observeEvent(input$other_assessed, {
    rv$data[
      which(rv$data$data == "other_assessed"),
      "n"
    ] <- input$other_assessed
  })
  observeEvent(input$other_excluded, {
    rv$data[
      which(rv$data$data == "other_excluded"),
      "n"
    ] <- input$other_excluded
  })
  observeEvent(input$new_studies, {
    rv$data[
      which(rv$data$data == "new_studies"),
      "n"
    ] <- input$new_studies
  })
  observeEvent(input$new_reports, {
    rv$data[
      which(rv$data$data == "new_reports"),
      "n"
    ] <- input$new_reports
  })
  observeEvent(input$total_studies, {
    rv$data[
      which(rv$data$data == "total_studies"),
      "n"
    ] <- input$total_studies
  })
  observeEvent(input$total_reports, {
    rv$data[
      which(rv$data$data == "total_reports"),
      "n"
    ] <- input$total_reports
  })
  observeEvent(input$previous, {
    rv$opts["previous"] <- input$previous
  })
  observeEvent(input$other, {
    rv$opts["other"] <- input$other
  })
  observeEvent(input$dbDetail, {
    rv$opts["dbDetail"] <- input$dbDetail
  })
  observeEvent(input$regDetail, {
    rv$opts["regDetail"] <- input$regDetail
  })
  # Define table proxy
  proxy <- DT::dataTableProxy("mytable")
  # Update reactive dataset on cell edit
  observeEvent(
    input$mytable_cell_edit, {
      info <- input$mytable_cell_edit
      # Define edited row
      i <- info$row
      # Define edited column (column index offset by 4, because you are hiding
      # the rownames column and the first 3 columns of the data)
      j <- info$col + 4L
      # Define value of edit
      v <- info$value
      # Pass edited value to appropriate cell of data stored in rv$data
      rv$data[i, j] <- shiny::coerceValue(v, rv$data[i, j])
      # Replace data in table with updated data stored in rv$data
      replaceData(
        proxy,
        rv$data,
        resetPaging = FALSE,
        rownames = FALSE)  # important
  })
  # Define thank you modal
  thank_you_modal <- modalDialog(
          easyClose = TRUE,
          title = "Thank You",
          "Thank you for using the PRISMA Flow Diagram tool.
          Your flow diagram is being downloaded.",
          hr(),
          "Please remember to cite the tool as: ",
          br(),
          "Haddaway, N. R., Page, M. J., Pritchard, C. C., &
          McGuinness, L. A. (2022). PRISMA2020: An R package
          and Shiny app for producing PRISMA 2020-compliant flow
          diagrams, with interactivity for optimised digital transparency
          and Open Synthesis Campbell Systematic Reviews, 18, e1230.",
          tags$a(
            href = "https://doi.org/10.1002/cl2.1230",
            "https://doi.org/10.1002/cl2.1230"
          ),
          br(),
          tags$a(
            href = "Haddaway_et_al_2022.ris",
            "Download citation (.ris)",
            download = NA,
            target = "_blank"
          )
  )
  # Reactive plot ----
  # Create plot
  plot <- reactive({
    data <- PRISMA2020::PRISMA_data(rv$data)
    if (rv$opts["previous"] == "Included") {
      include_previous <- TRUE
    } else {
      include_previous <- FALSE
    }
    if (rv$opts["other"] == "Included") {
      include_other <- TRUE
    } else {
      include_other <- FALSE
    }
    if (rv$opts["dbDetail"] == "Included") {
      detail_databases <- TRUE
    } else {
      detail_databases <- FALSE
    }
    if (rv$opts["regDetail"] == "Included") {
      detail_registers <- TRUE
    } else {
      detail_registers <- FALSE
    }
    shinyjs::runjs(
      paste0(
        'const nodeMap = new Map([["node1","',
        rv$data[which(rv$data$data == "identification"), "boxtext"],
        '"], ["node2","',
        rv$data[which(rv$data$data == "screening"), "boxtext"],
        '"], ["node3","',
        rv$data[which(rv$data$data == "included"), "boxtext"],
        '"]])',
        "\n",
        "createLabels(nodeMap)"
      )
    )
    plot <- PRISMA2020::PRISMA_flowdiagram(
      data,
      fontsize = 12,
      font = "Helvetica",
      interactive = TRUE,
      previous = include_previous,
      other = include_other,
      side_boxes = TRUE,
      detail_databases = detail_databases,
      detail_registers = detail_registers
    )
  })
  # Display plot
  output$plot1 <- DiagrammeR::renderDiagrammeR({
    plot <- plot()
  })
  # Handle downloads ----
  output$PRISMAflowdiagramPDF <- downloadHandler( #nolint
    filename = "prisma.pdf",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "PDF")
    }
  )
  output$PRISMAflowdiagramPNG <- downloadHandler( #nolint
    filename = "prisma.png",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "PNG")
    }
  )
  output$PRISMAflowdiagramSVG <- downloadHandler( #nolint
    filename = "prisma.svg",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "SVG")
    }
  )
  output$PRISMAflowdiagramHTML <- downloadHandler( #nolint
    filename = "prisma.html",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "html")
    }
  )
  output$PRISMAflowdiagramZIP <- downloadHandler( #nolint
    filename = "prisma.zip",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(),
                 filename = file, filetype = "zip")
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
