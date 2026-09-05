library(shiny)
library(shinyjs)
library(rsvg)
library(DT) #nolint
library(rio)
library(PRISMA2020) #nolint
library(dplyr)
library(bslib)

utils::globalVariables(c(
  "n",
  "included"
))

template <- read.csv("www/PRISMA.csv", stringsAsFactors = FALSE) #nolint
the_options <- c(
  "Not Included",
  "Included",
  "Not Included",
  "Not Included",
  "Not Included"
)
names(the_options) <- c(
  "previous",
  "other",
  "dbDetail",
  "regDetail",
  "metaAnalysis"
)

PRISMA2020_theme <- bs_theme() #nolint
PRISMA2020_theme <- bs_add_rules(theme = PRISMA2020_theme, ".kofi-donate-popover { max-width: none; }") #nolint

# the below enables us to utilise analytics when pushing to shinyapps.io.
# if self hosting you can insert your own analytics code here
# it is your responsibility to ensure compliance with regulations such as
# the EU GDPR. we use a self-hosted version of umami,
# configured not to store any cookies or personally identifiable data.
# We also respect the "do-not-track" header.

prisma_citation <- "Haddaway, N. R., Page, M. J., Pritchard, C. C., &
          McGuinness, L. A. (2022). PRISMA2020: An R package
          and Shiny app for producing PRISMA 2020-compliant flow
          diagrams, with interactivity for optimised digital transparency
          and Open Synthesis. Campbell Systematic Reviews, 18, e1230."
analytics <- if (Sys.getenv("PRISMA_ANALYTICS") == TRUE) { #nolint
  tagList(
    tags$script(
      src = "https://u.210812.xyz/script.js", # nolint
      "async",
      "defer",
      "data-website-id" = "01a071e6-45f0-76f2-9934-25178c77923b", # nolint
      "data-do-not-track" = "true", # nolint
      "data-host-url" = "https://u.210812.xyz", # nolint
      "data-domains" = "estech.shinyapps.io" # nolint
    ),
    tags$script(
      "async",
      "defer",
      "src" = "https://badge.dimensions.ai/badge.js",
      "charset" = "utf-8"
    ),
    tags$script(
      "async",
      "defer",
      "type" = "text/javascript",
      "src" = "https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js"
    )
  )
}
kofi_load <- if (Sys.getenv("KOFI_DONATE") == TRUE) {
  tags$script(
    type = "text/javascript",
    src = "kofi_Widget_2.js",
    "async",
    "defer"
  )
}
kofi_show <- if (Sys.getenv("KOFI_DONATE") == TRUE) {
  tagList(
    tags$script(
      type = "text/javascript",
      src = "kofi.js",
    ),
    popover(
      actionLink(
        inputId = "kofi_donate",
        label = tagList(
          span(
            class = "kofitext",
            tagList(
              img(
                src = "https://storage.ko-fi.com/cdn/cup-border.png",
                alt = "Ko-fi donations",
                class = "kofiimg"
              ),
              "Support me"
            )
          )
        ),
        class = "kofi-button",
        style = "background-color:#794bc4;"
      ),
      tags$iframe(
        id = "kofiframe",
        src = "https://ko-fi.com/chriscpritchard/?hidefeed=true&widget=true&embed=true&preview=true",
        style = "border:none;width:100%;padding:4px",
        height = 712,
        title = "chriscpritchard"
      ),
      options = list(customClass = "kofi-donate-popover")
    )
  )
  #<span class="kofitext"><img src="https://storage.ko-fi.com/cdn/cup-border.png" alt="Ko-fi donations" class="kofiimg">Support me</span>
}
ui <- tagList( #nolint
  tags$head(
    tags$script(
      src = "labels.js"
    ),
    tags$link(
      rel = "shortcut icon", #nolint
      href = "favicon.ico" #nolint
    ),
    kofi_load,
    analytics
  ),
  page_navbar(
    theme = PRISMA2020_theme,
    title = tagList(
      img(
        src = "PRISMA2020-hex.png",
        width = "45px",
      ),
      "PRISMA2020 Flow Diagram"
    ),
    navbar_options = navbar_options(position = "fixed-top"),
    padding = c(70, 0, 0, 0),
    footer = div(
      style = "padding-top:5px; padding-bottom:2px; padding-left:5px; display: flex;",
      div(
        style = "flex-grow:1;",
        a(
          href = "https://github.com/prisma-flowdiagram/PRISMA2020",
          style = "text-decoration: none;",
          img(
            style = "height:40px;width:40px",
            src = "https://pngimg.com/uploads/github/github_PNG40.png"
          )
        ),
        "Created November 2020, Updated June 2026"
      ),
      div(
        kofi_show
      )
    ),
    # Tab 1 ----
    nav_panel(
      "Home",
      card(
        h4(
          "To get started, click \"Create flow diagram\" above,
          or read the instructions below for more information."
        ),
        h4("Introduction"),
        p(
          "Systematic reviews should be described in
          a high degree of methodological detail. ",
          a(
            href = "http://prisma-statement.org/",
            "The PRISMA Statement"
          ),
          "calls for a high level of reporting detail in
          systematic reviews and meta-analyses. An integral
          part of the methodological description of a review
          is a flow diagram. This tool allows you to produce a flow diagram
          for your own review that conforms to ",
          a(
            href = "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003583", # nolint
            "the PRISMA2020 Statement."
          ),
        ),
        h4("General workflow"),
        p(
          "You can provide the numbers in the data entry section
          of the 'Create flow diagram' tab.
          The app allows you to export the flowchart in a variety of formats.
          The flowchart is initiated with the values from a template csv file,
          which you can",
          a(
            href = "PRISMA.csv",
            "download.",
            download = NA,
            target = "_blank"
          ),
          "Although you can edit this csv file manually,
          it's not very convenient.
          But if you want to translate the text in the
          flowchart to another language,
          you can do it there. After creating your flowchart in the web app,
          you can also export it as a csv.
          Then when you upload that csv file below
          and open the 'Create flow diagram' tab,
          you will see the same flowchart again."
        ),
        h4("Initite the web app using custom URLs"),
        p(
          "These numbers will be initialised to any values provided in the
          URL query string. For example, if you provide the URL path:
          '?website_results=100&organisation_results=200', this will initialise
          the website results to 100 and the organisation results to 200.
          The name of the query string parameter should match the name of the
          'data' column in the template file below. The additional arguments
          \"previous\", \"other\", \"dbDetail\", and \"regDetail\" can be
          used to set the initial main options for further customisation.
          Alternatively, you can use the template file to specify any
          values, and to change some of the labels
          within the diagram (see above). "
        ),
        h4("R package", code("PRISMA2020")),
        p(
          "We also provide an R package:",
          a(
            href = "https://github.com/prisma-flowdiagram/PRISMA2020",
            "PRISMA2020 flow diagram R package on Github."
          ),
          "This package contain the function",
          code("PRISMA2020::PRISMA_flowdiagram()"),
          "which is the backbone function for this web app.
          You can use this function to programmatically create
          the same flowcharts as in the web app.",
        ),
        h4("Feedback and comments"),
        p(
          "Please let us know if you have any feedback or
          if you encounter an error by creating an",
          a(
            href = "https://github.com/prisma-flowdiagram/PRISMA2020/issues",
            "issue on GitHub"
          ),
        ),
        h4("Upload csv"),
        p(
          "If you have created a flowchart using the web app,
          and exported it as a csv,
          you can upload it here to recreate the exact same figure.
          Also,If you have downloaded the csv template and modified it directly,
          you can upload that file.",
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
          )
        ),
        h4("Citing Us"),
        p(
          prisma_citation,
          a(
            href = "https://doi.org/10.1002/cl2.1230",
            "https://doi.org/10.1002/cl2.1230"
          ),
        ),
        a(
          href = "Haddaway_et_al_2022.ris",
          "Download citation (.ris)",
          download = NA,
          target = "_blank"
        ),
        h4("Credits:"),
        p(
          "Neal R Haddaway (creator, author)",
          br(),
          "Luke A McGuinness (coder, author)",
          br(),
          "Chris C Pritchard (coder, author, maintainer)",
          br(),
          "Brennan Chapman (coder, contributor)",
          br(),
          "Hossam Hammady (coder, contributor)",
          br(),
          "Anders Kolstad (coder, contributor)",
          br(),
          "Shreya Dimri (coder, contributor)",
          br(),
          "Matt Lloyd Jones (coder, contributor)",
          br(),
          "John-o Kulas (coder, contributor)",
          br(),
          "Matthew J Page (advisor)",
          br(),
          "Jack Wasey (advisor)",
        ),
      )
    ),
    # Tab 2 ----
    nav_panel(
      "Create flow diagram",
      shinyjs::useShinyjs(),
      card(
      layout_sidebar(
        sidebar = sidebar(
          width = "25%",
          resizable = FALSE,
          open = "always",
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
          ),
          downloadButton(
            "PRISMAflowdiagramCSV",
            "CSV"
          ),
          h3("Reset"),
          actionButton(
            "reset",
            "Click to reset"
          ),
        ),
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
      nav_panel(
        "Privacy & Impact",
        card(
          h4("Privacy"),
          p(
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
            be set within your browser preferences."
          ),
          p(
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
          ),
          h4("Impact"),
          p(
            "The site's usage can be viewed",
            tags$a(
              href = "https://u.210812.xyz/share/IR8kPofbDG6rT0F9", #nolint
              "on the public dashboard."
            ),
          ),
          p(
            "Our",
            tags$a(
              href = "https://doi.org/10.1002/cl2.1230",
              "article"
            ),
            "metrics are:",
            div(
              style = "display: flex;gap: 100px",
              div(
                "class" = "__dimensions_badge_embed__",
                "data-doi" = "10.1002/cl2.1230",
                "data-legend" = "hover-right",
                "data-style" = "small_circle",
                "width" = "64"
              ),
              div(
                "class" = "altmetric-embed",
                "data-badge-type" = "donut",
                "data-doi" = "10.1002/cl2.1230",
                "width" = "64"
              )
            )
          ),
          p(
            "We also published a",
            a(
              href = "https://doi.org/10.1002/cl2.1230",
              "preprint."
            ),
            "Our metrics for the preprint are:",
            div(
              style = "display: flex; gap:100px;",
              div(
                "class" = "__dimensions_badge_embed__",
                "data-doi" = "10.1101/2021.07.14.21260492",
                "data-legend" = "hover-right",
                "data-style" = "small_circle",
                "width" = "64"
              ),
              div(
                "class" = "altmetric-embed",
                "data-badge-type" = "donut",
                "data-doi" = "10.1101/2021.07.14.21260492",
                "width" = "64"
              )
            )
          )
        )
      )
    }
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) { #nolint
  # Define reactive values
  rv <- shiny::reactiveValues()
  # Define modals
  thank_you_modal <- modalDialog(
    easyClose = TRUE,
    title = "Thank You",
    "Thank you for using the PRISMA Flow Diagram tool.
          Your flow diagram is being downloaded.",
    hr(),
    "Please remember to cite the tool as: ",
    br(),
    prisma_citation,
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
        if ("metaAnalysis" %in% names(query)) {
          if (query$metaAnalysis == 1) {
            the_options["metaAnalysis"] <- "Included"
          } else if (query$metaAnalysis == 0) {
            the_options["metaAnalysis"] <- "Not Included"
          }
        }
        for (i in seq_len(nrow(template))) {
          if (!is.null(query[[template[i, "data"]]])) {
            template[i, "n"] <- query[[template[i, "data"]]]
          }
        }
      }
      # Create initial value that is passed to UI
      rv$data_initial <- template
      rv$opts_initial <- the_options
      # Create version that is edited and passed to graphing function
      rv$data <- template
      rv$opts <- the_options
    } else {
      # Create initial value that is passed to UI
      rv$data_initial <- read.csv(input$data_upload$datapath)
      previous_load <- rv$data_initial |>
        dplyr::filter(
          data == "previous_studies" | data == "previous_reports"
        ) |>
        dplyr::summarise(
          included = any(n != "0")
        ) |>
        dplyr::pull(.data$included)

      db_detail_load <- rv$data_initial |>
        dplyr::filter(
          data == "database_specific_results"
        ) |>
        dplyr::summarise(
          included = any(
            n != "Database 1, xxx; Database 2, xxx; Database 3, xxx"
          )
        ) |>
        dplyr::pull(.data$included)

      reg_detail_load <- rv$data_initial |>
        dplyr::filter(data == "register_specific_results") |>
        dplyr::summarise(
          included = any(
            n != "Register 1, xxx; Register 2, xxx; Register 3, xxx"
          )
        ) |>
        dplyr::pull(.data$included)

      meta_analysis_load <- rv$data_initial |>
        dplyr::filter(
          data == "total_studies_ma" | data == "total_reports_ma"
        ) |>
        dplyr::summarise(included = any(n != "0")) |>
        dplyr::pull(.data$included)

      the_options <- c(
        previous = if (previous_load) "Included" else "Not Included",
        other = "Included",
        dbDetail = if (db_detail_load) "Included" else "Not Included",
        regDetail = if (reg_detail_load) "Included" else "Not Included",
        metaAnalysis = if (meta_analysis_load) "Included" else "Not Included"
      )
      rv$opts_initial <- the_options
      # Create version that is edited and passed to graphing function
      rv$data <- read.csv(input$data_upload$datapath)
      rv$opts <- the_options
    }
  })
  # Reset to upload button
  observeEvent(
    input$reset,
    {
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
    input$reset_data_upload,
    {
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
      ),
      selectInput(
        "metaAnalysis",
        "Meta analysis",
        choices = c(
          "Not Included",
          "Included"
        ),
        selected = rv$opts_initial["metaAnalysis"]
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
              which(rv$data_initial$data == "organisation_results"),
              "n"
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
        condition = "input.metaAnalysis == 'Included'",
        splitLayout(
          textInput(
            "total_studies_ma",
            label = "Total studies (MA)",
            value = rv$data_initial[
              which(rv$data_initial$data == "total_studies_ma"),
              "n"
            ]
          ),
          textInput(
            "total_reports_ma",
            label = "Total Reports (MA)",
            value = rv$data_initial[
              which(rv$data_initial$data == "total_reports_ma"),
              "n"
            ]
          )
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
  observeEvent(input$total_studies_ma, {
    rv$data[
      which(rv$data$data == "total_studies_ma"),
      "n"
    ] <- input$total_studies_ma
  })
  observeEvent(input$total_reports_ma, {
    rv$data[
      which(rv$data$data == "total_reports_ma"),
      "n"
    ] <- input$total_reports_ma
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
  observeEvent(input$metaAnalysis, {
    rv$opts["metaAnalysis"] <- input$metaAnalysis
  })
  # Define table proxy
  proxy <- DT::dataTableProxy("mytable")
  # Update reactive dataset on cell edit
  observeEvent(
    input$mytable_cell_edit,
    {
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
        rownames = FALSE
      ) # important
    }
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
    if (rv$opts["metaAnalysis"] == "Included") {
      meta_analysis <- TRUE
    } else {
      meta_analysis <- FALSE
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
      meta_analysis = meta_analysis,
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
      PRISMA2020::PRISMA_save(plot(), filename = file, filetype = "PDF")
    }
  )
  output$PRISMAflowdiagramPNG <- downloadHandler( #nolint
    filename = "prisma.png",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(), filename = file, filetype = "PNG")
    }
  )
  output$PRISMAflowdiagramSVG <- downloadHandler( #nolint
    filename = "prisma.svg",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(), filename = file, filetype = "SVG")
    }
  )
  output$PRISMAflowdiagramHTML <- downloadHandler( #nolint
    filename = "prisma.html",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(), filename = file, filetype = "html")
    }
  )
  output$PRISMAflowdiagramZIP <- downloadHandler( #nolint
    filename = "prisma.zip",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      PRISMA2020::PRISMA_save(plot(), filename = file, filetype = "zip")
    }
  )

  output$PRISMAflowdiagramCSV <- downloadHandler(
    #nolint
    filename = "prisma.csv",
    content = function(file) {
      showModal(
        thank_you_modal
      )
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
