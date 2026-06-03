#' Read in PRISMA flow diagram data
#'
#' @description Read in a template CSV containing data for the flow diagram
#' @param data File to read in.
#' @return A list of objects needed to plot the flow diagram
#' @examples
#' csvFile <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
#' data <- read.csv(csvFile);
#' data <- PRISMA_data(data);
#' @export
PRISMA_data <- function(data) { #nolint
  # Ensure data is a df, not a tibble;
  # tibbles do not return vectors using df[, 1].
  data <- as.data.frame(data)
  #Set parameters
  previous_studies <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "previous_studies",
          data[, 1]
        ),
      ]$n
    )
  )
  previous_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "previous_reports",
          data[, 1]
        ),
      ]$n
    )
  )
  register_results <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "register_results",
          data[, 1]
        ),
      ]$n
    )
  )
  database_results <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "database_results",
          data[, 1]
        ),
      ]$n
    )
  )
  database_specific_results <- PRISMA_parse_reasons_(data[ #nolint
    grep(
      "database_specific_results",
      data[, 1]
    ),
  ]$n
  )
  register_specific_results <- PRISMA_parse_reasons_(data[ #nolint
    grep(
      "register_specific_results",
      data[, 1]
    ),
  ]$n
  )
  website_results <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "website_results",
          data[, 1]
        ),
      ]$n
    )
  )
  organisation_results <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "organisation_results",
          data[, 1]
        ),
      ]$n
    )
  )
  citations_results <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "citations_results",
          data[, 1]
        ),
      ]$n
    )
  )
  duplicates <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "duplicates",
          data[, 1]
        ),
      ]$n
    )
  )
  excluded_automatic <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "excluded_automatic",
          data[, 1]
        ),
      ]$n
    )
  )
  excluded_other <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "excluded_other",
          data[, 1]
        ),
      ]$n
    )
  )
  records_screened <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "records_screened",
          data[, 1]
        ),
      ]$n
    )
  )
  records_excluded <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "records_excluded",
          data[, 1]
        ),
      ]$n
    )
  )
  dbr_sought_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "dbr_sought_reports",
          data[, 1]
        ),
      ]$n
    )
  )
  dbr_notretrieved_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "dbr_notretrieved_reports",
          data[, 1]
        ),
      ]$n
    )
  )
  other_sought_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "other_sought_reports",
          data[, 1]
        ),
      ]$n
    )
  )
  other_notretrieved_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "other_notretrieved_reports",
          data[, 1]
        ),
      ]$n
    )
  )
  dbr_assessed <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "dbr_assessed",
          data[, 1]
        ),
      ]$n
    )
  )
  dbr_excluded <- PRISMA_parse_reasons_(data[ #nolint
    grep(
      "dbr_excluded",
      data[, 1]
    ),
  ]$n
  )
  other_assessed <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "other_assessed",
          data[, 1]
        ),
      ]$n
    )
  )
  other_excluded <- PRISMA_parse_reasons_(data[ #nolint
    grep(
      "other_excluded",
      data[, 1]
    ),
  ]$n
  )
  new_studies <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "new_studies",
          data[, 1]
        ),
      ]$n
    )
  )
  new_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "new_reports",
          data[, 1]
        ),
      ]$n
    )
  )
  total_studies <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "total_studies$",
          data[, 1]
        ),
      ]$n
    )
  )
  total_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "total_reports$",
          data[, 1]
        ),
      ]$n
    )
  )
  total_studies_ma <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "total_studies_ma",
          data[, 1]
        ),
      ]$n
    )
  )
  total_reports_ma <- scales::comma(
    PRISMA_format_number_( #nolint
      data[
        grep(
          "total_reports_ma",
          data[, 1]
        ),
      ]$n
    )
  )
  tooltips <- list()
  for (i in seq_len(nrow(data))) {
    if (!is.na(data[i, ]$tooltips)) {
      if (is.na(data[i, ]$data)) {
        name <- data[i, ]$box
      } else {
        name <- data[i, ]$data
      }
      tooltips[[name]] <- data[i, ]$tooltips
    }
  }
  
  urls <- data.frame(
    box = data[!duplicated(data$box), ]$box,
    url = data[!duplicated(data$box), ]$url
  )
  #set text - if text >33 characters,
  previous_text <- data[grep("prevstud", data[, 3]), ]$boxtext
  newstud_text <- data[grep("newstud", data[, 3]), ]$boxtext
  other_text <- data[grep("othstud", data[, 3]), ]$boxtext
  previous_studies_text <- data[grep("previous_studies", data[, 1]), ]$boxtext
  previous_reports_text <- data[grep("previous_reports", data[, 1]), ]$boxtext
  register_results_text <- data[grep("register_results", data[, 1]), ]$boxtext
  database_results_text <- data[grep("database_results", data[, 1]), ]$boxtext
  website_results_text <- data[grep("website_results", data[, 1]), ]$boxtext
  organisation_results_text <- data[
    grep(
      "organisation_results",
      data[, 1]
    ),
  ]$boxtext
  citations_results_text <- data[grep("citations_results", data[, 1]), ]$boxtext
  duplicates_text <- data[grep("duplicates", data[, 1]), ]$boxtext
  excluded_automatic_text <- data[
    grep(
      "excluded_automatic",
      data[, 1]
    ),
  ]$boxtext
  excluded_other_text <- data[grep("excluded_other", data[, 1]), ]$boxtext
  records_screened_text <- data[grep("records_screened", data[, 1]), ]$boxtext
  records_excluded_text <- data[grep("records_excluded", data[, 1]), ]$boxtext
  dbr_sought_reports_text <- data[
    grep(
      "dbr_sought_reports",
      data[, 1]
    ),
  ]$boxtext
  dbr_notretrieved_reports_text <- data[
    grep(
      "dbr_notretrieved_reports",
      data[, 1]
    ),
  ]$boxtext
  other_sought_reports_text <- data[
    grep(
      "other_sought_reports",
      data[, 1]
    ),
  ]$boxtext
  other_notretrieved_reports_text <- data[ #nolint
    grep(
      "other_notretrieved_reports",
      data[, 1]
    ),
  ]$boxtext
  dbr_assessed_text <- data[grep("dbr_assessed", data[, 1]), ]$boxtext
  dbr_excluded_text <- data[grep("dbr_excluded", data[, 1]), ]$boxtext
  other_assessed_text <- data[grep("other_assessed", data[, 1]), ]$boxtext
  other_excluded_text <- data[grep("other_excluded", data[, 1]), ]$boxtext
  new_studies_text <- data[grep("new_studies", data[, 1]), ]$boxtext
  new_reports_text <- data[grep("new_reports", data[, 1]), ]$boxtext
  total_studies_text <- data[grep("total_studies$", data[, 1]), ]$boxtext
  total_reports_text <- data[grep("total_reports$", data[, 1]), ]$boxtext
  identification_text <- data[grep("identification", data[, 1]), ]$boxtext
  screening_text <- data[grep("screening", data[, 1]), ]$boxtext
  included_text <- data[grep("included", data[, 1]), ]$boxtext
  total_studies_ma_text <- data[grep("total_studies_ma", data[, 1]), ]$boxtext
  total_reports_ma_text <- data[grep("total_reports_ma", data[, 1]), ]$boxtext
  x <- list(
    previous_studies = previous_studies,
    previous_reports = previous_reports,
    register_results = register_results,
    database_results = database_results,
    database_specific_results = database_specific_results,
    register_specific_results = register_specific_results,
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
    total_studies_ma = total_studies_ma,
    total_reports_ma = total_reports_ma,
    previous_text = previous_text,
    newstud_text = newstud_text,
    other_text = other_text,
    previous_studies_text = previous_studies_text,
    previous_reports_text = previous_reports_text,
    register_results_text = register_results_text,
    database_results_text = database_results_text,
    website_results_text = website_results_text,
    organisation_results_text = organisation_results_text,
    citations_results_text = citations_results_text,
    duplicates_text = duplicates_text,
    excluded_automatic_text = excluded_automatic_text,
    excluded_other_text = excluded_other_text,
    records_screened_text = records_screened_text,
    records_excluded_text = records_excluded_text,
    dbr_sought_reports_text = dbr_sought_reports_text,
    dbr_notretrieved_reports_text = dbr_notretrieved_reports_text,
    other_sought_reports_text = other_sought_reports_text,
    other_notretrieved_reports_text = other_notretrieved_reports_text,
    dbr_assessed_text = dbr_assessed_text,
    dbr_excluded_text = dbr_excluded_text,
    other_assessed_text = other_assessed_text,
    other_excluded_text = other_excluded_text,
    new_studies_text = new_studies_text,
    new_reports_text = new_reports_text,
    total_studies_text = total_studies_text,
    total_reports_text = total_reports_text,
    total_studies_ma_text = total_studies_ma_text,
    total_reports_ma_text = total_reports_ma_text,
    identification_text = identification_text,
    screening_text = screening_text,
    included_text = included_text,
    tooltips = tooltips,
    urls = urls
  )
  return(x)
}
