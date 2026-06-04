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
    PRISMA_format_number_(
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "previous_studies",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "previous_studies"
      )
    )
  )
  previous_reports <- scales::comma(
    PRISMA_format_number_(
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "previous_reports",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "previous_reports"
      )
    )
  )
  register_results <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "register_results",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "register_results"
      )
    )
  )
  database_results <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "database_results",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "database_results"
      )
    )
  )
  database_specific_results <- PRISMA_parse_reasons_(
    PRISMA_default_or_csv_(
      expr = data[ #nolint
        grep(
          "database_specific_results",
          data[, 1]
        ),
      ]$n,
      default = "Database 1, xxx; Database 2, xxx; Database 3, xxx",
      var_name = "database_specific_results"
    )
  )
  register_specific_results <- PRISMA_parse_reasons_(
    PRISMA_default_or_csv_(
      expr = data[ #nolint
        grep(
          "register_specific_results",
          data[, 1]
        ),
      ]$n,
      default = "Register 1, xxx; Register 2, xxx; Register 3, xxx",
      var_name = "register_specific_results"
    )
  )
  website_results <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "website_results",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "website_results"
      )
    )
  )
  organisation_results <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "organisation_results",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "organisation_results"
      )
    )
  )
  citations_results <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "citations_results",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "citations_results"
      )
    )
  )
  duplicates <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "duplicates",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "duplicates"
      )
    )
  )
  excluded_automatic <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "excluded_automatic",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "excluded_automatic"
      )
    )
  )
  excluded_other <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "excluded_other",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "excluded_other"
      )
    )
  )
  records_screened <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "records_screened",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "records_screened"
      )
    )
  )
  records_excluded <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
       expr = data[
        grep(
          "records_excluded",
          data[, 1]
        ),
      ]$n,
      default = 0,
      var_name = "records_excluded"
      )
    )
  )
  dbr_sought_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
        grep(
          "dbr_sought_reports",
          data[, 1]
        ),
      ]$n,
      default = 0,
      var_name = "dbr_sought_reports"
      )
    )
  )
  dbr_notretrieved_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
        grep(
          "dbr_notretrieved_reports",
          data[, 1]
        ),
      ]$n,
      default = 0,
      var_name = "dbr_notretrieved_reports"
      )
    )
  )
  other_sought_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
      expr = data[
        grep(
          "other_sought_reports",
          data[, 1]
        ),
      ]$n,
      default = 0,
      var_name = "other_sought_reports"
      )
    )
  )
  other_notretrieved_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
        grep(
          "other_notretrieved_reports",
          data[, 1]
        ),
      ]$n,
      default = 0,
      var_name = "other_notretrieved_reports"
      )
    )
  )
  dbr_assessed <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
      expr = data[
        grep(
          "dbr_assessed",
          data[, 1]
        ),
      ]$n,
      default = 0,
      var_name = "dbr_assessed"
      )
    )
  )
  dbr_excluded <- PRISMA_parse_reasons_(
    PRISMA_default_or_csv_(
      expr = data[ #nolint
        grep(
          "dbr_excluded",
          data[, 1]
       ),
      ]$n,
      default = "Reason1, xxx; Reason2, xxx; Reason3, xxx",
      var_name = "dbr_excluded"
    )
  )
  other_assessed <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "other_assessed",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "other_assessed"
      )
    )
  )
  other_excluded <- PRISMA_parse_reasons_(
    PRISMA_default_or_csv_(
      expr = data[ #nolint
        grep(
          "other_excluded",
          data[, 1]
        ),
      ]$n,
      default = "Reason1, xxx; Reason2, xxx; Reason3, xxx",
      var_name = "other_excluded"
    )
  )
  new_studies <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "new_studies",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "new_studies"
      )
    )
  )
  new_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "new_reports",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "new_reports"
      )
    )
  )
  total_studies <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "total_studies$",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "total_studies$"
      )
    )
  )
  total_reports <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "total_reports$",
          data[, 1]
          ),
      ]$n,
      default = 0,
      var_name = "total_reports$"
      )
    )
  )
  total_studies_ma <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "total_studies_ma",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "total_studies_ma"
      )
    )
  )
  total_reports_ma <- scales::comma(
    PRISMA_format_number_( #nolint
      PRISMA_default_or_csv_(
        expr = data[
          grep(
            "total_reports_ma",
            data[, 1]
          ),
        ]$n,
        default = 0,
        var_name = "total_reports_ma"
      )
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
  previous_text <- PRISMA_default_or_csv_(
    expr = data[grep("prevstud", data[, 3]), ]$boxtext,
    default = "Previous studies",
    var_name = previous_text
  )
  newstud_text <- PRISMA_default_or_csv_(
    expr = data[grep("newstud", data[, 3]), ]$boxtext,
    default = "Identification of new studies via databases and registers",
    var_name = newstud_text
  )
  other_text <- PRISMA_default_or_csv_(
    expr = data[grep("othstud", data[, 3]), ]$boxtext,
    default = "Identification of new studies via other methods",
    var_name = other_text
  )
  previous_studies_text <- PRISMA_default_or_csv_(
    expr = data[grep("previous_studies", data[, 1]), ]$boxtext,
    default = "Studies included in previous version of review",
    var_name = previous_studies_text
  )
  previous_reports_text <- PRISMA_default_or_csv_(
    expr = data[grep("previous_reports", data[, 1]), ]$boxtext,
    default = "Reports of studies included in previous version of review",
    var_name = previous_reports_text
  )
  register_results_text <- PRISMA_default_or_csv_(
    expr = data[grep("register_results", data[, 1]), ]$boxtext,
    default = "Registers",
    var_name = register_results_text
  )
  database_results_text <- PRISMA_default_or_csv_(
    expr = data[grep("database_results", data[, 1]), ]$boxtext,
    default = "Databases",
    var_name = database_results_text
  )
  website_results_text <- PRISMA_default_or_csv_(
    expr = data[grep("website_results", data[, 1]), ]$boxtext,
    default = "Websites",
    var_name = website_results_text
  )
  organisation_results_text <- PRISMA_default_or_csv_(
    expr = data[grep("organisation_results", data[, 1]),]$boxtext,
    default = "Organisations",
    var_name = organisation_results_text
  )
  citations_results_text <- PRISMA_default_or_csv_(
    expr = data[grep("citations_results", data[, 1]), ]$boxtext, 
    default = "Citation searching", 
    var_name = citations_results_text
  )
  duplicates_text <- PRISMA_default_or_csv_(
    expr = data[grep("duplicates", data[, 1]), ]$boxtext,
    default = "Duplicate records",
    var_name = duplicates_text
  )
  excluded_automatic_text <- PRISMA_default_or_csv_(
    expr = data[grep("excluded_automatic", data[, 1]),]$boxtext,
    default = "Records marked as ineligible by automation tools",
    var_name = excluded_automatic_text
  )
  excluded_other_text <- PRISMA_default_or_csv_(
    expr = data[grep("excluded_other", data[, 1]), ]$boxtext,
    default = "Records removed for other reasons",
    var_name = excluded_other_text
  )
  records_screened_text <- PRISMA_default_or_csv_(
    expr = data[grep("records_screened", data[, 1]), ]$boxtext,
    default = "Records screened",
    var_name = records_screened_text
  )
  records_excluded_text <- PRISMA_default_or_csv_( 
    expr = data[grep("records_excluded", data[, 1]), ]$boxtext,
    default = "Records excluded",
    var_name = records_excluded_text
  )
  dbr_sought_reports_text <- PRISMA_default_or_csv_( 
    expr = data[grep("dbr_sought_reports", data[, 1]),]$boxtext,
    default = "Reports sought for retrieval",
    var_name = dbr_sought_reports_text
  )
  dbr_notretrieved_reports_text <- PRISMA_default_or_csv_(
    expr = data[grep("dbr_notretrieved_reports", data[, 1]),]$boxtext,
    default = "Reports not retrieved",
    var_name = dbr_notretrieved_reports_text
  )
  other_sought_reports_text <- PRISMA_default_or_csv_(
    expr = data[ grep("other_sought_reports", data[, 1]),]$boxtext,
    default = "Reports sought for retrieval",
    var_name = other_sought_reports_text
  )
  other_notretrieved_reports_text <- PRISMA_default_or_csv_(
    expr = data[ #nolint
    grep("other_notretrieved_reports",data[, 1]),]$boxtext,
    default = "Reports not retrieved",
    var_name = other_notretrieved_reports_text
  )
  dbr_assessed_text <- PRISMA_default_or_csv_(
    expr = data[grep("dbr_assessed", data[, 1]), ]$boxtext,
    default = "Reports assessed for eligibility", 
    var_name = dbr_assessed_text
  )
  dbr_excluded_text <- PRISMA_default_or_csv_(
    expr = data[grep("dbr_excluded", data[, 1]), ]$boxtext,
    default = "Reports excluded",
    var_name = dbr_excluded_text 
  )
  other_assessed_text <- PRISMA_default_or_csv_(
    expr = data[grep("other_assessed", data[, 1]), ]$boxtext,
    default = "Reports assessed for eligibility",
    var_name = other_assessed_text
  )
  other_excluded_text <- PRISMA_default_or_csv_(
    expr = data[grep("other_excluded", data[, 1]), ]$boxtext,
    default = "Reports excluded", 
    var_name = other_excluded_text
  )
  new_studies_text <- PRISMA_default_or_csv_(
    expr = data[grep("new_studies", data[, 1]), ]$boxtext,
    default = "New studies included in review",
    var_name = new_studies_text
  )
  new_reports_text <- PRISMA_default_or_csv_(
    expr = data[grep("new_reports", data[, 1]), ]$boxtext,
    default = "Reports of new included studies",
    var_name = new_reports_text
  )
  total_studies_text <- PRISMA_default_or_csv_(
    expr = data[grep("total_studies$", data[, 1]), ]$boxtext,
    default = "Total studies included in review",
    var_name = total_studies_text
  )
  total_reports_text <- PRISMA_default_or_csv_(
    expr = data[grep("total_reports$", data[, 1]), ]$boxtext,
    default = "Reports of total included studies",
    var_name = total_reports_text
  )
  identification_text <- PRISMA_default_or_csv_(
    expr = data[grep("identification", data[, 1]), ]$boxtext,
    default = "Identification",
    var_name = identification_text
  )
  screening_text <- PRISMA_default_or_csv_(
    expr = data[grep("screening", data[, 1]), ]$boxtext,
    default = "Screening",
    var_name = screening_text
  )
  included_text <- PRISMA_default_or_csv_(
    expr = data[grep("included", data[, 1]), ]$boxtext,
    default = "Included",
    var_name = included_text
  )
  total_studies_ma_text <- PRISMA_default_or_csv_(
    expr = data[grep("total_studies_ma", data[, 1]), ]$boxtext,
    default = "Total studies included in meta-analysis",
    var_name = total_studies_ma_text
  )
  total_reports_ma_text <- PRISMA_default_or_csv_(
    expr = data[grep("total_reports_ma", data[, 1]), ]$boxtext,
    default = "Reports of total included studies in meta-analysis",
    var_name = total_reports_ma_text
  )
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