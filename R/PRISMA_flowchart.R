#' Plot interactive flow charts for systematic reviews
#' 
#' @description Produces a PRISMA2020 style flow chart for systematic reviews, 
#' with the option to add interactivity through tooltips (mouseover popups) and 
#' hyperlink URLs to each box. Data can be imported from the standard CSV template 
#' provided.
#' @param previous_studies The number of studies included in previous version of 
#' review.
#' @param previous_reports The number of reports of studies included in previous 
#' version of review.
#' @param register_results The number of records identified from registers.
#' @param database_results The number of records identified from databases.
#' @param website_results The number of records identified from websites.
#' @param organisation_results The number of records identified from 
#' organisations.
#' @param citations_results The number of records identified from citation 
#' searching.
#' @param duplicates The number of duplicate records.
#' @param excluded_automatic The number of records marked as ineligible by 
#' automation tools.
#' @param excluded_other The number of records removed for other reasons.
#' @param records_screened The number of records screened for databases and 
#' registers.
#' @param records_excluded The number of records excluded for databases and 
#' registers.
#' @param dbr_sought_reports The number of reports sought for retrieval for 
#' databases and registers.
#' @param dbr_notretrieved_reports The number of reports not retrieved for 
#' databases and registers.
#' @param other_sought_reports The number of reports sought for retrieval 
#' from other sources.
#' @param other_notretrieved_reports The number of reports not retrieved for 
#' retrievalfrom other sources.
#' @param dbr_assessed The number of reports assessed for eligibility for 
#' databases and registers.
#' @param dbr_excluded The number of reports excluded for databases and 
#' registers: (separate reasons and numbers using ; e.g. Reason1, xxx; 
#' Reason2, xxx; Reason3, xxx).
#' @param other_assessed The number of reports assessed for eligibility for 
#' other sources.
#' @param other_excluded The number of reports excluded for other sources: 
#' (separate reasons and numbers using ; e.g. Reason1, xxx; Reason2, xxx; 
#' Reason3, xxx).
#' @param new_studies The number of new studies included in review.
#' @param new_reports The number of reports of new included studies.
#' @param total_studies The number of total studies included in review.
#' @param total_reports The number of total reports included studies.
#' @param interactive Logical argument TRUE or FALSE whether to plot interactivity 
#' (tooltips and hyperlinked boxes).
#' @param tooltips Mouseover popups for each box containing explanatory text. 
#' Should be provided as a vector.
#' @param urls A dataframe of urls to act as hyperlinks for each box, with one column 
#' (named 'box') corresponding to the boxname ('box1', etc.) and one column (named 
#' 'url') containing the urls. A total of 16 URLs should be provided, one for each box. 
#' See the 'PRISMA_flow_schema.png' for details of box numbers, sequentially from 
#' top left to bottom across columns, from left to right. Additional URLs can be given 
#' for the side and top rounded panel boxes if desired.
#' @param font The font for text in each box. The default is 'Helvetica'.
#' @param title_colour The colour for the upper middle title box (new studies). 
#' The default is 'Goldenrod1'. See DiagrammeR colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param greybox_colour The colour for the left and right column boxes. The 
#' default is 'Gainsboro'. See DiagrammeR colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param main_colour The colour for the main box borders. The default is 
#' 'Black'. See DiagrammeR colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param arrow_colour The colour for the connecting lines. The default
#' is 'Black'. See DiagrammeR colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param arrow_head The head shape for the line connectors. The default is 
#' 'normal'. See DiagrammeR arrow shape specification 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#arrow-shapes>.
#' @param arrow_tail The tail shape for the line connectors. The default is 
#' 'none'. See DiagrammeR arrow shape specification 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#arrow-shapes>.
#' @return A flow chart plot.
#' @examples 
#' \dontrun{
#' data <- read.csv(file.choose());
#' data <- read_PRISMAdata(data);
#' attach(data); 
#' plot <- PRISMA_flowchart(previous_studies = previous_studies,
#'                 previous_reports = previous_reports,
#'                 register_results = register_results,
#'                 database_results = database_results,
#'                 website_results = website_results,
#'                 organisation_results = organisation_results,
#'                 citations_results = citations_results,
#'                 duplicates = duplicates,
#'                 excluded_automatic = excluded_automatic,
#'                 excluded_other = excluded_other,
#'                 records_screened = records_screened,
#'                 records_excluded = records_excluded,
#'                 dbr_sought_reports = dbr_sought_reports,
#'                 dbr_notretrieved_reports = dbr_notretrieved_reports,
#'                 other_sought_reports = other_sought_reports,
#'                 other_notretrieved_reports = other_notretrieved_reports,
#'                 dbr_assessed = dbr_assessed,
#'                 dbr_excluded = dbr_excluded,
#'                 other_assessed = other_assessed,
#'                 other_excluded = other_excluded,
#'                 new_studies = new_studies,
#'                 new_reports = new_reports,
#'                 total_studies = total_studies,
#'                 total_reports = total_reports,
#'                 interactive = TRUE,
#'                 tooltips = tooltips,
#'                 urls = urls,
#'                 previous = TRUE);
#' plot
#' }
#' @export
PRISMA_flowchart <- function (previous_studies,
                             previous_reports,
                             register_results,
                             database_results,
                             website_results,
                             organisation_results,
                             citations_results,
                             duplicates,
                             excluded_automatic,
                             excluded_other,
                             records_screened,
                             records_excluded,
                             dbr_sought_reports,
                             dbr_notretrieved_reports,
                             other_sought_reports,
                             other_notretrieved_reports,
                             dbr_assessed,
                             dbr_excluded,
                             other_assessed,
                             other_excluded,
                             new_studies,
                             new_reports,
                             total_studies,
                             total_reports,
                             interactive = FALSE,
                             tooltips = '',
                             urls = '',
                             previous = TRUE,
                             font = 'Helvetica',
                             title_colour = 'Goldenrod1',
                             greybox_colour = 'Gainsboro',
                             main_colour = 'Black',
                             arrow_colour = 'Black',
                             arrow_head = 'normal',
                             arrow_tail = 'none') {
  
  # Node text preparation
  # Left branch
  node1 <- 'Previous studies'
  node2 <- paste0('Studies included in\nprevious version of\nreview (n = ',
                  previous_studies, 
                  ')\n\nReports of studies\nincluded in previous\nversion of review (n = ',
                  previous_reports,
                  ')')
  # Central branch
  node3 <- 'Identification of new studies via databases and registers'
  node4 <- paste0('Records identified from:\n\tDatabases (n = ',
                  database_results,
                  ')\n\tRegisters (n = ',
                  register_results,
                  ')')
  node5 <- paste0('Records removed before\nscreening:\n\tDuplicate records (n = ',
                  duplicates,
                  ')\n\tRecords marked as ineligible\nby automation tools (n = ',
                  excluded_automatic,
                  ')\n\tRecords removed for other\nreasons (n = ',
                  excluded_other,
                  ')')
  node6 <- paste0('Records screened\n(n = ',
                  records_screened,
                  ')')
  node7 <- paste0('Records excluded*\n(n = ',
                  records_excluded,
                  ')')
  node8 <- paste0('Reports sought for retrieval\n(n = ',
                  dbr_sought_reports,
                  ')')
  node9 <- paste0('Reports not retrieved\n(n = ',
                  dbr_notretrieved_reports,
                  ')')
  node10 <- paste0('Reports assessed for eligibility\n(n = ',
                   dbr_assessed,
                   ')')
  node11 <- paste0('Reports excluded:',
                   paste(paste('\n\t', dbr_excluded[,1], ' (n = ', dbr_excluded[,2], ')', sep = ''), collapse = ''))
  node12 <- paste0('New studies included in review\n(n = ',
                   new_studies,
                   ')\n',
                   'Reports of new included studies\n(n = ',
                   new_reports,
                   ')')
  # Right branch
  node13 <- 'Identification of new studies via other methods'
  node14 <- paste0('Records identified from:\n\tWebsites (n = ',
                   website_results,
                   ')\n\tOrganisations (n = ',
                   organisation_results,
                   ')\n\tCitation searching (n = ',
                   citations_results,
                   ')')
  node15 <- paste0('Reports sought for retrieval\n(n = ',
                   other_sought_reports,
                   ')')
  node16 <- paste0('Reports not retrieved\n(n = ',
                   other_notretrieved_reports,
                   ')')
  node17 <- paste0('Reports assessed for eligibility\n(n = ',
                   other_assessed,
                   ')')
  node18 <- paste0('Reports excluded:',
                   paste(paste('\n\t', other_excluded[,1], ' (n = ', other_excluded[,2], ')', sep = ''), collapse = ''))
  node19 <- paste0('Total studies included in review\n(n = ',
                   total_studies,
                   ')\n',
                   'Reports of total included studies\n(n = ',
                   total_reports,
                   ')')
  
  # Produce plot WITH previous
  if (previous == FALSE) {
    x <- DiagrammeR::grViz(
      paste0("digraph TD {
  
  graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information']
  
  # node statements
  node [shape = box]
  identification [image='identification.png', color = LightSteelBlue2, label='', style = 'filled,rounded', pos='-1.5,7.93!', width = 0.4, height = 2.6, tooltip = '", tooltips[20], "'];
  screening [image='screening.png', color = LightSteelBlue2, label='', style = 'filled,rounded', pos='-1.5,4.5!', width = 0.4, height = 3.5, tooltip = '", tooltips[21], "'];
  included [image='included.png', color = LightSteelBlue2, label='', style = 'filled,rounded', pos='-1.5,1.5!', width = 0.4, height = 1.1, tooltip = '", tooltips[22], "'];
  
  node [shape = box,
        fontname = ", font, ",
        color = ", title_colour, "]
  3 [label = '@@3', style = 'rounded,filled', width = 7, height = 0.5, pos='2.5,9!', tooltip = '", tooltips[3], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  4 [label = '@@4', width = 3, width = 3, height = 0.5, height = 0.5, pos='0.5,7.5!', tooltip = '", tooltips[4], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  5 [label = '@@5', width = 3, height = 0.5, pos='4.5,7.5!', tooltip = '", tooltips[7], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  6 [label = '@@6', width = 3, width = 3, height = 0.5, height = 0.5, pos='0.5,5.5!', tooltip = '", tooltips[8], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  7 [label = '@@7', width = 3, height = 0.5, pos='4.5,5.5!', tooltip = '", tooltips[9], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  8 [label = '@@8', width = 3, width = 3, height = 0.5, height = 0.5, pos='0.5,4.5!', tooltip = '", tooltips[10], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  9 [label = '@@9', width = 3, height = 0.5, pos='4.5,4.5!', tooltip = '", tooltips[11], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  10 [label = '@@10', width = 3, height = 0.5, pos='0.5,3.5!', tooltip = '", tooltips[14], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  11 [label = '@@11', width = 3, height = 0.5, pos='4.5,3.5!', tooltip = '", tooltips[15], "']
    
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, ", pos='0.5,4.5!', tooltip = '", tooltips[18], "']
  12 [label = '@@12', pos='0.5,1.5!']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  13 [label = '@@13', style = 'rounded,filled', width = 7, height = 0.5, pos='10,9!', tooltip = '", tooltips[5], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  14 [label = '@@14', style = 'filled', width = 3, height = 0.5, pos='8,7.5!', tooltip = '", tooltips[6], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  15 [label = '@@15', style = 'filled', width = 3, height = 0.5, pos='8,4.5!', tooltip = '", tooltips[12], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  16 [label = '@@16', style = 'filled', width = 3, height = 0.5, pos='12,4.5!', tooltip = '", tooltips[13], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  17 [label = '@@17', style = 'filled', width = 3, height = 0.5, pos='8,3.5!', tooltip = '", tooltips[16], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  18 [label = '@@18', style = 'filled', width = 3, height = 0.5, pos='12,3.5!', tooltip = '", tooltips[17], "']

  
  node [shape = square, width = 0, color=White]
  B [label = '', pos='8,1.5!', tooltip = '']
  
  subgraph cluster1 {
    edge [style = invis]
    3->4; 3->5;
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = ", arrow_tail, ", 
        style = filled]
    4->5;
    4->6; 6->7;
    6->8; 8->9;
    8->10; 10->11;
    10->12;
    edge [style = invis]
    5->7;
    7->9;
    9->11;
    16->18;
  }
  
  subgraph cluster2 {
    edge [color = White, 
          arrowhead = none, 
          arrowtail = none]
    13->14;
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = ", arrow_tail, "]
    14->15; 15->16;
    15->17; 17->18;
    edge [color = ", arrow_colour, ", 
        arrowhead = none, 
        arrowtail = ", arrow_tail, "]
    17->B; 
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = none,
        constraint = FALSE]
    B->12;
  }
  
  {rank = same; 3; 13}
  {rank = same; 4; 5; 14}
  {rank = same; 6; 7}
  {rank = same; 8; 9; 15; 16}
  {rank = same; 10; 11; 17; 18}
  {rank = same; 12; B}
  
  }
  
  [1]: node1
  [2]: node2
  [3]: node3
  [4]: node4
  [5]: node5
  [6]: node6
  [7]: node7
  [8]: node8
  [9]: node9
  [10]: node10
  [11]: node11
  [12]: node12
  [13]: node13
  [14]: node14
  [15]: node15
  [16]: node16
  [17]: node17
  [18]: node18
  
  ")
      
    ) 
    
    insertJS <- function(plot){
      javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'502\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Identification</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'257\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'38\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Included</text>";
                              ')
      htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
    }
    
    x <- insertJS(x)
    
    if (interactive == TRUE) {
      x <- sr_flow_interactive_noprev(x, urls)
    }
    
    }
  
  
  # Produce plot WITH previous studies
  else if (previous == TRUE){
  x <- DiagrammeR::grViz(
    paste0("digraph TD {
  
  graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information']
  
  # node statements
  node [shape = box]
  identification [image='identification.png', color = LightSteelBlue2, label='', style = 'filled,rounded', pos='-1.5,7.93!', width = 0.4, height = 2.6, tooltip = '", tooltips[20], "'];
  screening [image='screening.png', color = LightSteelBlue2, label='', style = 'filled,rounded', pos='-1.5,4.5!', width = 0.4, height = 3.5, tooltip = '", tooltips[21], "'];
  included [image='included.png', color = LightSteelBlue2, label='', style = 'filled,rounded', pos='-1.5,0.87!', width = 0.4, height = 2.5, tooltip = '", tooltips[22], "'];
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  1 [label = '@@1', style = 'rounded,filled', width = 3, height = 0.5, pos='0.5,9!', tooltip = '", tooltips[1], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  2 [label = '@@2', style = 'filled', width = 3, height = 0.5, pos='0.5,7.5!', tooltip = '", tooltips[2], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", title_colour, "]
  3 [label = '@@3', style = 'rounded,filled', width = 7, height = 0.5, pos='6,9!', tooltip = '", tooltips[3], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  4 [label = '@@4', width = 3, width = 3, height = 0.5, height = 0.5, pos='4,7.5!', tooltip = '", tooltips[4], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  5 [label = '@@5', width = 3, height = 0.5, pos='8,7.5!', tooltip = '", tooltips[7], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  6 [label = '@@6', width = 3, width = 3, height = 0.5, height = 0.5, pos='4,5.5!', tooltip = '", tooltips[8], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  7 [label = '@@7', width = 3, height = 0.5, pos='8,5.5!', tooltip = '", tooltips[9], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  8 [label = '@@8', width = 3, width = 3, height = 0.5, height = 0.5, pos='4,4.5!', tooltip = '", tooltips[10], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  9 [label = '@@9', width = 3, height = 0.5, pos='8,4.5!', tooltip = '", tooltips[11], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  10 [label = '@@10', width = 3, height = 0.5, pos='4,3.5!', tooltip = '", tooltips[14], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  11 [label = '@@11', width = 3, height = 0.5, pos='8,3.5!', tooltip = '", tooltips[15], "']
    
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, ", pos='6,4.5!', tooltip = '", tooltips[18], "']
  12 [label = '@@12', pos='4,1.5!']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  13 [label = '@@13', style = 'rounded,filled', width = 7, height = 0.5, pos='13.5,9!', tooltip = '", tooltips[5], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  14 [label = '@@14', style = 'filled', width = 3, height = 0.5, pos='11.5,7.5!', tooltip = '", tooltips[6], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  15 [label = '@@15', style = 'filled', width = 3, height = 0.5, pos='11.5,4.5!', tooltip = '", tooltips[12], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  16 [label = '@@16', style = 'filled', width = 3, height = 0.5, pos='15.5,4.5!', tooltip = '", tooltips[13], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  17 [label = '@@17', style = 'filled', width = 3, height = 0.5, pos='11.5,3.5!', tooltip = '", tooltips[16], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  18 [label = '@@18', style = 'filled', width = 3, height = 0.5, pos='15.5,3.5!', tooltip = '", tooltips[17], "']

  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  19 [label = '@@19', style = 'filled', width = 3, height = 0.5, pos='4,0!', tooltip = '", tooltips[19], "']
  
  node [shape = square, width = 0, color=White]
  A [label = '', pos='0.5,0!', tooltip = '']
  B [label = '', pos='11.5,1.5!', tooltip = '']
  
  subgraph cluster0 {
    edge [color = White, 
        arrowhead = none, 
        arrowtail = none]
    1->2;
    edge [color = ", arrow_colour, ", 
        arrowhead = none, 
        arrowtail = ", arrow_tail, "]
    2->A; 
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = none,
        constraint = FALSE]
    A->19;
  }
  
  subgraph cluster1 {
    edge [style = invis]
    3->4; 3->5;
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = ", arrow_tail, ", 
        style = filled]
    4->5;
    4->6; 6->7;
    6->8; 8->9;
    8->10; 10->11;
    10->12;
    edge [style = invis]
    5->7;
    7->9;
    9->11;
    16->18;
  }
  
  subgraph cluster2 {
    edge [color = White, 
          arrowhead = none, 
          arrowtail = none]
    13->14;
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = ", arrow_tail, "]
    14->15; 15->16;
    15->17; 17->18;
    edge [color = ", arrow_colour, ", 
        arrowhead = none, 
        arrowtail = ", arrow_tail, "]
    17->B; 
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = none,
        constraint = FALSE]
    B->12;
  }
  
  edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = ", arrow_tail, "]

  12->19;
  
  {rank = same; A; 19}
  {rank = same; 1; 3; 13}
  {rank = same; 2; 4; 5; 14}
  {rank = same; 6; 7}
  {rank = same; 8; 9; 15; 16}
  {rank = same; 10; 11; 17; 18}
  {rank = same; 12; B}
  
  }
  
  [1]: node1
  [2]: node2
  [3]: node3
  [4]: node4
  [5]: node5
  [6]: node6
  [7]: node7
  [8]: node8
  [9]: node9
  [10]: node10
  [11]: node11
  [12]: node12
  [13]: node13
  [14]: node14
  [15]: node15
  [16]: node16
  [17]: node17
  [18]: node18
  [19]: node19
  
  ")
    
  )
  
  
  insertJS <- function(plot){
    javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'610\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Identification</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'365\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'105\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Included</text>";
                              ')
    htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
  }
  
  x <- insertJS(x)
  
  if (interactive == TRUE) {
    x <- sr_flow_interactive(x, urls)
  }
  
  }
  
  return(x)
}


#' Read in PRISMA flow chart data
#' 
#' @description Read in a template CSV containing data for the flow chart.
#' @param data File to read in.
#' @return A list of objects needed to plot the flow chart.
#' @examples 
#' \dontrun{
#' data <- read.csv(file.choose());
#' data <- read_PRISMAdata(data);
#' attach(data);
#' }
#' @export
read_PRISMAdata <- function(data){
  
  #Set parameters
  previous_studies <- data[grep('previous_studies', data[,1]),]$k
  previous_reports <- data[grep('previous_reports', data[,1]),]$k
  register_results <- data[grep('register_results', data[,1]),]$k
  database_results <- data[grep('database_results', data[,1]),]$k
  website_results <- data[grep('website_results', data[,1]),]$k
  organisation_results <- data[grep('organisation_results', data[,1]),]$k
  citations_results <- data[grep('citations_results', data[,1]),]$k
  duplicates <- data[grep('duplicates', data[,1]),]$k
  excluded_automatic <- data[grep('excluded_automatic', data[,1]),]$k
  excluded_other <- data[grep('excluded_other', data[,1]),]$k
  records_screened <- data[grep('records_screened', data[,1]),]$k
  records_excluded <- data[grep('records_excluded', data[,1]),]$k
  dbr_sought_reports <- data[grep('dbr_sought_reports', data[,1]),]$k
  dbr_notretrieved_reports <- data[grep('dbr_notretrieved_reports', data[,1]),]$k
  other_sought_reports <- data[grep('other_sought_reports', data[,1]),]$k
  other_notretrieved_reports <- data[grep('other_notretrieved_reports', data[,1]),]$k
  dbr_assessed <- data[grep('dbr_assessed', data[,1]),]$k
  dbr_excluded <- data.frame(reason = gsub(",.*$", "", unlist(strsplit(data[grep('dbr_excluded', data[,1]),]$k, split = '; '))), 
                             k = gsub(".*,", "", unlist(strsplit(data[grep('dbr_excluded', data[,1]),]$k, split = '; '))))
  other_assessed <- data[grep('other_assessed', data[,1]),]$k
  other_excluded <- data.frame(reason = gsub(",.*$", "", unlist(strsplit(data[grep('other_excluded', data[,1]),]$k, split = '; '))), 
                               k = gsub(".*,", "", unlist(strsplit(data[grep('other_excluded', data[,1]),]$k, split = '; '))))
  new_studies <- data[grep('new_studies', data[,1]),]$k
  new_reports <- data[grep('new_reports', data[,1]),]$k
  total_studies <- data[grep('total_studies', data[,1]),]$k
  total_reports <- data[grep('total_reports', data[,1]),]$k
  tooltips <- stats::na.omit(data$tooltips)
  urls <- data.frame(box = data[!duplicated(data$box), ]$box, url = data[!duplicated(data$box), ]$url)
  
  x <- list(previous_studies = previous_studies,
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
           tooltips = tooltips,
           urls = urls)
  
  return(x)
  
}


#' Insert rotated text to side panel nodes
#' 
#' @description Text cannot be rotated within DiagrammeR, so this function appends 
#' a Javascript function to add in rotated text after the id="node" 'g' tag that 
#' corresponds to the blue nodes at the side of the plot.
#' @param plot A PRISMA_flowchart plot.
#' @return A plot object containing javascript code.
#' @examples 
#' \dontrun{
#' plot <- insertJS(plot);
#' plot;
#' }
#' @export
insertJS <- function(plot){
  javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'610\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Identification</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'365\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'105\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Included</text>";
                              ')
  htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
}


#' Plot interactive flow charts for systematic reviews
#' 
#' @description Converts a PRISMA systematic review flow chart into an 
#' interactive HTML plot, for embedding links from each box.
#' @param plot A plot object from sr_flow().
#' @param urls A dataframe consisting of two columns: nodes and urls. The first
#' column should contain 19 rows for the nodes from node1 to node19. The second 
#' column should contain a corresponding URL for each node.
#' @return An interactive flow diagram plot.
#' @examples 
#' \dontrun{
#' urls <- data.frame(
#'     box = c('box1', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 'box8', 'box9', 'box10', 'box11', 'box12', 'box13', 'box14', 'box15', 'box16'), 
#'     url = c('page1.html', 'page2.html', 'page3.html', 'page4.html', 'page5.html', 'page6.html', 'page7.html', 'page8.html', 'page9.html', 'page10.html', 
#'             'page11.html', 'page12.html', 'page13.html', 'page14.html', 'page15.html', 'page16.html'));
#' output <- sr_flow_interactive(x, urls);
#' output
#' }
#' @export
sr_flow_interactive <- function(plot, 
                                urls) {
  
  link <- data.frame(boxname = c('identification', 'screening', 'included', 'prevstud', 'box1', 'newstud', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 
                                 'box8', 'box9', 'box10', 'othstud', 'box11', 'box12', 'box13', 'box14', 'box15', 'box16', 'A', 'B'), 
                     node = paste0('node', seq(1, 24)))
  
  link <- merge(link, urls, by.x = 'boxname', by.y = 'box', all.x = TRUE)
  #link$order <- readr::parse_number(link$node)
  #link <- link[order(link$order),]
  target <- c('node1', 'node2', 'node3', 'node4', 'node5', 'node23', 'node6', 'node7', 'node8', 'node9', 'node10', 'node11', 'node12', 'node13', 'node14', 'node15', 'node22', 'node16', 'node17', 'node18', 'node19', 'node20', 'node21', 'node24')
  link <- link[match(target, link$node),]
  node <- link$node
  url <- link$url
  
  #the following function produces three lines of JavaScript per node to add a specified hyperlink for the node, pulled in from nodes.csv
  myfun <- function(node, 
                    url){
    t <- paste0('const ', node, ' = document.getElementById("', node, '");
  var link', node, ' = "<a href=\'', url, '\' target=\'_blank\'>" + ', node, '.innerHTML + "</a>";
  ', node, '.innerHTML = link', node, ';
  ')
  }
  #the following code adds the location link for the new window
  javascript <- htmltools::HTML(paste(mapply(myfun, 
                                             node, 
                                             url), 
                                      collapse = '\n'))  
  htmlwidgets::prependContent(plot, 
                              htmlwidgets::onStaticRenderComplete(javascript))
  
}


#' Plot interactive flow charts for systematic reviews
#' 
#' @description Converts a PRISMA systematic review flow chart into an 
#' interactive HTML plot, for embedding links from each box.
#' @param plot A plot object from sr_flow().
#' @param urls A dataframe consisting of two columns: nodes and urls. The first
#' column should contain 19 rows for the nodes from node1 to node19. The second 
#' column should contain a corresponding URL for each node.
#' @return An interactive flow diagram plot.
#' @examples 
#' \dontrun{
#' urls <- data.frame(
#'     box = c('identification', 'screening', 'included', 'newstud', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 'box8', 'box9', 'box10', 'box11', 'box12', 'othstud', 'box13', 'box14', 'box15'), 
#'     url = c('identification.html', 'screening.html', 'included.html', 'newstudies.html', 'page2.html', 'page3.html', 'page4.html', 'page5.html', 'page6.html', 'page7.html', 'page8.html', 'page9.html', 'page10.html', 
#'             'page11.html', 'page12.html', 'otherstud.html', 'page13.html', 'page14.html', 'page15.html'));
#' output <- sr_flow_interactive_noprev(x, urls);
#' output
#' }
#' @export
sr_flow_interactive_noprev <- function(plot, 
                                urls) {
  
  link <- data.frame(boxname = c('identification', 'screening', 'included', 'newstud', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 
                                 'box8', 'box9', 'box10', 'othstud', 'box11', 'box12', 'box13', 'box14', 'box15', 'B'), 
                     node = paste0('node', seq(1, 20)))
  
  link <- merge(link, urls, by.x = 'boxname', by.y = 'box', all.x = TRUE)
  #link$order <- readr::parse_number(link$node)
  #link <- link[order(link$order),]
  target <- c('node1', 'node2', 'node3', 'node4', 'node5', 'node6', 'node7', 'node8', 'node9', 'node10', 'node11', 'node12', 'node13', 'node14', 'node15', 'node16', 'node17', 'node18', 'node19', 'node20')
  link <- link[match(target, link$node),]
  node <- link$node
  url <- link$url
  
  #the following function produces three lines of JavaScript per node to add a specified hyperlink for the node, pulled in from nodes.csv
  myfun <- function(node, 
                    url){
    t <- paste0('const ', node, ' = document.getElementById("', node, '");
  var link', node, ' = "<a href=\'', url, '\' target=\'_blank\'>" + ', node, '.innerHTML + "</a>";
  ', node, '.innerHTML = link', node, ';
  ')
  }
  #the following code adds the location link for the new window
  javascript <- htmltools::HTML(paste(mapply(myfun, 
                                             node, 
                                             url), 
                                      collapse = '\n'))  
  htmlwidgets::prependContent(plot, 
                              htmlwidgets::onStaticRenderComplete(javascript))
  
}
