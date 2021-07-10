#' Plot interactive flow diagrams for systematic reviews
#' 
#' 
#' @description Produces a PRISMA2020 style flow diagram for systematic reviews, 
#' with the option to add interactivity through tooltips (mouseover popups) and 
#' hyperlink URLs to each box. Data can be imported from the standard CSV template 
#' provided.
#' @param data List of data inputs including numbers of studies, box text, tooltips 
#' and urls for hyperlinks. Data inputted via the [PRISMA_data()] function. If 
#' inputting individually, see the necessary parameters listed in the 
#' [PRISMA_data()]) function and combine them in a list using `data <- list()`.
#' @param interactive Logical argument TRUE or FALSE whether to plot interactivity 
#' (tooltips and hyperlinked boxes).
#' @param previous Logical argument (TRUE or FALSE) specifying whether previous 
#' studies were sought.
#' @param other Logical argument (TRUE or FALSE) specifying whether other studies 
#' were sought.
#' @param font The font for text in each box. The default is 'Helvetica'.
#' @param fontsize The font size for text in each box. The default is '12'.
#' @param title_colour The colour for the upper middle title box (new studies). 
#' The default is 'Goldenrod1'. See 'DiagrammeR' colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param greybox_colour The colour for the left and right column boxes. The 
#' default is 'Gainsboro'. See 'DiagrammeR' colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param main_colour The colour for the main box borders. The default is 
#' 'Black'. See 'DiagrammeR' colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param arrow_colour The colour for the connecting lines. The default
#' is 'Black'. See 'DiagrammeR' colour scheme 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors>.
#' @param arrow_head The head shape for the line connectors. The default is 
#' 'normal'. See DiagrammeR arrow shape specification 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#arrow-shapes>.
#' @param arrow_tail The tail shape for the line connectors. The default is 
#' 'none'. See DiagrammeR arrow shape specification 
#' <http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#arrow-shapes>.
#' @param side_boxes Whether or not to include the blue label boxes along the side
#' @return A flow diagram plot.
#' @examples
#' csvFile <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
#' data <- read.csv(csvFile);
#' data <- PRISMA_data(data);
#' plot <- PRISMA_flowdiagram(data,
#'                 fontsize = 12,
#'                 interactive = TRUE,
#'                 previous = FALSE,
#'                 other = TRUE);
#' plot
#' @export
PRISMA_flowdiagram <- function (data,
                                interactive = FALSE,
                                previous = TRUE,
                                other = TRUE,
                                fontsize = 7,
                                font = 'Helvetica',
                                title_colour = 'Goldenrod1',
                                greybox_colour = 'Gainsboro',
                                main_colour = 'Black',
                                arrow_colour = 'Black',
                                arrow_head = 'normal',
                                arrow_tail = 'none',
                                side_boxes = TRUE) {
  # removes the need to attach() the data first https://stackoverflow.com/a/11505637
  for(var in seq_len(length(data))) {
    assign(names(data)[var], data[[var]])
  }
  #wrap exclusion reasons
  dbr_excluded[,1] <- stringr::str_wrap(dbr_excluded[,1], 
                    width = 35)
  other_excluded[,1] <- stringr::str_wrap(other_excluded[,1], 
                    width = 35)
  if(stringr::str_count(paste(dbr_excluded[,1], collapse = "\n"), "\n") > 3){
    dbr_excludedh <- 3.5 - ((stringr::str_count(paste(dbr_excluded[,1], collapse = "\n"), "\n")-4)/9)
  } else {
    dbr_excludedh <- 3.5
  }
  if(nrow(other_excluded) > 3){
    other_excludedh <- 3.5 - ((nrow(other_excluded)-4)/9)
  } else {
    other_excludedh <- 3.5
  }
  #remove previous box if both values are zero
  if (is.na(previous_studies) == TRUE && is.na(previous_reports) == TRUE) {
    previous <- FALSE
  }

  if(previous == TRUE){
    xstart <- 0
    ystart <- 0
    A <- paste0("A [label = '', pos='",xstart+1,",",ystart+0,"!', tooltip = '']")
    Aedge <- paste0("subgraph cluster0 {
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
                }")
    bottomedge <- paste0("edge [color = ", arrow_colour, ", 
  arrowhead = ", arrow_head, ", 
  arrowtail = ", arrow_tail, "]
              12->19;\n")
    h_adj1 <- 0
    h_adj2 <- 0

    #conditional studies and reports - empty text if blank
    if(is.na(previous_studies) == TRUE) {
      cond_prevstud <- ''
    } else {
      cond_prevstud <- stringr::str_wrap(paste0(previous_studies_text,
                                                " (n = ",
                                                previous_studies, 
                                                ")"), 
                                         width = 40)
    }
    if(is.na(previous_reports) == TRUE) {
      cond_prevrep <- ''
    } else {
      cond_prevrep <- paste0(stringr::str_wrap(previous_reports_text, 
                                               width = 40),
                             "\n(n = ",
                             previous_reports,
                             ')')
    }
    if (is.na(previous_studies) == TRUE || is.na(previous_reports) == TRUE) {
      dbl_br <- ''
    } else {
      dbl_br <- "\n\n"
    }
previous_nodes <- paste0("node [shape = box,
          fontsize = ", fontsize,",
          fontname = ", font, ",
          color = ", greybox_colour, "]
    1 [label = '", previous_text, "', style = 'rounded,filled', width = 3.5, height = 0.5, pos='",xstart+1,",",ystart+8.25,"!', tooltip = '", tooltips[1], "']
    
    node [shape = box,
          fontname = ", font, ",
          color = ", greybox_colour, "]
    2 [label = '",paste0(cond_prevstud,
                         dbl_br,
                         cond_prevrep), 
                         "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+1,",",ystart+7,"!', tooltip = '", tooltips[2], "']")
    finalnode <- paste0("
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  19 [label = '",paste0(stringr::str_wrap(paste0(total_studies_text,
                                                 " (n = ",
                                                 total_studies, 
                                                 ")"), 
                                          width = 33),
                        "\n",
                        stringr::str_wrap(paste0(total_reports_text,
                                                 " (n = ",
                                                 total_reports,
                                                 ')'), 
                                          width = 33)),  
                        "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+5,",",ystart+0,"!', tooltip = '", tooltips[19], "']")
    prev_rank1 <- "{rank = same; A; 19}"
    prevnode1 <- "1; "
    prevnode2 <- "2; "
    
  } else {
    xstart <- -3.5
    ystart <- 0
    A <- ""
    Aedge <- ""
    bottomedge <- ""
    previous_nodes <- ""
    finalnode <- ""
    h_adj1 <- 0.63
    h_adj2 <- 1.4
    prev_rank1 <- ""
    prevnode1 <- ""
    prevnode2 <- ""  
  }
  if(side_boxes == TRUE){
    sidebox <- paste0("node [shape = box,
        fontsize = ", fontsize,",
        fontname = ", font, ",
        color = ", title_colour, "
        ]
  identification [color = LightSteelBlue2, label=' ', style = 'filled,rounded', pos='",-1.4,",",ystart+7,"!', width = 0.4, height = 1.5, tooltip = '", tooltips[20], "'];
  screening [color = LightSteelBlue2, label=' ', style = 'filled,rounded', pos='",-1.4,",",ystart+4.5,"!', width = 0.4, height = 2.5, tooltip = '", tooltips[21], "'];
  included [color = LightSteelBlue2, label=' ', style = 'filled,rounded', pos='",-1.4,",",h_adj1+0.87,"!', width = 0.4, height = ",2.5-h_adj2,", tooltip = '", tooltips[22], "'];\n
  ")
  } else {
    sidebox <- ""
  }
  if (is.na(website_results) == TRUE && is.na(organisation_results) == TRUE && is.na(citations_results) == TRUE) {
    other <- FALSE
  }
  if(other == TRUE){
    if (is.data.frame(other_excluded) == TRUE){
      other_excluded_data <- paste0(':',
                                    paste(paste('\n', 
                                         other_excluded[,1], 
                                         ' (n = ', 
                                         other_excluded[,2], 
                                         ')', 
                                         sep = ''), 
                                   collapse = ''))
    } else {
      other_excluded_data <- paste0('\n', '(n = ', other_excluded, ')')
    }
    B <- paste0("B [label = '', pos='",xstart+13,",",ystart+1.5,"!', tooltip = '']")

     if (is.na(website_results) == FALSE) {
      cond_websites <- paste0(website_results_text,
                              " (n = ",
                              website_results,
                              ')\n')
    } else {
      cond_websites <- ''
    }
    if (is.na(organisation_results) == FALSE) {
      cond_organisation <- paste0(organisation_results_text,
                                  " (n = ",
                                  organisation_results,
                                  ')\n')
    } else {
      cond_organisation <- ''
    }
    if (is.na(citations_results) == FALSE) {
      cond_citation <- paste0(citations_results_text,
                              " (n = ",
                              citations_results,
                              ')')
    } else {
      cond_citation <- ''
    }
    cluster2 <- paste0("subgraph cluster2 {
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
  }")
    othernodes <- paste0("node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  13 [label = '", other_text, "', style = 'rounded,filled', width = 7.5, height = 0.5, pos='",xstart+15,",",ystart+8.25,"!', tooltip = '", tooltips[5], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  14 [label = '", paste0('Records identified from:\n',
                         cond_websites,
                         cond_organisation,
                         cond_citation),
                         "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+13,",",ystart+7,"!', tooltip = '", tooltips[6], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  15 [label = '", paste0(other_sought_reports_text,
                         '\n(n = ',
                         other_sought_reports,
                         ')'), "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+13,",",ystart+4.5,"!', tooltip = '", tooltips[12], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  16 [label = '", paste0(other_notretrieved_reports_text,'\n(n = ',
                         other_notretrieved_reports,
                         ')'), "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+17,",",ystart+4.5,"!', tooltip = '", tooltips[13], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  17 [label = '", paste0(other_assessed_text,
                         '\n(n = ',
                         other_assessed,
                         ')'),"', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+13,",",ystart+3.5,"!', tooltip = '", tooltips[16], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  18 [label = '", paste0(other_excluded_text,
                         other_excluded_data), "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+17,",",ystart+other_excludedh,"!', tooltip = '", tooltips[17], "']\n
                       ")
    extraedges <- "16->18;"
    othernode13 <- "; 13"
    othernode14 <- "; 14"
    othernode1516 <- "; 15; 16"
    othernode1718 <- "; 17; 18"
    othernodeB <- "; B"
    
  } else {
    B <- ""
    cluster2 <- ""
    othernodes <- ""
    extraedges <- ""
    optnodesother <- ""
    othernode13 <- ""
    othernode14 <- ""
    othernode1516 <- ""
    othernode1718 <- ""
    othernodeB <- ""
    
  }
  if (any(!grepl("\\D", dbr_excluded)) == FALSE){
    dbr_excluded_data <- paste0(':',
                                paste(paste('\n', 
                                            dbr_excluded[,1], 
                                            ' (n = ', 
                                            dbr_excluded[,2], 
                                            ')', 
                                            sep = ''), 
                                      collapse = ''))
  } else {
    dbr_excluded_data <- paste0('\n', '(n = ', dbr_excluded, ')')
  }
  
  if (is.na(database_results) == FALSE) {
    cond_database <- paste0(database_results_text, 
                            ' (n = ',
                            database_results,
                            ')\n')
  } else {
    cond_database <- ''
  }
  if (is.na(register_results) == FALSE) {
    cond_register <- paste0(register_results_text, 
                            ' (n = ',
                            register_results,
                            ')')
  } else {
    cond_register <- ''
  }
  
  if (is.na(duplicates) == FALSE) {
    cond_duplicates <- paste0(stringr::str_wrap(paste0(duplicates_text,
                                                       ' (n = ',
                                                       duplicates,
                                                       ')'),
                                                width = 42),
                              '\n')
  } else {
    cond_duplicates <- ''
  }
  if (is.na(excluded_automatic) == FALSE) {
    cond_automatic <- paste0(stringr::str_wrap(paste0(excluded_automatic_text,
                                                       ' (n = ',
                                                       excluded_automatic,
                                                       ')'),
                                                width = 42),
                              '\n')
  } else {
    cond_automatic <- ''
  }
  if (is.na(excluded_other) == FALSE) {
    cond_exclother <- paste0(stringr::str_wrap(paste0(excluded_other_text, 
                                                      ' (n = ',
                                                      excluded_other,
                                                      ')'),
                                               width = 42))
  } else {
    cond_exclother <- ''
  }
  if (is.na(duplicates) == TRUE && is.na(excluded_automatic) == TRUE && is.na(excluded_other) == TRUE) {
    cond_duplicates <- paste0('(n = 0)')
  }
  
  if(is.na(new_studies) == FALSE) {
    cond_newstud <- paste0(stringr::str_wrap(new_studies_text, width = 40),
                           '\n(n = ',
                           new_studies,
                           ')\n')
  } else {
    cond_newstud <- ''
  }
  if(is.na(new_reports) == FALSE) {
    cond_newreports <- paste0(stringr::str_wrap(new_reports_text, width = 40),
                           '\n(n = ',
                           new_reports,
                           ')')
  } else {
    cond_newreports <- ''
  }
  x <- DiagrammeR::grViz(
    paste0("digraph TD {
  
  graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information', outputorder=edgesfirst]",
           sidebox,
           previous_nodes,"
  node [shape = box,
        fontsize = ", fontsize,",
        fontname = ", font, ",
        color = ", title_colour, "]
  3 [label = '", newstud_text, "', style = 'rounded,filled', width = 7.5, height = 0.5, pos='",xstart+7,",",ystart+8.25,"!', tooltip = '", tooltips[3], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
 4 [label = '", paste0('Records identified from:\n', 
                        cond_database, 
                        cond_register), "', width = 3.5, height = 0.5, height = 0.5, pos='",xstart+5,",",ystart+7,"!', tooltip = '", tooltips[4], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  5 [label = '", paste0('Records removed before screening:\n', 
                        cond_duplicates,
                        cond_automatic, 
                        cond_exclother),
           "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+7,"!', tooltip = '", tooltips[7], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
6 [label = '", paste0(records_screened_text,
                        '\n(n = ',
                        records_screened,
                        ')'), "', width = 3.5, height = 0.5, height = 0.5, pos='",xstart+5,",",ystart+5.5,"!', tooltip = '", tooltips[8], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
 7 [label = '", paste0(records_excluded_text,
                        '\n(n = ',
                        records_excluded,
                        ')'), "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+5.5,"!', tooltip = '", tooltips[9], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  8 [label = '", paste0(dbr_sought_reports_text,
                        '\n(n = ',
                        dbr_sought_reports,
                        ')'), "', width = 3.5, height = 0.5, height = 0.5, pos='",xstart+5,",",ystart+4.5,"!', tooltip = '", tooltips[10], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
9 [label = '", paste0(dbr_notretrieved_reports_text,
                        '\n(n = ',
                        dbr_notretrieved_reports,
                        ')'), "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+4.5,"!', tooltip = '", tooltips[11], "']

  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
10 [label = '", paste0(dbr_assessed_text,
                         '\n(n = ',
                         dbr_assessed,
                         ')'), "', width = 3.5, height = 0.5, pos='",xstart+5,",",ystart+3.5,"!', tooltip = '", tooltips[14], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, ", 
        fillcolor = White,
        style = filled]
  11 [label = '", paste0(dbr_excluded_text,
                         dbr_excluded_data), "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+dbr_excludedh,"!', tooltip = '", tooltips[15], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, ", fillcolor = '', style = solid]
  12 [label = '", paste0(cond_newstud,
                         cond_newreports), "', width = 3.5, height = 0.5, pos='",xstart+5,",",ystart+1.5,"!', tooltip = '", tooltips[18], "']
  
  ",othernodes,
           
           finalnode,"
  
  node [shape = square, width = 0, color=White]\n",
           A,"
  ",B,"
  
  ",
           Aedge,"
  
  node [shape = square, width = 0, style=invis]
  C [label = '', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+3.5,"!', tooltip = '']
  
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
    8->10; 10->C;
    10->12;
    edge [style = invis]
    5->7;
    7->9;
    9->11;
    ",extraedges,"
  }
  
  ",cluster2,"
  
  ",
           bottomedge,"\n\n",
           prev_rank1,"\n",
           "{rank = same; ",prevnode1,"3",othernode13,"} 
  {rank = same; ",prevnode2,"4; 5",othernode14,"} 
  {rank = same; 6; 7} 
  {rank = same; 8; 9",othernode1516,"} 
  {rank = same; 10; 11",othernode1718,"} 
  {rank = same; 12",othernodeB,"} 
  
  }
  ")
  )
  if (side_boxes == TRUE) {
    x <- PRISMA_insert_js_(x, identification_text = identification_text,screening_text = screening_text,included_text = included_text)
  }
  
  if (interactive == TRUE) {
    x <- PRISMA_interactive_(x, urls, previous = previous, other = other)
  }
  return(x)
}


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
PRISMA_data <- function(data){
  
  #Set parameters
  previous_studies <- scales::comma(as.numeric(data[grep('previous_studies', data[,1]),]$n))
  previous_reports <- scales::comma(as.numeric(data[grep('previous_reports', data[,1]),]$n))
  register_results <- scales::comma(as.numeric(data[grep('register_results', data[,1]),]$n))
  database_results <- scales::comma(as.numeric(data[grep('database_results', data[,1]),]$n))
  website_results <- scales::comma(as.numeric(data[grep('website_results', data[,1]),]$n))
  organisation_results <- scales::comma(as.numeric(data[grep('organisation_results', data[,1]),]$n))
  citations_results <- scales::comma(as.numeric(data[grep('citations_results', data[,1]),]$n))
  duplicates <- scales::comma(as.numeric(data[grep('duplicates', data[,1]),]$n))
  excluded_automatic <- scales::comma(as.numeric(data[grep('excluded_automatic', data[,1]),]$n))
  excluded_other <- scales::comma(as.numeric(data[grep('excluded_other', data[,1]),]$n))
  records_screened <- scales::comma(as.numeric(data[grep('records_screened', data[,1]),]$n))
  records_excluded <- scales::comma(as.numeric(data[grep('records_excluded', data[,1]),]$n))
  dbr_sought_reports <- scales::comma(as.numeric(data[grep('dbr_sought_reports', data[,1]),]$n))
  dbr_notretrieved_reports <- scales::comma(as.numeric(data[grep('dbr_notretrieved_reports', data[,1]),]$n))
  other_sought_reports <- scales::comma(as.numeric(data[grep('other_sought_reports', data[,1]),]$n))
  other_notretrieved_reports <- scales::comma(as.numeric(data[grep('other_notretrieved_reports', data[,1]),]$n))
  dbr_assessed <- scales::comma(as.numeric(data[grep('dbr_assessed', data[,1]),]$n))
  dbr_excluded <- data.frame(reason = gsub(",.*$", "", unlist(strsplit(as.character(data[grep('dbr_excluded', data[,1]),]$n), split = '; '))), 
                             n = gsub(".*,", "", unlist(strsplit(as.character(data[grep('dbr_excluded', data[,1]),]$n), split = '; '))))
  other_assessed <- scales::comma(as.numeric(data[grep('other_assessed', data[,1]),]$n))
  other_excluded <- data.frame(reason = gsub(",.*$", "", unlist(strsplit(as.character(data[grep('other_excluded', data[,1]),]$n), split = '; '))), 
                               n = gsub(".*,", "", unlist(strsplit(as.character(data[grep('other_excluded', data[,1]),]$n), split = '; '))))
  new_studies <- scales::comma(as.numeric(data[grep('new_studies', data[,1]),]$n))
  new_reports <- scales::comma(as.numeric(data[grep('new_reports', data[,1]),]$n))
  total_studies <- scales::comma(as.numeric(data[grep('total_studies', data[,1]),]$n))
  total_reports <- scales::comma(as.numeric(data[grep('total_reports', data[,1]),]$n))
  tooltips <- stats::na.omit(data$tooltips)
  urls <- data.frame(box = data[!duplicated(data$box), ]$box, url = data[!duplicated(data$box), ]$url)
  
  #set text - if text >33 characters, 
  previous_text <- data[grep('prevstud', data[,3]),]$boxtext
  newstud_text <- data[grep('newstud', data[,3]),]$boxtext
  other_text <- data[grep('othstud', data[,3]),]$boxtext
  previous_studies_text <- data[grep('previous_studies', data[,1]),]$boxtext
  previous_reports_text <- data[grep('previous_reports', data[,1]),]$boxtext
  register_results_text <- data[grep('register_results', data[,1]),]$boxtext
  database_results_text <- data[grep('database_results', data[,1]),]$boxtext
  website_results_text <- data[grep('website_results', data[,1]),]$boxtext
  organisation_results_text <- data[grep('organisation_results', data[,1]),]$boxtext
  citations_results_text <- data[grep('citations_results', data[,1]),]$boxtext
  duplicates_text <- data[grep('duplicates', data[,1]),]$boxtext
  excluded_automatic_text <- data[grep('excluded_automatic', data[,1]),]$boxtext
  excluded_other_text <- data[grep('excluded_other', data[,1]),]$boxtext
  records_screened_text <- data[grep('records_screened', data[,1]),]$boxtext
  records_excluded_text <- data[grep('records_excluded', data[,1]),]$boxtext
  dbr_sought_reports_text <- data[grep('dbr_sought_reports', data[,1]),]$boxtext
  dbr_notretrieved_reports_text <- data[grep('dbr_notretrieved_reports', data[,1]),]$boxtext
  other_sought_reports_text <- data[grep('other_sought_reports', data[,1]),]$boxtext
  other_notretrieved_reports_text <- data[grep('other_notretrieved_reports', data[,1]),]$boxtext
  dbr_assessed_text <- data[grep('dbr_assessed', data[,1]),]$boxtext
  dbr_excluded_text <- data[grep('dbr_excluded', data[,1]),]$boxtext
  other_assessed_text <- data[grep('other_assessed', data[,1]),]$boxtext
  other_excluded_text <- data[grep('other_excluded', data[,1]),]$boxtext
  new_studies_text <- data[grep('new_studies', data[,1]),]$boxtext
  new_reports_text <- data[grep('new_reports', data[,1]),]$boxtext
  total_studies_text <- data[grep('total_studies', data[,1]),]$boxtext
  total_reports_text <- data[grep('total_reports', data[,1]),]$boxtext
  identification_text <- data[grep('identification', data[,1]),]$boxtext
  screening_text <- data[grep('screening', data[,1]),]$boxtext
  included_text <- data[grep('included', data[,1]),]$boxtext
  
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
           identification_text = identification_text,
           screening_text = screening_text,
           included_text = included_text,
           tooltips = tooltips,
           urls = urls)
  
  return(x)
  
}

#' Save PRISMA2020 flow diagram
#' 
#' @description Save the output from [PRISMA_flowdiagram()] to the 
#' working directory.
#' @param plotobj A plot produced using [PRISMA_flowdiagram()].
#' @param filename The filename to save (including extension)
#' @param filetype The filetype to save the plot in, supports: HTML, PDF, PNG, SVG, PS and WEBP
#' (if NA, the filetype will be calculated out based on the file extension)
#' HTML files maintain hyperlinks and tooltips
#' @param overwrite if TRUE, will overwrite an existing file
#' @return the absolute filename of the saved diagram plot.
#' @examples
#' csvFile <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
#' data <- read.csv(csvFile);
#' data <- PRISMA_data(data);
#' plot <- PRISMA_flowdiagram(data,
#'                 fontsize = 12,
#'                 interactive = TRUE,
#'                 previous = FALSE,
#'                 other = TRUE);
#' PRISMA_save(plot, filename = tempfile(), filetype="html");
#' @export
PRISMA_save <- function(plotobj, filename = 'PRISMA2020_flowdiagram.html', filetype = NA, overwrite = FALSE){
  if (!file.exists(filename) | overwrite == TRUE ) {
    format_real <- PRISMA_calc_filetype_(filename, filetype)
    switch(
      format_real,
      "HTML" = {
        tmp_html <- tempfile(pattern = "PRISMA2020_", tmpdir = tempdir(), fileext = ".html" )
        htmlwidgets::saveWidget(plotobj, file=tmp_html, title = tools::file_path_sans_ext(filename))
        if (!(file.copy(tmp_html, filename, overwrite = TRUE))){
          stop("Error saving HTML")
        }
        file.remove(tmp_html)
      },
      "PDF" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj)
        rsvg::rsvg_pdf(tmp_svg, filename)
        file.remove(tmp_svg)
      },
      "PNG" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj)
        rsvg::rsvg_png(tmp_svg, filename)
        file.remove(tmp_svg)
      },
      "SVG" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj)
        if (!(file.copy(tmp_svg, filename, overwrite = TRUE))){
          stop("Error saving SVG")
        }
        file.remove(tmp_svg)
      },
      "PS" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj)
        rsvg::rsvg_ps(tmp_svg, filename)
        file.remove(tmp_svg)
      },
      "WEBP" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj)
        rsvg::rsvg_webp(tmp_svg, filename)
        file.remove(tmp_svg)
      },
      stop("Please choose one of the supported file types")
    )
    return(tools::file_path_as_absolute(filename))
  } else {
    stop("File exists, please set overwite = TRUE to overwrite")
  }
}

#' Plot interactive flow diagram for systematic reviews - DEPRECATED
#' 
#' @description DEPRECATED - Converts a PRISMA systematic review flow diagram into an 
#' interactive HTML plot, for embedding links from each box.
#' @seealso [PRISMA_interactive_()]
#' @param plot A plot object from [PRISMA_flowdiagram()].
#' @param urls A dataframe consisting of two columns: nodes and urls. The first
#' column should contain 19 rows for the nodes from node1 to node19. The second 
#' column should contain a corresponding URL for each node.
#' @param previous Logical argument (TRUE or FALSE) (supplied through 
#' [PRISMA_flowdiagram()]) specifying whether previous studies were sought.
#' @param other Logical argument (TRUE or FALSE) (supplied through 
#' [PRISMA_flowdiagram()]) specifying whether other studies were sought.
#' @return An interactive flow diagram plot.
#' @export
sr_flow_interactive <- function(plot, 
                                urls,
                                previous,
                                other) {
  .Deprecated("PRISMA_interactive_")
  return(PRISMA_interactive_(plot, urls, previous, other))
}

#' Read in PRISMA flow diagram data - DEPRECATED
#' @description DEPRECATED - read in a template CSV containing data for the flow diagram
#' @seealso [PRISMA_data()]
#' @param data File to read in.
#' @return A list of objects needed to plot the flow diagram
#' @export
read_PRISMAdata <- function(data){
  .Deprecated("PRISMA_data")
  return(PRISMA_data(data))
}