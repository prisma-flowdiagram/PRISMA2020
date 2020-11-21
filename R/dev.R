previous_studies = previous_studies
previous_reports = previous_reports
register_results = register_results
database_results = database_results
website_results = website_results
organisation_results = organisation_results
citations_results = citations_results
duplicates = duplicates
excluded_automatic = excluded_automatic
excluded_other = excluded_other
records_screened = records_screened
records_excluded = records_excluded
dbr_sought_reports = dbr_sought_reports
dbr_notretrieved_reports = dbr_notretrieved_reports
other_sought_reports = other_sought_reports
other_notretrieved_reports = other_notretrieved_reports
dbr_assessed = dbr_assessed
dbr_excluded = dbr_excluded
other_assessed = other_assessed
other_excluded = other_excluded
new_studies = new_studies
new_reports = new_reports
total_studies = total_studies
total_reports = total_reports
interactive = TRUE
tooltips = tooltips
urls = urls
font = 'Helvetica'
title_colour = 'Goldenrod1'
greybox_colour = 'Gainsboro'
main_colour = 'Black'
arrow_colour = 'Black'
arrow_head = 'normal'
arrow_tail = 'none'




if(previous == TRUE){
  xstart <- 0
  ystart <- 0
  A <- paste0("A [label = '', pos='",xstart+0.5,",",ystart+0,"!', tooltip = '']")
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
  previous_nodes <- paste0("node [shape = box,
          fontname = ", font, ",
          color = ", greybox_colour, "]
    1 [label = 'Previous studies', style = 'rounded,filled', width = 3, height = 0.5, pos='",xstart+0.5,",",ystart+9,"!', tooltip = '", tooltips[1], "']
    
    node [shape = box,
          fontname = ", font, ",
          color = ", greybox_colour, "]
    2 [label = '",paste0('Studies included in\nprevious version of\nreview (n = ',
                         previous_studies, 
                         ')\n\nReports of studies\nincluded in previous\nversion of review (n = ',
                         previous_reports,
                         ')'), "', style = 'filled', width = 3, height = 0.5, pos='",xstart+0.5,",",ystart+7.5,"!', tooltip = '", tooltips[2], "']")
  finalnode <- paste0("
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  19 [label = '",paste0('Total studies included in review\n(n = ',
                        total_studies,
                        ')\n',
                        'Reports of total included studies\n(n = ',
                        total_reports,
                        ')'), "', style = 'filled', width = 3, height = 0.5, pos='",xstart+4,",",ystart+0,"!', tooltip = '", tooltips[19], "']")
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

if(other == TRUE){
  B <- paste0("B [label = '', pos='",xstart+11.5,",",ystart+1.5,"!', tooltip = '']")
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
  13 [label = 'Identification of new studies via other methods', style = 'rounded,filled', width = 7, height = 0.5, pos='",xstart+13.5,",",ystart+9,"!', tooltip = '", tooltips[5], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  14 [label = '", paste0('Records identified from:\n\tWebsites (n = ',
                        website_results,
                        ')\n\tOrganisations (n = ',
                        organisation_results,
                        ')\n\tCitation searching (n = ',
                        citations_results,
                        ')'), "', style = 'filled', width = 3, height = 0.5, pos='",xstart+11.5,",",ystart+7.5,"!', tooltip = '", tooltips[6], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  15 [label = '", paste0('Reports sought for retrieval\n(n = ',
                        other_sought_reports,
                        ')'), "', style = 'filled', width = 3, height = 0.5, pos='",xstart+11.5,",",ystart+4.5,"!', tooltip = '", tooltips[12], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  16 [label = '", paste0('Reports not retrieved\n(n = ',
                        other_notretrieved_reports,
                        ')'), "', style = 'filled', width = 3, height = 0.5, pos='",xstart+15.5,",",ystart+4.5,"!', tooltip = '", tooltips[13], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  17 [label = '", paste0('Reports assessed for eligibility\n(n = ',
                        other_assessed,
                        ')'),"', style = 'filled', width = 3, height = 0.5, pos='",xstart+11.5,",",ystart+3.5,"!', tooltip = '", tooltips[16], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  18 [label = '", paste0('Reports excluded:',
                        paste(paste('\n\t', 
                                    other_excluded[,1], 
                                    ' (n = ', 
                                    other_excluded[,2], 
                                    ')', 
                                    sep = ''), 
                              collapse = '')), "', style = 'filled', width = 3, height = 0.5, pos='",xstart+15.5,",",ystart+3.5,"!', tooltip = '", tooltips[17], "']\n
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

DiagrammeR::grViz(
  paste0("digraph TD {
  
  graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information']
  
  node [shape = box]
  identification [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='",-1.4,",",ystart+7.93,"!', width = 0.4, height = 2.6, tooltip = '", tooltips[20], "'];
  screening [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='",-1.4,",",ystart+4.5,"!', width = 0.4, height = 3.5, tooltip = '", tooltips[21], "'];
  included [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='",-1.4,",",h_adj1+0.87,"!', width = 0.4, height = ",2.5-h_adj2,", tooltip = '", tooltips[22], "'];\n
  ",
  previous_nodes,"
  node [shape = box,
        fontname = ", font, ",
        color = ", title_colour, "]
  3 [label = 'Identification of new studies via databases and registers', style = 'rounded,filled', width = 7, height = 0.5, pos='",xstart+6,",",ystart+9,"!', tooltip = '", tooltips[3], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  4 [label = '", paste0('Records identified from:\n\tDatabases (n = ',
                        database_results,
                        ')\n\tRegisters (n = ',
                        register_results,
                        ')'), "', width = 3, width = 3, height = 0.5, height = 0.5, pos='",xstart+4,",",ystart+7.5,"!', tooltip = '", tooltips[4], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  5 [label = '", paste0('Records removed before\nscreening:\n\tDuplicate records (n = ',
                        duplicates,
                        ')\n\tRecords marked as ineligible\nby automation tools (n = ',
                        excluded_automatic,
                        ')\n\tRecords removed for other\nreasons (n = ',
                        excluded_other,
                        ')'), "', width = 3, height = 0.5, pos='",xstart+8,",",ystart+7.5,"!', tooltip = '", tooltips[7], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  6 [label = '", paste0('Records screened\n(n = ',
                        records_screened,
                        ')'), "', width = 3, width = 3, height = 0.5, height = 0.5, pos='",xstart+4,",",ystart+5.5,"!', tooltip = '", tooltips[8], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  7 [label = '", paste0('Records excluded*\n(n = ',
                        records_excluded,
                        ')'), "', width = 3, height = 0.5, pos='",xstart+8,",",ystart+5.5,"!', tooltip = '", tooltips[9], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  8 [label = '", paste0('Reports sought for retrieval\n(n = ',
                        dbr_sought_reports,
                        ')'), "', width = 3, width = 3, height = 0.5, height = 0.5, pos='",xstart+4,",",ystart+4.5,"!', tooltip = '", tooltips[10], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  9 [label = '", paste0('Reports not retrieved\n(n = ',
                        dbr_notretrieved_reports,
                        ')'), "', width = 3, height = 0.5, pos='",xstart+8,",",ystart+4.5,"!', tooltip = '", tooltips[11], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  10 [label = '", paste0('Reports assessed for eligibility\n(n = ',
                         dbr_assessed,
                         ')'), "', width = 3, height = 0.5, pos='",xstart+4,",",ystart+3.5,"!', tooltip = '", tooltips[14], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  11 [label = '", paste0('Reports excluded:',
                         paste(paste('\n\t', 
                                     dbr_excluded[,1], 
                                     ' (n = ', 
                                     dbr_excluded[,2], 
                                     ')', 
                                     sep = ''), 
                               collapse = '')), "', width = 3, height = 0.5, pos='",xstart+8,",",ystart+3.5,"!', tooltip = '", tooltips[15], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  12 [label = '", paste0('New studies included in review\n(n = ',
                         new_studies,
                         ')\n',
                         'Reports of new included studies\n(n = ',
                         new_reports,
                         ')'), "', width = 3, height = 0.5, pos='",xstart+4,",",ystart+1.5,"!', tooltip = '", tooltips[18], "']
  
  ",othernodes,
  
  finalnode,"
  
  node [shape = square, width = 0, color=White]\n",
  A,"
  ",B,"
  
  ",
  Aedge,"
  
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
