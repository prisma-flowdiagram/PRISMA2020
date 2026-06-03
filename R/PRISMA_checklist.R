#' Save PRISMA2020 flow diagram
#' @description Save the output from [PRISMA_flowdiagram()] to the
#' working directory.
#' @param plotobj A plot produced using [PRISMA_flowdiagram()].
#' @param filename The filename to save (including extension)
#' @param filetype The filetype to save the plot in, supports:
#' HTML, ZIP, PDF, PNG, SVG, PS and WEBP
#' (if NA, the filetype will be calculated out based on the file extension)
#' HTML files maintain hyperlinks and tooltips
#' The ZIP option creates an archive containing the HTML file,
#' alongside supporting javascript and css files in an adjacent folder,
#' instead of embedded base64 within the HTML file
#' @param overwrite if TRUE, will overwrite an existing file
#' @param width passed as the width argument to
#' [rsvg::rsvg()] and similar functions
#' @param height passed as the height argument to
#' [rsvg::rsvg()] and similar functions
#' @param css passed as the css argument to
#' [rsvg::rsvg()] and similar functions
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
#' PRISMA_checklist(plot, filename = tempfile(), filetype="html");

library(dplyr)
library(flextable)

filename='inst/extdata/PRISMAchecklist.xlsx';
h=8;
w=11;
fontsize=6

#' @export
PRISMA_checklistdata <- function(filename, 
                                 h=8,
                                 w=11,
                                 fontsize=6){

#read in the data  
checklist<-readxl::read_excel(filename)

#replace semicolons with newlines
checklist<-as.data.frame(apply(checklist,2,function(x){stringr::str_replace_all(x,'; ','\n \n')}))

#specify subheader row names
subheaders<-c("TITLE","ABSTRACT","INTRODUCTION","METHODS","RESULTS","DISCUSSION","OTHER INFORMATION")
#find subheader row numbers
subheaderrows<-which(checklist$`Section and topic`%in%subheaders)

#pipe checklist to flextable 
checklist %>% 
  flextable() %>% #make flextable
  fontsize(., size=fontsize, part='all') %>% #set fontsize
  height(., height = (h/(nrow(checklist)))) %>% #set height
  width(., width = (w/(ncol(checklist)))) %>% #set width
  theme_box() %>% #add box grid
  flextable::merge_h_range(i = c(subheaderrows), j1=1, j2=4, part = "body") %>% #merge subheader rows
  bold(i = subheaderrows, bold= TRUE, part = "body") %>% #make subheader rows bold
  width(j = 1:4, width=c(1,0.5,5.5,4), unit = "in") #adjust widths of box

}