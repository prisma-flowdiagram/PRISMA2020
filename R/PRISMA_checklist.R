#' Save PRISMA2020 flow diagram
#' @description Save the output from [PRISMA_flowdiagram()] to the
#' working directory.
#' @param plotobj A plot produced using [PRISMA_flowdiagram()].
#' @param filename The filename of the checklist (including extension)
#' @param h Height of the page (default to just below A4 height)
#' @param w Width of the page (default to just below A4 height)
#' @param fontsize Fontsize (default to 6)
#' 
#' @param overwrite if TRUE, will overwrite an existing file
#' @param width passed as the width argument to
#' [rsvg::rsvg()] and similar functions
#' @param height passed as the height argument to
#' [rsvg::rsvg()] and similar functions
#' @param css passed as the css argument to
#' [rsvg::rsvg()] and similar functions
#' @return the absolute filename of the saved diagram plot.
#' @examples


# library(dplyr)
# library(flextable)
# filename='inst/extdata/PRISMAchecklist.xlsx';
# h=8;
# w=11;
# fontsize=6

#' @export
PRISMA_checklistdata <- function(filename,
                                 filetype,
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
checklist_flextable<-
  checklist %>% 
  flextable() %>% #make flextable
  fontsize(., size=fontsize, part='all') %>% #set fontsize
  height(., height = (h/(nrow(checklist)))) %>% #set height
  width(., width = (w/(ncol(checklist)))) %>% #set width
  theme_box() %>% #add box grid
  flextable::merge_h_range(i = c(subheaderrows), j1=1, j2=4, part = "body") %>% #merge subheader rows
  bold(i = subheaderrows, bold= TRUE, part = "body") %>% #make subheader rows bold
  width(j = 1:4, width=c(1,0.5,5.5,4), unit = "in") #adjust widths of box


return(checklist_flextable)
}