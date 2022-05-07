# PRISMA2020 Flow Diagram <img src="https://raw.githubusercontent.com/nealhaddaway/PRISMA2020/master/PRISMA2020-hex.png" align="right" width="15%"/>

You can use this package to produce a flow diagram that conforms to the PRISMA 2020 standards using the `PRISMA_flowdiagram()` function. The data can be manually entered into the function, or loaded up using the template CSV file provided in 'INST/EXTDATA/'. The function, (if 'interactive = TRUE') produces an interactive HTML ouput with each box linking to a specific page (e.g. of search results or methods details), and hover-over tooltips for further information. 
<br>
<img src="https://raw.githubusercontent.com/nealhaddaway/PRISMA2020/master/inst/extdata/PRISMA.png" width="70%" />
<br>

The 'Previous' and 'Other' study arms of the flowchart can be toggled on and off and removed or added to the diagram by specifying this in the function inputs.

A static version is produced otherwise.  

<a href="https://srflowdiagram.github.io/template.html" target="_blank">See the interactive template here.</a><br>


<a href="https://estech.shinyapps.io/prisma_flowdiagram/" target="_blank">Visit the web-based Shiny app for a point-and-click user interface here.</a>

Please cite as:<br>
 Haddaway, N. R., Page, M. J., Pritchard, C. C., & McGuinness, L. A. (2022). PRISMA2020: An R package and Shiny app for producing PRISMA 2020-compliant flow diagrams, with interactivity for optimised digital transparency and Open Synthesis. Campbell Systematic Reviews, 18, e1230. <a href=https://doi.org/10.1002/cl2.1230>https://doi.org/10.1002/cl2.1230</a><br>
<a id="raw-url" href="https://raw.githubusercontent.com/nealhaddaway/PRISMA2020/master/inst/extdata/citation.ris">Citation in .ris format (right click 'Save Link As')</a>

<!-- badges: start -->
[![R build status](https://github.com/nealhaddaway/PRISMA2020/workflows/R-CMD-check/badge.svg/)](https://github.com/nealhaddaway/PRISMA2020/actions/)
![GitHub all releases](https://img.shields.io/github/downloads/nealhaddaway/PRISMA2020/total?style=plastic/)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/nealhaddaway/PRISMA2020)
![GitHub Repo stars](https://img.shields.io/github/stars/nealhaddaway/PRISMA2020?style=social)
<!-- badges: end -->
