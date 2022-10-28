<!-- badges: start -->
[![Check & Deploy](https://github.com/prisma-flowdiagram/PRISMA2020/actions/workflows/check-and-deploy.yml/badge.svg)](https://github.com/prisma-flowdiagram/PRISMA2020/actions/workflows/check-and-deploy.yml)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/prisma-flowdiagram/PRISMA2020)
![GitHub Repo stars](https://img.shields.io/github/stars/prisma-flowdiagram/PRISMA2020?style=social)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4287834.svg)](https://doi.org/10.5281/zenodo.4287834)
[![DOI](https://zenodo.org/badge/DOI/10.1002/cl2.1230.svg)](https://doi.org/10.1002/cl2.1230)
<!-- badges: end -->

# PRISMA2020 Flow Diagram <img src="https://raw.githubusercontent.com/prisma-flowdiagram/PRISMA2020/master/PRISMA2020-hex.png" align="right" width="15%"/>

You can use this package to produce a flow diagram that conforms to the PRISMA 2020 standards using the `PRISMA_flowdiagram()` function. The data can be manually entered into the function, or loaded up using the template CSV file provided in 'INST/EXTDATA/'. The function, (if 'interactive = TRUE') produces an interactive HTML ouput with each box linking to a specific page (e.g. of search results or methods details), and hover-over tooltips for further information. 
<br>
<img src="https://raw.githubusercontent.com/prisma-flowdiagram/PRISMA2020/master/inst/extdata/PRISMA.png" width="70%" />
<br>

The 'Previous' and 'Other' study arms of the flowchart can be toggled on and off and removed or added to the diagram by specifying this in the function inputs.

A static version is produced otherwise.  

<a href="https://srflowdiagram.github.io/template.html" target="_blank">See the interactive template here.</a><br>


<a href="https://estech.shinyapps.io/prisma_flowdiagram/" target="_blank">Visit the web-based Shiny app for a point-and-click user interface here.</a>

---
## Docker Installation

You can quickly install the PRISMA2020 package and run the included
example shinyapp using [Docker](https://docs.docker.com/engine/install/).

```bash
docker build . -t prisma-shiny:1
docker run -it --rm -p 3838:3838 prisma-shiny:1
```

Then visit http://localhost:3838/app in your web browser.
To stop the app, press `Ctrl+C` in the terminal.

---

Please cite as:<br>
 Haddaway, N. R., Page, M. J., Pritchard, C. C., & McGuinness, L. A. (2022). PRISMA2020: An R package and Shiny app for producing PRISMA 2020-compliant flow diagrams, with interactivity for optimised digital transparency and Open Synthesis. Campbell Systematic Reviews, 18, e1230. <a href=https://doi.org/10.1002/cl2.1230>https://doi.org/10.1002/cl2.1230</a><br>
<a id="raw-url" href="https://raw.githubusercontent.com/nealhaddaway/PRISMA2020/master/inst/extdata/citation.ris">Citation in .ris format (right click 'Save Link As')</a>
