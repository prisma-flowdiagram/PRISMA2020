year <- sub("-.*", "", meta$Date)
note <- sprintf("R package version %s", meta$Version)

bibentry(bibtype = "Manual",
         title = "PRISMA2020: R package and ShinyApp for producing PRISMA 2020 compliant flow diagrams (Version 0.0.1)",
         author = person(c("Neal", "R."), "Haddaway"),
         year = year,
         note = note,
         url = "http://doi.org/10.5281/zenodo.4287835"
)
