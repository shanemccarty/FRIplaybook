## install.R
## Run this file ONE time per computer: click the "Source" button.
## It is safe to run again: it only installs the packages you are missing.
## comments indicate what each package is used for

packages <- c(
  "readxl",     # IMPORT: read in xlsx files with read_excel()
  "haven",      # IMPORT: read in SPSS .sav files with read_sav()
  "tidyverse",  # TIDY, TRANSFORM, VISUALIZE: tidyr, dplyr, ggplot2, and more
  "naniar",     # TIDY: replace codes like -99 with missing values (NA)
  "psych",      # TRANSFORM: composite scores, cronbach's alpha, descriptive stats
  "knitr",      # COMMUNICATE: tables with kable()
  "rmarkdown",  # COMMUNICATE: lets RStudio render Quarto reports with R code
  "ggpubr",     # VISUALIZE: one-line publication plots, ggdensity() and ggviolin() (Compare 2 Groups)
  "see"         # VISUALIZE: half-violin (raincloud) plots with geom_violinhalf() (Compare 2 Groups; Compare 1 Group, Pre/Post)
)

## EXTRAS: only needed for a few chapters. Change FALSE to TRUE if you use them.
## (Instructors who render the whole playbook need these.)
install_extras <- FALSE

extras <- c(
  "english",    # write numbers as words (Methods & Results)
  "shiny",      # interactive apps (Shiny Data Viz, Advanced Plays)
  "plotly",     # hover-and-zoom plots in a Shiny app
  "DT"          # searchable tables in a Shiny app
)

if (install_extras) packages <- c(packages, extras)

## Find the packages that are not installed yet, then install only those
missing <- packages[!packages %in% rownames(installed.packages())]

if (length(missing) > 0) {
  install.packages(missing)
} else {
  message("All packages are already installed. You are ready to go!")
}
