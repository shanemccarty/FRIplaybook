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
  "rmarkdown"   # COMMUNICATE: lets RStudio render Quarto reports with R code
)

## EXTRAS: only needed for a few chapters. Change FALSE to TRUE if you use them.
## (Instructors who render the whole playbook need these.)
install_extras <- FALSE

extras <- c(
  "see",        # half-violin plots (Visualize Pre/Post Data)
  "gghalves",   # half-violin plots (Visualize Pre/Post Data)
  "ggdist",     # raincloud plots (Visualize Pre/Post Data)
  "english",    # write numbers as words (Methods & Results)
  "NHANES"      # CDC practice dataset (Import Data Once)
)

if (install_extras) packages <- c(packages, extras)

## Find the packages that are not installed yet, then install only those
missing <- packages[!packages %in% rownames(installed.packages())]

if (length(missing) > 0) {
  install.packages(missing)
} else {
  message("All packages are already installed. You are ready to go!")
}
