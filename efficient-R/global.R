
# Global Options ---------------------------------------------------------------

  # options(papersize="a4", editor="notepad" , pager="internal")
  #   help_type="text", help_type="html" , show.signif.stars=FALSE)

  ## Set Default cran repository
  r <- getOption("repos"); r["CRAN"] <- "https://cran.rstudio.com/"
  options(tz="US/Eastern", stringsAsFactors=FALSE, repos = r)
  rm(r)

# Libraries --------------------------------------------------------------------

  ipak <- function(pkg){
    suppressMessages(require(utils, quietly = TRUE, warn.conflicts = FALSE))
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, function(x)
      suppressMessages(require(x, character.only = TRUE,
      quietly=TRUE, warn.conflicts = FALSE)))
  }

  Lib <- c(
    'stats',
    'grid',
    'gridExtra',
    'tidyverse',
    'magrittr',
    'ggExtra',
    'RColorBrewer',
    'lubridate',
    'hydrosystems')

# Initialize and terminate R ---------------------------------------------------

  updateFromGithub <- function(...) {
    devtools::install_github("rstudio/rmarkdown", ...)
    devtools::install_github("yihui/knitr", ...)
    devtools::install_github("hadley/tidyverse", ...)
    devtools::install_github("rstudio/flexdashboard", ...)
    devtools::install_github("rstudio/shiny", ...)
    devtools::install_github("rstudio/DT", ...)
    devtools::install_github("jbkunst/highcharter", ...)
    devtools::install_github("ramnathv/htmlwidgets", ...)
    devtools::install_github("mdlincoln/docthis", ...)
    devtools::install_github("ropensci/plotly", ...)
  }

  #.First() run at the start of every R session.
  .First <- function() {

    #Set Rlib folder in MAC-OS
    if(Sys.getenv("LOGNAME") == 'umit') {
      .libPaths("~/Umit/Rlibs")
    }

    ipak(Lib)
    cat("\nPre-loaded packages: \n"); print(Lib)
    cat("\nTip: updateFromGithub() to update essential packages")
    cat("\nHappy coding!", "-", base::date(), "\n")

  }


