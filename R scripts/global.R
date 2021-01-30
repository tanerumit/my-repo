
## GLOBAL OPTIONS FILE FOR R


# Source scripts from an online repository such as github
source_https <- function(url, ...) {
  # load package
  reuire(RCurl)
 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

# Set global R options:
r <- getOption("repos"); r["CRAN"] <- "https://cran.rstudio.com/"
options(tz="US/Eastern", stringsAsFactors=FALSE, repos = r,
  show.signif.stars = FALSE)
# papersize = "a4", editor = "notepad" , pager = "internal")
#help_type="text", help_type = "html" , show.signif.stars = FALSE

libs <- c('stats', 'grid', 'gridExtra', 'tidyverse', 'magrittr',
  'ggExtra', 'tidyr', 'RColorBrewer', 'lubridate', 'hydrosystems',
  'readr', 'dplyr', 'readxl', "prim", "purrr")

ipak <- function(pkg) {
  
    suppressMessages(require(utils, quietly = TRUE, warn.conflicts = FALSE))
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, function(x)
      suppressMessages(require(x, character.only = TRUE,
      quietly=TRUE, warn.conflicts = FALSE)))
}

#.First() run at the start of every R session.
.First <- function() {

#  #Set Rlib folder in MAC-OS
#  if(Sys.getenv("LOGNAME") == 'umit') {
#    .libPaths("~/Umit/Rlibs")
#  }

  ipak(libs)
  
  cat("\nPre-loaded packages: \n"); print(Lib)
  cat("Working dir:", getwd())
  cat("\nHappy coding!", "-", base::date(), "\n")
}

rm(r)
rm(ipak)