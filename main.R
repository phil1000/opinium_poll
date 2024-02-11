# project launch file

# Load global packages --------------------------------------

# check if librarian package manager is loaded
if (!requireNamespace("librarian")) install.packages("librarian", quiet = TRUE)

# use suppress to prevent build warnings
suppressWarnings(
  librarian::stock(yaml, tidyverse, tools, fs, curl, httr,
                   polite, rvest, tidyxl, unpivotr, ggplot2, ggpubr,
                   lubridate, ggrepel, scales, sf, svglite, openxlsx, xlsx,
                   pdftools, glue,
                   quiet = TRUE)
)

source("./R/run_analysis.R", local=TRUE)
source("./R/logger.R", local=TRUE)

# Sort out logging
logger <- get_dhsc_logger()
logger$set_threshold("log.console", "INFO")

logger$info("[Started]")

run_analysis()

logger$info("[End]")
