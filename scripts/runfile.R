# This file executes all the files ???

# Cleans existing files and directories and creates new directores.
paths <- c("./corpus", "data")

for (path in paths) {
    unlink(path, recursive = TRUE) 
    dir.create(path)
}

source("./scripts/01_sample_files.R")
source("./scripts/02_extract_p-values.R")
# source("02_analysis.R")
# rmarkdown::render("03_report.Rmd", output_dir = "Final Project")

