# This file executes all the files ???

# Cleans existing files and directories and creates new directores.

#paths <- c("./data/sampled_corpus", "./data/processed")
paths <- c("./data/processed")

for (path in paths) {
    unlink(path, recursive = TRUE) 
    dir.create(path)
}

#source("./scripts/01_sample_files.R")
source("./scripts/02_extract_p-values.R")
source("./scripts/03_tidy_data.R")
# source("02_analysis.R")
# rmarkdown::render("03_report.Rmd", output_dir = "Final Project")

