#########################################################
# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.
#########################################################
# This script create a folder output for tidy data files.
# and runs and render scripts. 
#########################################################

# Library
library(rmarkdown)

# Cleans existing files and directories and creates new directores.
paths <- c("./data/processed", "plots")

for (path in paths) {
    unlink(path, recursive = TRUE) 
    dir.create(path)
}

#source("./scripts/01_sample_files.R") # To activiate this code full corpus of data is needed. 
source("./scripts/02_extract_p-values.R")
source("./scripts/03_tidy_data.R")
source("./scripts/04_analysis.R")
rmarkdown::render_site()


