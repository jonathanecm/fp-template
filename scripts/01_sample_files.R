# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.

# Libraries
library(tidyverse)

# Functions:
# This script contain a list of functions used in the analysis

# Create a list of files' names.

files_paths <- file.path(".", "files") %>% 
  list.files(full.names = TRUE, recursive = TRUE)

# Takes a sample of files' names.

sampled_files <- sample(files_paths, 100)
files_names <- basename(sampled_files)

# Creates a corpus of sampled files in the "corpus" directory. 
destination_files <- file.path(".", "corpus", files_names)
file.copy(sampled_files, destination_files, overwrite = FALSE)
