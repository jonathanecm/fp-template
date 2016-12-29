# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.

# Libraries
library(tidyverse)

# Functions:
# This script contain a list of functions used in the analysis

# Create a list of files' names.

full_corpus_path <- file.path(".", "data", "full_corpus") %>% 
  list.files(full.names = TRUE, recursive = TRUE)

# Takes a sample of files' names.
sampled_files <- sample(full_corpus_path, 50)
files_names <- basename(sampled_files)

# Creates a corpus of sampled files in the "corpus" directory. 
destination_files <- file.path(".", "data", "sampled_corpus", files_names)
file.copy(sampled_files, destination_files, overwrite = FALSE)

# Saving memory. 
rm(full_corpus_path)
rm(destination_files)
