# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.

# Extractor:
# This script #### 

# Libraries ####
library(tm)
library(tidyverse)
library(tidytext)
library(XML)
source("./scripts/00_functions.R")


# Files location.
files_path <- "./corpus" 

# Creating a corpus. 
corpus <- Corpus(DirSource(files_path), readerControl = list(reader = readReut21578XML))

# Defining possible titles of the result section. 
possible_titles <- c('results', 'results and discussion', 'methods and results', 'results and discussions',
                   'results, discussion and conclusions', 'discussion and results',
                   'study results', 'results and conclusions', 'results and conclusion',
                   'experimental results', 'observations and results',
                   'results and observations', 'results and observation',
                   'observations, results and discussion', 'empirical results', 'materials and results',
                   'experimental procedures and results', 'research results', 'analysis and results',
                   'results and analysis', 'methodsandresults', 'resultsanddiscussion', 'resultsandconclusions',
                   'method and results')

## Extracting results section.
#Covert this to a map function.
section_vec <- vector()
for (i in seq_along(corpus)){
    if (!is.null(corpus[[i]]$content$doc$children$article[["body"]])) {
        body_sections <- getNodeSet(corpus[[i]]$content$doc$children$article[["body"]], "//sec")
        
        for (j in seq_along(body_sections)){
            section_title <- getNodeSet(body_sections[[j]][["title"]], "//title")

            clarified_titles_vec <- titles_clarifier(xmlValue(section_title[[1]]))
            
            if (length(clarified_titles_vec) > 0) {
                if (clarified_titles_vec %in% possible_titles) {
                    #print(xmlValue(body_sections[[j]]))
                    section <- getNodeSet(body_sections[[j]], "/sec")
                    section_vec <- append(section_vec, xmlValue(section[[1]]))
                    break()
                }
            }
        }
    }
}

abstract_vec <- vector()
for (i in seq_along(corpus)) {
   abstract <- getNodeSet(corpus[[1]]$content$doc$children$article, "//abstract")
   abstract_vec <- append(abstract_vec, xmlValue(abstract[[1]]))
}

article_vec <- vector()
for (i in seq_along(corpus)) {
    article <- corpus[[i]]$content$doc$children$article
    article_vec <- append(article_vec, xmlValue(article))
}

regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
str_extract_all(abstract_vec,regexp)
all <- str_extract_all(section_vec,regexp)
one <- str_extract(section_vec,regexp)

