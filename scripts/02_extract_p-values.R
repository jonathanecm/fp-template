# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.

# Notes ####
# Extractor:
# This script #### 
# Use the R studio "Show document outline" functionality to see the script structure. 
#####





# Libraries ####
library(tm)
library(tidyverse)
library(tidytext)
library(XML)
library(stringr)
source("./scripts/00_functions.R")

# Variables #### 
files <- list.files("./data/sampled_corpus", full.names = TRUE)
autors_number_vec <- vector()
section_vec <- vector()
article_name_vec <- vector()
journal_vec <- vector()
abstract_vec <- vector()
pub_year_vec <- vector()
title_vec <- vector()
subject_vec <- vector() 
abbrev_journal_name_vec <- vector()

possible_titles <- c('results', 'results and discussion', 'methods and results', 'results and discussions',
                     'results, discussion and conclusions', 'discussion and results',
                     'study results', 'results and conclusions', 'results and conclusion',
                     'experimental results', 'observations and results',
                     'results and observations', 'results and observation',
                     'observations, results and discussion', 'empirical results', 'materials and results',
                     'experimental procedures and results', 'research results', 'analysis and results',
                     'results and analysis', 'methodsandresults', 'resultsanddiscussion', 'resultsandconclusions',
                     'method and results')


# extract P-values and other information form the nXML file. 
for (i in seq_along(files)){
  
  content <- xmlTreeParse(files[i]) 
  if (!is.null(content$doc$children$article)) {
    body_sections <- getNodeSet(content$doc$children$article, "//sec")
    
    closeAllConnections() # Fixes the xmlValue bug related to too many textConnections opened. 
    for (j in seq_along(body_sections)){
      section_title <- getNodeSet(body_sections[[j]][["title"]], "//title")
      
      clarified_titles_vec <- text_clarifier(xmlValue(section_title[[1]], recursive = FALSE))
      
      if (length(clarified_titles_vec) > 0) {
        #print(clarified_titles_vec)
        if (clarified_titles_vec %in% possible_titles) {
          #browser()
          section <- getNodeSet(body_sections[[j]], "/sec")
          section_vec <- append(section_vec, xmlValue(section[[1]])) # Extracts result section. 
          
          # Extract journal name.
          #journal <- getNodeSet(content$doc$children$article, "//journal-title")
          #journal_vec <- append(journal_vec, xmlValue(journal[[1]])) # Extracts result section. 
          
          # Extract abbreviated journal name. 
          abbrev_journal_name <- getNodeSet(content$doc$children$article, "//journal-id[@journal-id-type='iso-abbrev']")
          if (!is_empty(abbrev_journal_name)) {
            abbrev_journal_name_vec <- append(abbrev_journal_name_vec, xmlValue(abbrev_journal_name[[1]]))
          } else {
            abbrev_journal_name_vec <- append(abbrev_journal_name_vec, "NA")
          }
          
          # Extract abtract.
          abstract <- getNodeSet(content$doc$children$article, "//abstract")
          if (!is_empty(abstract)) {
            abstract_vec <- append(abstract_vec, xmlValue(abstract[[1]]))
          } else {
            abstract_vec <- append(abstract_vec, "NA")
          }
          
          # Extract title. 
          title <- basename(content$doc$file)
          if (!is_empty(title)) {
            title_vec <- append(title_vec, title)
          } else {
            title_vec <- append(title_vec, "NA")
          }
          
         
          
          
          
          # Extract subject. 
          # subject <- getNodeSet(content$doc$children$article, "//subject")
          # if (!is_empty(subject)) {
          #   subject_vec <- append(subject_vec, xmlValue(subject[[1]]))
          # } else {
          #   subject_vec <- append(subject_vec, "NA")
          # }
          
          # Number of authors.
          autors <- length(getNodeSet(content$doc$children$article, "//contrib")) 
          autors_number_vec <- append(autors_number_vec, autors)
          
          # Get year of publication
          pub_year <- getNodeSet(content$doc$children$article[["front"]][["article-meta"]][["pub-date"]], "//year")
          pub_year_vec <- append(pub_year_vec, xmlValue(pub_year[[1]]))
          
          break()
        }
      }
    } 
  }
  
  print(i/length(files) * 100)
}


# Extract p-values and symbols into a data frame. 
p_remover <- function (x) gsub("[Pp]", "", x) # removes p from the string 

regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
p_values_vec <- as.vector(str_extract(section_vec, regexp)) %>%
  map_chr(p_remover) %>% 
  str_replace_all(" ", "") %>%
  stripWhitespace() %>%
  str_replace_all(" ", "") 

# symbols <- str_extract(p_values_vec, "[=<>≤≥]") 
# 
# p_values_df <- p_values_vec %>% 
#   as_tibble() %>%
#   separate(value, c("symbol", "p_value"), sep = "[=<>≤≥]") 

# # Extractig p-values form abstracts
regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
abstract_p_val_vec  <- as.vector(str_extract(abstract_vec, regexp)) %>%
  map_chr(p_remover) %>%
  str_replace_all(" ", "") %>%
  stripWhitespace() %>%
  str_replace_all(" ", "")
# 
# symbols <- str_extract(abstract_p_val_vec, "[=<>≤≥]") 
# 
# abstract_p_val_df <- abstract_p_val_vec %>% 
#   as_tibble() %>%
#   separate(value, c("symbol", "p_value"), sep = "[=<>≤≥]") 


# create data frame. ####
p_values_df <- tibble(p_values_vec, abstract_p_val_vec, autors_number_vec, 
                      pub_year_vec, title_vec, abbrev_journal_name_vec)

names(p_values_df) <- c("result", "abstract", "num_authors", 
                        "pub_year", "title", "abbrev_journal_name")  

symbols_remover <- function (x) gsub("[=<>≤≥]", "", x)
p_values_df <-p_values_df %>% 
  gather(`result`, `abstract`, key = "section", value = "p-values") %>%
  mutate(symbols = str_extract(`p-values`, "[=<>≤≥]"), 
        `p-values` = as.double(map_chr(`p-values`, symbols_remover)))

# safe data frame.
write.csv(p_values_df, "./data/raw/p-values_df.csv")



