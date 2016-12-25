# This script process the corpora into a tidy format. 

# Libraries necessary for the script to run.
library(tm)
library(dplyr)
library(magrittr)
library(tidytext)
library(tidyverse)

# Creating a corpora from pdf in "./data/raw_data directory. 
files_paths <- file.path(".", "corpora")    
corpora <- Corpus(DirSource(files_paths), readerControl = list(reader = readPDF))
corpora_frame <- tidy(corpora)

# Corpora Transformation
## This block of code performs a series of transfromation to the "text" column. 

toSpace <- function(x, pattern) gsub(pattern, " ", x)
corpora_frame$text <- map_chr(corpora_frame$text, toSpace, "\n")
corpora_frame$text <- map_chr(corpora_frame$text, toSpace, "\f")
corpora_frame$text <- map_chr(corpora_frame$text, toSpace, '\xfc\xbe\x8e\x96\x84\xbc')
corpora_frame$text <- map_chr(corpora_frame$text, toSpace, 'ibid')
corpora_frame$text <- map_chr(corpora_frame$text, toSpace, 'dx')
corpora_frame$text <- map_chr(corpora_frame$text, toSpace, 'fx')
corpora_frame$text <- map_chr(corpora_frame$text, toSpace, 'ln')
corpora_frame$text <- map_chr(corpora_frame$text, removeNumbers)
corpora_frame$text <- map_chr(corpora_frame$text, removePunctuation)
corpora_frame$text <- map_chr(corpora_frame$text, stripWhitespace)

# Select id and text fields from data frame. 
data(stop_words)
corpora_frame <- corpora_frame %>% 
    select(id, text) %>%
    unnest_tokens(word, text)

# Eliminating stopwords. 
corpora_frame <- corpora_frame %>%
    anti_join(stop_words)

# Writes data frame to disk. 
write_csv(corpora_frame, "./data/tidy_data/tidy_corpora.csv")
