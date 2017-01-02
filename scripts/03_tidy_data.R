# Cleaning FOR categories files.
files_path <- file.path(".", "data", "raw") %>%
  list.files(full.names = TRUE, recursive = TRUE)

  categories_df <- read.csv(files_path[1], stringsAsFactors = FALSE) %>% 
    select(abbrev_journal_name = Abbreviation, 4:7, -5)

  # Getting p-values.csv files. 
  p_values_df_full_set <- read.csv("./data/raw/p_values_df_full_set.csv", stringsAsFactors = FALSE)
  p_values_df <- read.csv("./data/raw/p-values_df.csv", stringsAsFactors = FALSE)

  # Joining sets.
  tidy_p_values <- left_join(p_values_df, categories_df, by = "abbrev_journal_name") %>%
    select(2:11) %>%
    write.csv("./data/processed/tidy_p_values.csv")
  
  tidy_p_values_df_full_set <- left_join(p_values_df_full_set, categories_df, by = "abbrev_journal_name") %>%
    select(2:11) %>%
    write.csv("./data/processed/tidy_p_values_df_full_set.csv")






