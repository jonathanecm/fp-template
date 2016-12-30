# Cleaning FOR categories files.
files_path <- file.path(".", "data", "raw") %>%
  list.files(full.names = TRUE, recursive = TRUE)

categories_df <- read.csv(files_path, stringsAsFactors = FALSE) %>% 
  select(abbrev_journal_name = Abbreviation, 4:7, -5)

# Getting p-values.csv files. 
p_values_df <- read.csv("./data/processed/p-values_df.csv", stringsAsFactors = FALSE)

# Joining sets.
tidy_p_values <- left_join(p_values_df, categories_df, by = "abbrev_journal_name") %>%
  select(2:13) %>%
  write.csv("./data/processed/tidy_p_values.csv")






