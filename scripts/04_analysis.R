###############################################################
# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.
###############################################################
# This script contains a series of helper functions:
# text_clarifier: Clarifies text.
#
#### The following based on Head, M. L., et al (2015) analysis:
# bootstrap.binomial.bias.test: Carries out a binomial test
# binomial.bias.test: Creates the limits for the binomial test
###############################################################

# libraries
#library(RColorBrewer)
source("./scripts/00_functions.R")

# Fiting data set for analysis.
p_values_df <- read.csv("./data/processed/tidy_p_values_df_full_set.csv") %>%
  filter((p.value >= 0) & (!is.na(p.value)))


ggplot(p_values_df) +
  geom_histogram(aes(p.value), binwidth = 0.002, na.rm = TRUE) +
  labs(title = "Distribution of significant p-values",  subtitle = "Significace trhreshold <= 0.05",
       x = "p-values",
       y = "Number of p-values")

ggsave("./plots/p_val_distribution.png", scale = 1)


# Distribution of p-values by categories. 
# Subsetting more common categories. 
common_cat <- p_values_df %>%
  select(Category, p.value) %>%
  group_by(Category) %>%
  count() %>%
  filter(n > 2000) %>% 
  select(Category) 

p_values_df_common_cat <- filter(p_values_df,  Category %in% common_cat$Category) 

ggplot(p_values_df_common_cat) +
  geom_freqpoly(aes(p.value, color = Category), bins = 30) +
  labs(title = "Distribution of p-values by categories",  subtitle = "Significace trhreshold <= 0.05",
       x = "p-values",
       y = "Frequency")

ggsave("./plots/p_val_cat_distribution.png", scale = 1)


# Distribution of p-values by year of publication. 
common_pub_year <- p_values_df %>%
  select(pub_year, p.value) %>%
  group_by(pub_year) %>%
  select(pub_year, p.value) %>% count()

ggplot(p_values_df) +
  geom_bar(aes(pub_year)) +
  labs(title= "P-values by Year of Publication ",
       x = "Year of Publication",
       y = "Frequency")

ggsave("./plots/p_val_year_distribution.png", scale = 1)

# P-values by section (abstract or results)
ggplot(p_values_df) +
  geom_bar(aes(section)) +
  labs(title= "P-values by Section (Abstract or Result)",
       x = "p-values",
       y = "Frequency")

ggsave("./plots/p_val_section_distribution.png", scale = 1)

# P-values in abstract section
ggplot(p_values_df) +
  geom_histogram(aes(p.value), bins = 30) +
  labs(title = "P-values by Sections",  subtitle = "Significace trhreshold <= 0.05",
       x = "p-values",
       y = "Frequency") +
  facet_wrap(~ section)

ggsave("./plots/sections_pval_distribution.png", scale = 1)


# binomial test.
results_df <- p_values_df %>%
  filter((section == "results") & (p.value != "NA") & (p.value < 1))

abstracts_df <- p_values_df %>%
  filter((section == "abstract") & (p.value != "NA") & (p.value < 1))


# binomial test all data.
replications  <- 10

results.bias.test <- bootstrap.binomial.bias.test(results_df, replications)
abstracts.bias.test <- bootstrap.binomial.bias.test(abstracts_df, replications)

write.csv(results.bias.test, file="./data/processed/results_combined_data.csv")
write.csv(abstracts.bias.test, file="./data/processed/abstracts_combined_data.csv")

# Binomial test by categories. 
results.FoR.test <- bootstrap.FoR.test(results_df, replications)
abstract.FoR.test <- bootstrap.FoR.test(abstracts_df, replications)

# Creating data frame for results. 
both <- results.FoR.test[which(rowSums(cbind(results.FoR.test$binomial2.bias.lower, 
                                             results.FoR.test$binomial2.bias.upper)) > 10),]

both <- rbind(both,abstract.FoR.test[abstract.FoR.test$category %in% both$category, ])
both$section <- rep(c("Results", "Abstract"), each = 8)
both$n <- both$binomial2.bias.higher + both$binomial2.bias.lower
both$prop.upper <- both$binomial2.bias.higher / both$n
both$lowerCI <-NA
both$upperCI <- NA

for(i in 1:nrow(both)) both[i, c(ncol(both)-1, ncol(both))] <- as.numeric(binom.test(c(both[i,7], 
                                                                                       both[i,6]), 
                                                                                     alternative="greater")$conf)

both$category <- factor(both$category, levels = rev(sort(unique(both$category))))


#colors <- colorRampPalette(c("white","red"))

# Plot of binomial distribution by category. 
ggplot(both, aes(x = prop.upper, y = category))  + 
  facet_wrap(~section) + geom_errorbarh(aes(xmin = lowerCI, xmax = upperCI),height=0.2) + 
  geom_point(aes(colour=log(n)),pch=5, size=4) + geom_vline(x=0.5, linetype=2, xintercept = 0.5) + 
  theme_bw() + scale_colour_gradient(low = "yellow", high = "blue") + 
  labs(title = "P-values by Sections",
       x = "p-values",
       y = "Categories") 

ggsave("./plots/binomial_distribution_by_cat.png", scale = 1)
