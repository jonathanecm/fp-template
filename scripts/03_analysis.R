# Script that performs a exploratory data analysis.
library(tidyverse)
library(stringr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(devtools)
library(tm)

# Loads corpus 
corpora_df <- as_tibble(read.csv(file = "./data/tidy_data/tidy_corpora.csv", 
                                              stringsAsFactors = FALSE))
# Frequency plot. 
corpora_df %>%
    count(word, sort = TRUE) %>%
    filter(n > 200) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = word)) +
    geom_bar(stat = "identity") +
    labs(title = "Most frequent words", x = "Words", y = "Number of words") +
    coord_flip() 
    ggsave("./images/plot_1.png", height = 3, width = 5)
  
# Trasnforming corpora_df into a term-matrix corporta_dtm
corpora_counts <- corpora_df %>%
    count(id, word, sort = TRUE)

corpora_dtm <- corpora_counts %>%
    cast_dtm(id, word, n)

corpora_dtm <- removeSparseTerms(corpora_dtm, 0.1)     # Eliminating sparse terms.   

corpora_dtm

# # Creating a wordcloud. 
# freq <- sort(colSums(as.matrix(corpora_dtm)), decreasing=TRUE)
# 
# png(filename="./images/wordcloud.png")
# set.seed(123)
# wordcloud(names(freq), freq, min.freq = 50, colors=brewer.pal(6, "Dark2"), scale=c(8,.6))
# dev.off()
# 
# # Topic modeling
# 
# # Topic model. 
# LDA_topics <- LDA(corpora_dtm, k = 4, control = list(seed = 11091987))
# 
# # Transforming the LDA topics into a table. 
# topics_table <- tidy(LDA_topics)
# 
# top_terms <- topics_table %>%
#     group_by(topic) %>%
#     top_n(6, beta) %>%
#     ungroup() %>%
#     arrange(topic, -beta)
# top_terms
# 
# # Plotting top terms. 
# top_terms %>%
#     mutate(term = reorder(term, beta)) %>%
#     ggplot(aes(term, beta, fill = factor(topic))) +
#     geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
#     facet_wrap(~ topic, scales = "free", ncol = 2) +
#     labs(title = "Top Terms in LDA", x = "Words", y = "Number of words") +
#     coord_flip()
# ggsave("./images/topics.png", height = 5, width = 5)
# 
# # Determining perplexity. 
# ## Generating different LDA topic models. 
# topics_number <- c(2, 4, 10, 20, 50, 100)
# ap_lda_compare <- topics_number %>%
#     map(LDA, x = corpora_dtm, control = list(seed = 1109))
# 
# # Plotting the different models. 
# data_frame(k = topics_number,
#            perplex = map_dbl(ap_lda_compare, perplexity)) %>%
#     ggplot(aes(k, perplex)) +
#     geom_point() +
#     geom_line() +
#     labs(title = "Evaluating LDA topic models",
#          subtitle = "Optimal number of topics (smaller is better)",
#          x = "Number of topics",
#          y = "Perplexity")
# ggsave("./images/topic_models.png", width = 5)
# 
# # Ploting the 6 first LDA topics
# lda_td <- tidy(ap_lda_compare[[6]])
# 
# top_terms <- lda_td %>%
#     group_by(topic) %>%
#     top_n(5, beta) %>%
#     ungroup() %>%
#     arrange(topic, -beta)
# 
# top_terms %>%
#     filter(topic <= 6) %>%
#     mutate(term = reorder(term, beta)) %>%
#     ggplot(aes(term, beta, fill = factor(topic))) +
#     geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
#     facet_wrap(~ topic, scales = "free", ncol = 3) +
#     labs(title = "Top 10 LDA Topics", x = "Terms", y = "Beta") +
#     coord_flip()
# ggsave("./images/top_six_LDAtopics.png", width = 7)

