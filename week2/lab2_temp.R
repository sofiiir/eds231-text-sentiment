## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 9,
  fig.height = 5,
  fig.align = "center"
)


## ----packages-----------------------------------------------------------------
library(tidyverse)      # data wrangling and plotting
library(tidytext)       # text mining in tidy format
#library(LexisNexisTools) # reading Nexis Uni.docx exports
library(SnowballC)      # Porter stemmer
library(textstem)       # lemmatization
library(scales)         # axis formatting
library(lubridate)      # date handling
library(quanteda)       # corpus and DFM construction
library(quanteda.textstats) # textstat_keyness()
library(quanteda.textplots) # textplot_keyness()


## ----load-data----------------------------------------------------------------
# pre_files <- list.files(
#   path = here::here("Nexis", "data-center"),
#   pattern = "\\.docx$",
#   full.names = TRUE,
#   recursive = TRUE,
#   ignore.case = TRUE
# )
# # read in the data obejct
# pre_dat <- lnt_read(pre_files)
# 
# # access the subdata frames
# pre_meta_df       <- pre_dat@meta
# pre_articles_df   <- pre_dat@articles
# pre_paragraphs_df <- pre_dat@paragraphs
# 
# # access the data 
# df <- tibble(
#   date     = pre_meta_df$Date,
#   headline = pre_meta_df$Headline,
#   id       = pre_meta_df$ID,
#   text     = pre_articles_df$Article,
#   source   = pre_meta_df$Newspaper
# )


url <- "https://raw.githubusercontent.com/MaRo406/EDS-231-text-sentiment/refs/heads/main/data/nexis-dat.csv"
df <- read_csv(url)


## ----clean-text---------------------------------------------------------------
# Drop articles with no date — these are often metadata rows, not real articles
df <- df[!is.na(df$date),]

# Remove URLs — Nexis embeds source URLs inline in the text body
df$text <- gsub("https?\\S+", "", df$text)

# Convert to ASCII — Nexis exports sometimes include curly quotes, em-dashes,
# and other Unicode characters that cause issues in tokenization
df$text <- iconv(df$text, to = "ASCII//TRANSLIT", sub = "")

# Also, remove possessives
df$text <- gsub("'s\\b", "", df$text, ignore.case = TRUE)


## ----inspect------------------------------------------------------------------
glimpse(df)

cat("\nDate range:", format(min(df$date)),"to", format(max(df$date)), "\n")
cat("Unique sources:", n_distinct(df$source), "\n")
cat("Total articles:", nrow(df), "\n")


## ----articles-per-source------------------------------------------------------
df |>
  count(source, sort = T) |> #
  filter(n > 10, source != "") |> #
  ggplot(aes(x = reorder(source, n), y = n)) +
  geom_col(fill = "#2c7bb6") +
  coord_flip() +
  labs(title = "Articles by Source",
       x = NULL, y = "Article count") +
  theme_minimal(base_size = 13)


## ----tokenize-----------------------------------------------------------------
tokens <- df |>
  select(id, date, source, text) |> 
  unnest_tokens(
    output = word,
    input = text,
    token = "words"
  )


glimpse(tokens)
cat("\nTotal tokens:", nrow(tokens) , "\n")
cat("Unique tokens:", n_distinct(tokens$word), "\n")


## ----standard-stopwords-------------------------------------------------------
standard_sw <- get_stopwords(source = "smart", )

tokens_clean <- tokens |> 
  anti_join(standard_sw, by = "word")

cat("Tokens after standard stopword removal:", nrow(tokens_clean), "\n")


## ----inspect-freq-------------------------------------------------------------
tokens_clean |>
  count(word, sort = TRUE) |>
  slice_head(n = 30) |>
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#d7191c") +
  coord_flip() +
  labs(title = "Top 30 Words (after standard stopwords)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)


## ----domain-stopwords---------------------------------------------------------
domain_sw <- tibble(word = c(
 # News wire / attribution boilerplate
  "said", "says", "told", "according", "reported", "noted", "added",
  "stated", "announced", "mr", "ms", "inc", "corp", "llc",
  # Generic quantitative terms ##(adjust based on your research question)
  "year", "years", "percent", "million", "billion", "quarter",
  "company", "companies", "firm", "business", # could include this based on the question 
  # Consider whether these are substantive for our RQ:
  "data", "center", "centers" 
))


tokens_clean2 <- tokens_clean |>
  anti_join(domain_sw, by = "word") |>
  filter(
        !str_detect(word, "^[0-9]+$"), #remove pure digit tokens
          str_length(word) > 2         #remove very short tokens
  )

cat("Tokens after domain stopword removal:", nrow(tokens_clean2), "\n")
cat("Reduction from raw:", scales::percent(1 - nrow(tokens_clean2) / nrow(tokens)), "\n")


## ----stemming-----------------------------------------------------------------
tokens_stemmed <- tokens_clean2 |>
  mutate(stem = wordStem(word, language = "english"))

# Compare original tokens to their stems
tokens_stemmed |>
  filter(word != stem) |>
  distinct(word, stem) |>
  slice_head(n = 20) |>
  knitr::kable(caption = "Sample: original tokens vs. stems")


## ----lemmatization------------------------------------------------------------
tokens_lemmatized <- tokens_clean2 |>
  mutate(lemma = lemmatize_words(word))

# Compare
tokens_lemmatized |>
  filter(word != lemma) |>
  distinct(word, lemma) |>
  slice_head(n = 20) |>
  knitr::kable(caption = "Sample: original tokens vs. lemmas")


## ----compare-approaches-------------------------------------------------------
comparison_words <- c("operating", "operated", "operations", "operator", "operators",
                      "building", "built", "builds", "build")

tokens_clean2 |>
  filter(word %in% comparison_words) |>
  mutate(
    stem  = wordStem(word),
    lemma = lemmatize_words(word)
  ) |>
  distinct(word, stem, lemma) |>
  arrange(word) |>
  knitr::kable(caption = "Stemming vs. lemmatization comparison")


## ----set-working-tokens-------------------------------------------------------
working_tokens <- tokens_lemmatized |>
  mutate(word = lemma) |>
  select(-lemma)


## ----freq-plot----------------------------------------------------------------
top_words <- working_tokens |>
  count(word, sort = TRUE) |>
  slice_head(n = 30)

top_words |>
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#1a9641") +
  coord_flip() +
  labs(
    title = "30 Most Frequent Terms — Data Center Corpus",
    subtitle = "After stopword removal and lemmatization",
    x = NULL,
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)


## ----freq-by-source-----------------------------------------------------------
top_sources <- df |>
  count(source, sort = TRUE) |>
  slice_max(n, n = 10) |> #select rows with largest values
  pull(source) #extract source column only

working_tokens |>
  filter(source %in% top_sources) |>
  count(source, word, sort = TRUE) |>
  group_by(source) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  ggplot(aes(x = reorder_within(word, n, source), y = n, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top 10 Terms by Source", x = NULL, y = "Count") +
  theme_minimal(base_size = 10)


## ----build-dfm----------------------------------------------------------------
# Build corpus — quanteda uses character doc IDs, so id (integer) is coerced
corp <- corpus(df, docid_field = "id", text_field = "text") 

# Create a new document variable (year) from date
docvars(corp, "year") <- format(df$date, "%Y")

# Tokenize inside quanteda: remove punctuation, numbers, symbols; lowercase
toks <- tokens(corp,
               remove_punct   = TRUE,
               remove_numbers = TRUE,
               remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("en", source = "smart") ) #

# Build DFM and drop any documents with no date
dfmat <- dfm(toks)
dfmat <- dfmat[!is.na(docvars(dfmat, "year")), ]

cat("DFM dimensions:", nrow(dfmat), "documents ×", ncol(dfmat), "features\n")


## ----define-groups------------------------------------------------------------
cat("2021 articles:", sum(docvars(dfmat, "year") == "2021"), "\n")
cat("2025 articles:", sum(docvars(dfmat, "year") == "2025"), "\n")

# Collapse DFM to two rows — one per year value
dfmat_grouped <- dfm_group(dfmat, groups = year)


## ----compute-keyness----------------------------------------------------------
result <- textstat_keyness(dfmat_grouped,
                           target  = "2025",
                           measure = "lr")

textplot_keyness(result, n = 20) +
  labs(
    title = "Keyness: Data Center Coverage",
    subtitle = "Target: 2025  |  Reference: 2021  |  Statistic: Log-Likelihood (G²)"
  )


## ----stopword-sensitivity-----------------------------------------------------



## ----alternative-keyness------------------------------------------------------



## ----bigrams------------------------------------------------------------------


