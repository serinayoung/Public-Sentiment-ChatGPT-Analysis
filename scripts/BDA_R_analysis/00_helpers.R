# ============================================================
# 00_helpers.R  -  fungsi kongsi (dipanggil oleh skrip 01 & 02)
# ============================================================

suppressPackageStartupMessages({
  library(DBI); library(RSQLite)
  library(dplyr); library(tidyr); library(stringr); library(readr)
  library(ggplot2); library(lubridate); library(scales); library(tidytext)
})

DB_PATH     <- "bda_group_assignment_cleaned.db"   # versi cleaned
RAW_DB_PATH <- "bda_group_assignment.db"           # versi asal (untuk kira row asal saja)
dir.create("output", showWarnings = FALSE)

# Bersihkan teks: buang tag HTML (Hacker News), tukar HTML entity,
# buang URL dan @mention, kemas ruang kosong.
clean_text <- function(x) {
  x %>%
    str_replace_all("<[^>]+>", " ") %>%
    str_replace_all("&#x27;|&#39;", "'") %>%
    str_replace_all("&quot;", "\"") %>%
    str_replace_all("&#x2F;", "/") %>%
    str_replace_all("&gt;", ">") %>%
    str_replace_all("&lt;", "<") %>%
    str_replace_all("&amp;", "&") %>%
    str_replace_all("https?://\\S+", " ") %>%
    str_replace_all("@\\w+", " ") %>%
    str_squish()
}

# Lexicon Bing
# distinct() sebab ada beberapa perkataan tersenarai dua kali.
bing <- get_sentiments("bing") %>% distinct(word, .keep_all = TRUE)

# Skor sentimen setiap post: +1 untuk perkataan positif, -1 untuk negatif.
# Jumlah > 0 = positive, < 0 = negative, 0 = neutral.
# df mesti ada column: id, text
score_posts <- function(df) {
  scores <- df %>%
    select(id, text) %>%
    unnest_tokens(word, text) %>%
    inner_join(bing, by = "word") %>%
    mutate(v = ifelse(sentiment == "positive", 1L, -1L)) %>%
    group_by(id) %>%
    summarise(score = sum(v), .groups = "drop")

  df %>%
    select(id) %>%
    left_join(scores, by = "id") %>%
    mutate(score = coalesce(score, 0L),
           label = case_when(score > 0 ~ "positive",
                             score < 0 ~ "negative",
                             TRUE      ~ "neutral"))
}
