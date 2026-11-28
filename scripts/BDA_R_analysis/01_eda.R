# ============================================================
# 01_eda.R  -  3 plot EDA (Twitter vs Hacker News)
# Guna data cleaned (cleaned_tweets, cleaned_hn, column text_clean)
# Output masuk folder "output/"
# ============================================================

source("00_helpers.R")

# ---- 1. Ambil data dari SQL (versi cleaned) ----
con <- dbConnect(SQLite(), DB_PATH)
# CAST id ke TEXT sebab ID tweet terlalu panjang untuk nombor biasa dalam R
tw <- dbGetQuery(con, "SELECT CAST(id AS TEXT) AS id, date, text_clean FROM cleaned_tweets")
hn <- dbGetQuery(con, "SELECT CAST(objectID AS TEXT) AS id, created_at, text_clean FROM cleaned_hn")
dbDisconnect(con)

n_tw_clean_db <- nrow(tw); n_hn_clean_db <- nrow(hn)

# Bilangan row asal (data sebelum clean)
n_tw_raw <- NA; n_hn_raw <- NA
if (file.exists(RAW_DB_PATH)) {
  con <- dbConnect(SQLite(), RAW_DB_PATH)
  n_tw_raw <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tweets_raw")$n
  n_hn_raw <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM hn_raw")$n
  dbDisconnect(con)
}


tw <- tw %>%
  transmute(id = paste0("tw_", id), platform = "Twitter",
            day = as.Date(substr(date, 1, 10)), text = text_clean)
hn <- hn %>%
  transmute(id = paste0("hn_", id), platform = "Hacker News",
            day = as.Date(substr(created_at, 1, 10)), text = text_clean)

posts <- bind_rows(tw, hn) %>%
  filter(!is.na(text), str_squish(text) != "", !is.na(day)) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(n_words = str_count(text, "\\S+"))

cleaning_log <- tibble(
  platform = c("Twitter", "Hacker News"),
  raw_rows = c(n_tw_raw, n_hn_raw),
  after_esther_cleaning = c(n_tw_clean_db, n_hn_clean_db),
  used_in_analysis = c(sum(posts$platform == "Twitter"), sum(posts$platform == "Hacker News"))
)
print(cleaning_log)
write_csv(cleaning_log, "output/cleaning_log.csv")

saveRDS(posts, "output/clean_posts.rds")    # dipakai oleh skrip 02

posts %>% group_by(platform) %>%
  summarise(dari = min(day), hingga = max(day)) %>% print()

# ---- PLOT 1: Isipadu post harian ----
p1 <- posts %>%
  count(platform, day) %>%
  ggplot(aes(day, n)) +
  geom_line(colour = "#2a6f97", linewidth = 0.6) +
  facet_wrap(~platform, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily volume of ChatGPT posts, Jan to Mar 2023",
       x = NULL, y = "Posts per day") +
  theme_minimal(base_size = 12)
ggsave("output/eda1_daily_volume.png", p1, width = 8, height = 5, dpi = 150)

top_days <- posts %>% count(platform, day) %>%
  group_by(platform) %>% slice_max(n, n = 5) %>% ungroup()
print(top_days)
write_csv(top_days, "output/eda1_top_days.csv")

# ---- PLOT 2: Panjang post (bilangan perkataan) ----
length_summary <- posts %>%
  group_by(platform) %>%
  summarise(posts = n(),
            median_words = median(n_words),
            mean_words = round(mean(n_words), 1),
            p90_words = quantile(n_words, 0.9))
print(length_summary)
write_csv(length_summary, "output/eda2_length_summary.csv")

p2 <- ggplot(posts, aes(platform, n_words)) +
  geom_boxplot(fill = "#a9d6e5", outlier.alpha = 0.05) +
  scale_y_log10(labels = comma) +
  labs(title = "Post length by platform",
       x = NULL, y = "Words per post (log scale)") +
  theme_minimal(base_size = 12)
ggsave("output/eda2_post_length.png", p2, width = 7, height = 5, dpi = 150)

# ---- PLOT 3: 15 perkataan paling kerap setiap platform ----
query_words <- tibble(word = c("chatgpt", "chat", "gpt", "openai", "amp"))

top_words <- posts %>%
  select(platform, text) %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(query_words, by = "word") %>%
  count(platform, word, sort = TRUE) %>%
  group_by(platform) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup()
write_csv(top_words, "output/eda3_top_words.csv")

p3 <- top_words %>%
  mutate(word = reorder_within(word, n, platform)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#2a6f97") +
  scale_y_reordered() +
  scale_x_continuous(labels = comma) +
  facet_wrap(~platform, scales = "free") +
  labs(title = "Most frequent words (stop words and query terms removed)",
       x = "Count", y = NULL) +
  theme_minimal(base_size = 12)
ggsave("output/eda3_top_words.png", p3, width = 9, height = 5, dpi = 150)

cat("\nDONE. plot and CSV saved in output/ folder.\n")
