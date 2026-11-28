# ============================================================
# 02_lexicon_sentiment.R  -  Lexicon sentiment (tidytext, Bing)
#   A. Sentimen semua post (Twitter vs HN, ikut minggu)
#   B. Prediction untuk gold set  -> RQ1
#   C. Benchmark masa & memori 10K / 100K / 500K  -> RQ3
# ============================================================

source("00_helpers.R")

posts <- readRDS("output/clean_posts.rds") %>% filter(!is.na(day))

# ---- A. Sentimen semua post ----
scored <- score_posts(posts) %>%
  left_join(posts %>% select(id, platform, day), by = "id")

overall <- scored %>%
  count(platform, label) %>%
  group_by(platform) %>%
  mutate(prop = round(n / sum(n), 3)) %>%
  ungroup()
print(overall)
write_csv(overall, "output/lexicon_overall_sentiment.csv")

weekly <- scored %>%
  mutate(week = floor_date(day, "week", week_start = 1)) %>%
  count(platform, week, label) %>%
  group_by(platform, week) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
write_csv(weekly, "output/lexicon_weekly_sentiment.csv")

p4 <- weekly %>%
  filter(label != "neutral") %>%
  ggplot(aes(week, prop, colour = label)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~platform, ncol = 1) +
  scale_y_continuous(labels = percent) +
  scale_colour_manual(values = c(negative = "#c0392b", positive = "#2a9d8f")) +
  labs(title = "Weekly share of positive and negative posts (Bing lexicon)",
       x = NULL, y = "Share of posts", colour = NULL) +
  theme_minimal(base_size = 12)
ggsave("output/lexicon_weekly_sentiment.png", p4, width = 8, height = 5, dpi = 150)

# ---- B. Prediction untuk gold set ----
con <- dbConnect(SQLite(), DB_PATH)
gold <- dbGetQuery(con, "SELECT gold_id, text FROM gold_set")
dbDisconnect(con)

gold_pred <- gold %>%
  transmute(id = gold_id, text = clean_text(text)) %>%
  score_posts() %>%
  rename(gold_id = id, lexicon_score = score, predicted_label = label)
write_csv(gold_pred, "output/gold_predictions_lexicon_R.csv")
print(count(gold_pred, predicted_label))

# ---- C. Benchmark 10K / 100K / 500K ----
# Guna cleaned_bdstar (data training Spark) supaya sampel sama dengan Spark.
# ID sampel disimpan dalam CSV
# Masa yang diukur: tokenize + join lexicon + kira skor.
con <- dbConnect(SQLite(), DB_PATH)
bd <- dbGetQuery(con, "SELECT ID, text_clean AS text FROM cleaned_bdstar") %>%
  filter(!is.na(text))
dbDisconnect(con)

set.seed(42)
sizes <- c(10000, 100000, 500000)
reps  <- 3      # ulang 3 kali, ambil median
results <- list()

for (s in sizes) {
  s_txt <- format(s, scientific = FALSE, big.mark = "")
  ids <- sample(bd$ID, s)
  write_csv(tibble(ID = ids), paste0("output/bench_ids_", s_txt, ".csv"))
  sub <- bd %>% filter(ID %in% ids) %>% rename(id = ID)

  for (r in 1:reps) {
    invisible(gc(reset = TRUE))
    t <- system.time({
      res <- score_posts(sub)
    })
    mem <- gc()
    results[[length(results) + 1]] <- tibble(
      tool = "R (tidytext)", n_rows = s, run = r,
      seconds = round(t[["elapsed"]], 2),
      peak_mem_mb = round(sum(mem[, ncol(mem)]), 1)   # "max used" memori R (Mb)
    )
    cat("Siap:", s_txt, "row, run", r, "-", round(t[["elapsed"]], 2), "saat\n")
    rm(res)
  }
  rm(sub)
}

bench_runs <- bind_rows(results)
write_csv(bench_runs, "output/benchmark_R_all_runs.csv")

bench <- bench_runs %>%
  group_by(tool, n_rows) %>%
  summarise(median_seconds = median(seconds),
            median_peak_mem_mb = median(peak_mem_mb), .groups = "drop")
print(bench)
write_csv(bench, "output/benchmark_R.csv")

cat("\nDONE.\n")
