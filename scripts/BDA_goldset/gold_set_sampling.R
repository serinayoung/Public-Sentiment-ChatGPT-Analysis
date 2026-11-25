# =============================================================
# KD44103 Big Data Analytics
# Gold set sampling: pick 1,000 (+50 spare) ChatGPT tweets for manual labelling
# =============================================================

library(dplyr)
library(readr)
library(stringr)
library(lubridate)

set.seed(13)  # same seed = same sample every run (reproducible)

# ---- 1. Settings: CHANGE THESE to match your file ----------------
INPUT    <- "Twitter Jan Mar.csv"
TEXT_COL <- "content"
DATE_COL <- "date"
OUT_DIR  <- "gold_set"

PER_MONTH <- 350                  # 3 months x 350 = 1,050 (1,000 + 50 spare)
MEMBERS   <- c("Esther", "Nurshurayani", "Nurnisriza", "Erin")

# ---- 2. Load ---------------------------------------------------------
tweets <- read_csv(INPUT, show_col_types = FALSE)
cat("Columns in file:\n"); print(names(tweets))
stopifnot(TEXT_COL %in% names(tweets), DATE_COL %in% names(tweets))

# ---- 3. Light cleaning (only to pick good tweets for labelling) ----
pool <- tweets %>%
  transmute(text = as.character(.data[[TEXT_COL]]),
            date = as.Date(substr(as.character(.data[[DATE_COL]]), 1, 10))) %>%
  filter(!is.na(text), !is.na(date)) %>%
  mutate(text = str_squish(text)) %>%
  filter(!str_detect(text, "^RT @"),            # drop retweets
         str_count(text, "\\S+") >= 5) %>%      # drop very short tweets
  distinct(text, .keep_all = TRUE) %>%          # drop duplicate text
  mutate(month = format(date, "%Y-%m"))

cat("\nTweets available per month after cleaning:\n"); print(count(pool, month))

# ---- 4. Stratified sample: equal tweets per month ------------------
gold <- pool %>%
  group_by(month) %>%
  slice_sample(n = PER_MONTH) %>%
  ungroup() %>%
  slice_sample(prop = 1) %>%                    # shuffle so months are mixed
  mutate(gold_id = sprintf("G%04d", row_number()))

n <- nrow(gold)
gold <- gold %>%
  mutate(group = case_when(
    row_number() <= 200  ~ "shared",            # labelled by ALL 4 (Fleiss' Kappa)
    row_number() <= 400  ~ MEMBERS[1],
    row_number() <= 600  ~ MEMBERS[2],
    row_number() <= 800  ~ MEMBERS[3],
    row_number() <= 1000 ~ MEMBERS[4],
    TRUE                 ~ "spare"              # replacements for skipped tweets
  ))

# ---- 5. Save files -----------------------------------------------------
dir.create(OUT_DIR, showWarnings = FALSE)

# Master file (keep this; give the gold_id + text list to Esther so she
# removes these tweets from the training data)
write_csv(gold %>% select(gold_id, text, date, month, group),
          file.path(OUT_DIR, "gold_set_master.csv"))

# One labelling file per member: 200 shared + 200 own
for (m in MEMBERS) {
  gold %>%
    filter(group %in% c("shared", m)) %>%
    arrange(desc(group == "shared"), gold_id) %>%
    transmute(gold_id, part = ifelse(group == "shared", "shared", "own"),
              text, label = "", note = "") %>%
    write_csv(file.path(OUT_DIR, paste0("label_", m, ".csv")))
}

# Spare tweets (use when someone marks a tweet as 'skip')
write_csv(gold %>% filter(group == "spare") %>% select(gold_id, text),
          file.path(OUT_DIR, "spare_tweets.csv"))

cat("\nDone:", n, "tweets sampled.\n"); print(count(gold, group))
cat("Files saved in:", normalizePath(OUT_DIR), "\n")
