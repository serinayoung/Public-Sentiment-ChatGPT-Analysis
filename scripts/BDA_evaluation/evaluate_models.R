# =============================================================
# KD44103 Big Data Analytics - Group 13
# Model evaluation on the human-labelled gold set
#   - Accuracy, per-class precision / recall / F1, macro-F1
#   - Confusion matrix for each model
#   - Comparison plots (R lexicon vs Spark MLlib)
# =============================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# ---- 1. Settings ----------------------------------------------------
GOLD_FILE <- "gold_set_final.csv"           # columns: gold_id, text, label
PRED_FILES <- c(                            # columns: gold_id, pred
  "R lexicon (tidytext)"         = "pred_r_lexicon.csv",
  "Spark MLlib (TF-IDF + LogReg)" = "pred_spark.csv"
)
OUT_DIR <- "evaluation_output"
CLASSES <- c("negative", "neutral", "positive")

dir.create(OUT_DIR, showWarnings = FALSE)

clean_label <- function(x) tolower(trimws(as.character(x)))

# ---- 2. Load gold set -------------------------------------------------
gold <- read_csv(GOLD_FILE, show_col_types = FALSE) %>%
  mutate(label = clean_label(label)) %>%
  filter(label %in% CLASSES)                 # drops any leftover 'skip'

cat("Gold set:", nrow(gold), "tweets\n"); print(count(gold, label))

# ---- 3. Metric functions ----------------------------------------------
# cm: rows = true CLASSES; columns = CLASSES plus "missing" (no/invalid prediction)
metrics_from_cm <- function(cm) {
  per_class <- lapply(CLASSES, function(k) {
    tp <- cm[k, k]
    fp <- sum(cm[, k]) - tp
    fn <- sum(cm[k, ]) - tp
    precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall    <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1        <- ifelse(precision + recall == 0, 0,
                        2 * precision * recall / (precision + recall))
    tibble(class = k, precision = precision, recall = recall, f1 = f1,
           support = sum(cm[k, ]))
  }) %>% bind_rows()

  list(per_class = per_class,
       accuracy  = sum(diag(cm[, CLASSES, drop = FALSE])) / sum(cm),
       macro_f1  = mean(per_class$f1))
}

# ---- 4. Evaluate each model ---------------------------------------------
summary_rows <- list(); per_class_rows <- list(); cm_rows <- list()

for (model in names(PRED_FILES)) {
  f <- PRED_FILES[[model]]
  if (!file.exists(f)) { cat("\n[SKIP] File not found for", model, ":", f, "\n"); next }

  pred <- read_csv(f, show_col_types = FALSE) %>%
    mutate(pred = clean_label(pred)) %>%
    select(gold_id, pred)

  joined <- gold %>% left_join(pred, by = "gold_id")
  n_missing <- sum(is.na(joined$pred))
  n_bad     <- sum(!is.na(joined$pred) & !joined$pred %in% CLASSES)
  if (n_missing > 0) cat("\n[WARNING]", model, "has no prediction for", n_missing, "gold tweets (counted as wrong)\n")
  if (n_bad > 0)     cat("\n[WARNING]", model, "has", n_bad, "predictions that are not negative/neutral/positive (counted as wrong)\n")

  # Missing / invalid predictions count as wrong: put them in a dummy level
  joined$pred[is.na(joined$pred) | !joined$pred %in% CLASSES] <- "missing"
  cm <- table(Actual    = factor(joined$label, levels = CLASSES),
              Predicted = factor(joined$pred,  levels = c(CLASSES, "missing")))
  cm3 <- cm[, CLASSES, drop = FALSE]           # 3x3 matrix for printing / plotting
  m <- metrics_from_cm(cm)                       # metrics count missing as wrong

  cat("\n==========", model, "==========\n")
  cat(sprintf("Accuracy: %.4f   Macro-F1: %.4f\n", m$accuracy, m$macro_f1))
  print(cm3)
  print(m$per_class %>% mutate(across(precision:f1, ~ round(.x, 4))))

  summary_rows[[model]]   <- tibble(model = model, n = nrow(joined),
                                    accuracy = m$accuracy, macro_f1 = m$macro_f1)
  per_class_rows[[model]] <- m$per_class %>% mutate(model = model, .before = 1)
  cm_rows[[model]]        <- as.data.frame(cm3) %>% mutate(model = model)
}

if (length(summary_rows) == 0) stop("No prediction files found. Check PRED_FILES.")

summary_tbl   <- bind_rows(summary_rows)
per_class_tbl <- bind_rows(per_class_rows)
cm_tbl        <- bind_rows(cm_rows)

write_csv(summary_tbl,   file.path(OUT_DIR, "metrics_summary.csv"))
write_csv(per_class_tbl, file.path(OUT_DIR, "metrics_per_class.csv"))
write_csv(cm_tbl,        file.path(OUT_DIR, "confusion_matrices.csv"))

# ---- 5. Plots -------------------------------------------------------------
# (a) Accuracy vs macro-F1 per model
p1 <- summary_tbl %>%
  pivot_longer(c(accuracy, macro_f1), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, accuracy = "Accuracy", macro_f1 = "Macro-F1")) %>%
  ggplot(aes(x = metric, y = value, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", value)),
            position = position_dodge(width = 0.75), vjust = -0.4, size = 3.5) +
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  scale_fill_manual(values = c("#378ADD", "#D85A30")) +
  labs(title = "Model performance on the human-labelled gold set",
       subtitle = paste0("n = ", summary_tbl$n[1], " tweets"),
       x = NULL, y = "Score", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())
ggsave(file.path(OUT_DIR, "plot_model_comparison.png"), p1, width = 7, height = 5, dpi = 300)

# (b) Per-class F1 per model
p2 <- per_class_tbl %>%
  ggplot(aes(x = class, y = f1, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", f1)),
            position = position_dodge(width = 0.75), vjust = -0.4, size = 3.5) +
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  scale_fill_manual(values = c("#378ADD", "#D85A30")) +
  labs(title = "F1-score by sentiment class", x = NULL, y = "F1-score", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())
ggsave(file.path(OUT_DIR, "plot_f1_by_class.png"), p2, width = 7, height = 5, dpi = 300)

# (c) Confusion matrix heatmaps (row-normalised = recall per class)
p3 <- cm_tbl %>%
  group_by(model, Actual) %>%
  mutate(row_pct = Freq / max(sum(Freq), 1)) %>%
  ungroup() %>%
  ggplot(aes(x = Predicted, y = Actual, fill = row_pct)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = paste0(Freq, "\n(", round(row_pct * 100), "%)")), size = 3.3) +
  scale_fill_gradient(low = "#F1EFE8", high = "#185FA5", limits = c(0, 1),
                      labels = scales::percent) +
  scale_y_discrete(limits = rev(CLASSES)) +
  facet_wrap(~ model) +
  labs(title = "Confusion matrices (row % = share of each true class)",
       x = "Predicted label", y = "True label (gold set)", fill = "Row %") +
  theme_minimal(base_size = 12)
ggsave(file.path(OUT_DIR, "plot_confusion_matrices.png"), p3, width = 10, height = 4.8, dpi = 300)

cat("\nDone. Results and plots saved in:", normalizePath(OUT_DIR), "\n")
print(summary_tbl %>% mutate(across(c(accuracy, macro_f1), ~ round(.x, 4))))
