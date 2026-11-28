# ============================================================
# 03_benchmark_only.R  -  Benchmark R (lexicon) sahaja, untuk RQ3
# Output: output/benchmark_R.csv, output/benchmark_R_all_runs.csv,
#         output/bench_ids_*.csv, output/machine_info.txt
# ============================================================

source("00_helpers.R")

con <- dbConnect(SQLite(), DB_PATH)
bd <- dbGetQuery(con, "SELECT ID, text_clean AS text FROM cleaned_bdstar") %>%
  filter(!is.na(text))
dbDisconnect(con)

set.seed(42)          # seed sama -> sampel sama dengan run sebelum ni
sizes <- c(10000, 100000, 500000)
reps  <- 3
results <- list()

for (s in sizes) {
  s_txt <- format(s, scientific = FALSE, big.mark = "")
  ids <- sample(bd$ID, s)
  write_csv(tibble(ID = ids), paste0("output/bench_ids_", s_txt, ".csv"))
  sub <- bd %>% filter(ID %in% ids) %>% rename(id = ID)

  for (r in 1:reps) {
    invisible(gc(reset = TRUE))
    t <- system.time({ res <- score_posts(sub) })
    mem <- gc()
    results[[length(results) + 1]] <- tibble(
      tool = "R (tidytext)", n_rows = s, run = r,
      seconds = round(t[["elapsed"]], 2),
      peak_mem_mb = round(sum(mem[, ncol(mem)]), 1)
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

# Simpan maklumat mesin untuk laporan
writeLines(c(paste("R version:", R.version.string),
             paste("OS:", Sys.info()[["sysname"]], Sys.info()[["release"]]),
             paste("Machine:", Sys.info()[["nodename"]]),
             paste("Date:", format(Sys.time()))),
           "output/machine_info.txt")

cat("\nDONE.\n")
