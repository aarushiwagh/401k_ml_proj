library(tidyverse)

df <- read.csv("401k.csv")

cat("=============================================================\n")
cat("DATASET OVERVIEW\n")
cat("=============================================================\n")
cat("Rows:          ", nrow(df), "\n")
cat("Columns:       ", ncol(df), "\n")
cat("Column names:  ", paste(names(df), collapse = ", "), "\n")

cat("\n=============================================================\n")
cat("MISSING VALUES PER COLUMN\n")
cat("=============================================================\n")
missing_counts <- colSums(is.na(df))
print(missing_counts)
cat("Total missing values:", sum(missing_counts), "\n")

df <- df %>%
  mutate(
    inc_cat = case_when(
      inc <  10000                  ~ "<10K",
      inc >= 10000 & inc <  20000   ~ "10-20K",
      inc >= 20000 & inc <  30000   ~ "20-30K",
      inc >= 30000 & inc <  40000   ~ "30-40K",
      inc >= 40000 & inc <  50000   ~ "40-50K",
      inc >= 50000 & inc <= 75000   ~ "50-75K",
      inc >  75000                  ~ ">75K"
    ),
    inc_cat = factor(inc_cat,
                     levels = c("<10K","10-20K","20-30K","30-40K",
                                "40-50K","50-75K",">75K"))
  )

cat("\n=============================================================\n")
cat("INCOME CATEGORY DISTRIBUTION\n")
cat("=============================================================\n")
print(table(df$inc_cat))

df <- df %>%
  mutate(
    inc_10_20  = as.integer(inc_cat == "10-20K"),
    inc_20_30  = as.integer(inc_cat == "20-30K"),
    inc_30_40  = as.integer(inc_cat == "30-40K"),
    inc_40_50  = as.integer(inc_cat == "40-50K"),
    inc_50_75  = as.integer(inc_cat == "50-75K"),
    inc_gt75   = as.integer(inc_cat == ">75K")
  )

df <- df %>%
  mutate(
    nohs  = as.integer(educ < 12),
    hs    = as.integer(educ == 12),
    smcol = as.integer(educ >= 13 & educ <= 15),
    col   = as.integer(educ >= 16)
  )

cat("\n=============================================================\n")
cat("EDUCATION CATEGORY DISTRIBUTION\n")
cat("=============================================================\n")
cat("No high school (<12 yrs): ", sum(df$nohs),  "\n")
cat("High school (12 yrs):     ", sum(df$hs),    "\n")
cat("Some college (13-15 yrs): ", sum(df$smcol), "\n")
cat("College (16+ yrs):        ", sum(df$col),   "\n")

df <- df %>%
  mutate(
    age_lt30    = as.integer(age < 30),
    age_30_35   = as.integer(age >= 30 & age <= 35),
    age_36_44   = as.integer(age >= 36 & age <= 44),
    age_45_54   = as.integer(age >= 45 & age <= 54),
    age_55plus  = as.integer(age >= 55)
  )

outcomes <- c("net_tfa", "net_nifa", "tw")

treatment  <- "p401"
instrument <- "e401"

covariates_full <- c(
  "inc", "fsize",
  "marr", "twoearn", "db", "pira", "hown",
  "hs", "smcol", "col",
  "age_lt30", "age_30_35", "age_36_44", "age_45_54",
  "inc_10_20", "inc_20_30", "inc_30_40",
  "inc_40_50", "inc_50_75", "inc_gt75"
)

cat("\n=============================================================\n")
cat("VARIABLE GROUPS\n")
cat("=============================================================\n")
cat("Outcomes:           ", paste(outcomes, collapse = ", "), "\n")
cat("Treatment:          ", treatment, "\n")
cat("Instrument (IV):    ", instrument, "\n")
cat("# Full covariates:  ", length(covariates_full), "\n")

cat("\n=============================================================\n")
cat("DESCRIPTIVE STATISTICS — OUTCOME VARIABLES\n")
cat("=============================================================\n")
summary_stats <- df %>%
  select(all_of(outcomes)) %>%
  summary()
print(summary_stats)

cat("\n=============================================================\n")
cat("PARTICIPATION & ELIGIBILITY RATES\n")
cat("=============================================================\n")
cat(sprintf("Eligible for 401(k):           %.1f%% (%d obs)\n",
            mean(df$e401) * 100, sum(df$e401)))
cat(sprintf("Participates in 401(k):        %.1f%% (%d obs)\n",
            mean(df$p401) * 100, sum(df$p401)))
cat(sprintf("Participation rate (eligible): %.1f%%\n",
            mean(df$p401[df$e401 == 1]) * 100))

cat("\n=============================================================\n")
cat("MEANS BY 401(k) PARTICIPATION STATUS\n")
cat("=============================================================\n")

compare_vars <- c("net_tfa", "net_nifa", "tw",
                  "inc", "age", "fsize", "marr", "pira", "db", "hown")

table1 <- df %>%
  group_by(p401) %>%
  summarise(across(all_of(compare_vars), mean), .groups = "drop") %>%
  mutate(p401 = ifelse(p401 == 1, "Participants", "Non-Participants")) %>%
  pivot_longer(-p401, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = p401, values_from = Mean)

table1$`Full Sample` <- colMeans(df[compare_vars])

print(table1 %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

cat("\n=============================================================\n")
cat("MEANS BY 401(k) ELIGIBILITY STATUS\n")
cat("=============================================================\n")

table1b <- df %>%
  group_by(e401) %>%
  summarise(across(all_of(compare_vars), mean), .groups = "drop") %>%
  mutate(e401 = ifelse(e401 == 1, "Eligible", "Not Eligible")) %>%
  pivot_longer(-e401, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = e401, values_from = Mean)

print(table1b %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

write.csv(df, "401k_clean.csv", row.names = FALSE)

save(outcomes, treatment, instrument, covariates_full,
     file = "project_config.RData")

cat("\n✓ Cleaned dataset saved to:  401k_clean.csv\n")
cat(  "✓ Variable config saved to:  project_config.RData\n")
cat(sprintf("  Final shape: %d rows x %d columns\n", nrow(df), ncol(df)))
cat("\nStep 1 complete. Ready for Step 2 (Summary Statistics Table).\n")
