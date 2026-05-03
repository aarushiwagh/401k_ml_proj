# =============================================================================
# Step 1: Data Preparation
# 401(k) Project - Effects of 401(k) Participation on Wealth
# =============================================================================

# Install required packages if not already installed
#install.packages(c("tidyverse", "haven", "fastDummies"))

library(tidyverse)

# ── 1. Load Data ──────────────────────────────────────────────────────────────
df <- read.csv("401k.csv")   # adjust path if needed

cat("=============================================================\n")
cat("DATASET OVERVIEW\n")
cat("=============================================================\n")
cat("Rows:          ", nrow(df), "\n")
cat("Columns:       ", ncol(df), "\n")
cat("Column names:  ", paste(names(df), collapse = ", "), "\n")

# ── 2. Check for Missing Values ───────────────────────────────────────────────
cat("\n=============================================================\n")
cat("MISSING VALUES PER COLUMN\n")
cat("=============================================================\n")
missing_counts <- colSums(is.na(df))
print(missing_counts)
cat("Total missing values:", sum(missing_counts), "\n")

# ── 3. Create Income Category Variable (matching the paper) ───────────────────
# Paper uses: <$10K, $10-20K, $20-30K, $30-40K, $40-50K, $50-75K, >$75K
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
    # Set factor with ordered levels so tables display in correct order
    inc_cat = factor(inc_cat,
                     levels = c("<10K","10-20K","20-30K","30-40K",
                                "40-50K","50-75K",">75K"))
  )

cat("\n=============================================================\n")
cat("INCOME CATEGORY DISTRIBUTION\n")
cat("=============================================================\n")
print(table(df$inc_cat))

# Income category dummy variables (base = "<10K" dropped in regressions)
df <- df %>%
  mutate(
    inc_10_20  = as.integer(inc_cat == "10-20K"),
    inc_20_30  = as.integer(inc_cat == "20-30K"),
    inc_30_40  = as.integer(inc_cat == "30-40K"),
    inc_40_50  = as.integer(inc_cat == "40-50K"),
    inc_50_75  = as.integer(inc_cat == "50-75K"),
    inc_gt75   = as.integer(inc_cat == ">75K")
  )

# ── 4. Create Education Dummies ───────────────────────────────────────────────
# Paper groups: <12 yrs (nohs), 12 yrs (hs), 13-15 yrs (smcol), 16+ yrs (col)
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

# ── 5. Create Age Category Dummies (matching the paper) ──────────────────────
# Paper groups: <30, 30-35, 36-44, 45-54, 55+
df <- df %>%
  mutate(
    age_lt30    = as.integer(age < 30),
    age_30_35   = as.integer(age >= 30 & age <= 35),
    age_36_44   = as.integer(age >= 36 & age <= 44),
    age_45_54   = as.integer(age >= 45 & age <= 54),
    age_55plus  = as.integer(age >= 55)
    # age_55plus dropped as base category in regressions
  )

# ── 6. Define Variable Groups ─────────────────────────────────────────────────

# Outcome variables (three wealth measures from the paper)
outcomes <- c("net_tfa", "net_nifa", "tw")

# Treatment and instrument
treatment  <- "p401"   # participation in 401(k)
instrument <- "e401"   # eligibility for 401(k)  -- used as IV

# Full covariate list for regressions (matching paper's specification)
# Base categories dropped: nohs (educ), age_55plus (age), inc_<10K (income)
covariates_full <- c(
  # Continuous
  "inc", "fsize",
  # Binary household/financial characteristics
  "marr", "twoearn", "db", "pira", "hown",
  # Education dummies (nohs is base/dropped)
  "hs", "smcol", "col",
  # Age dummies (age_55plus is base/dropped)
  "age_lt30", "age_30_35", "age_36_44", "age_45_54",
  # Income dummies (<10K is base/dropped)
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

# ── 7. Descriptive Statistics on Outcome Variables ────────────────────────────
cat("\n=============================================================\n")
cat("DESCRIPTIVE STATISTICS — OUTCOME VARIABLES\n")
cat("=============================================================\n")
summary_stats <- df %>%
  select(all_of(outcomes)) %>%
  summary()
print(summary_stats)

# ── 8. Participation & Eligibility Rates ──────────────────────────────────────
cat("\n=============================================================\n")
cat("PARTICIPATION & ELIGIBILITY RATES\n")
cat("=============================================================\n")
cat(sprintf("Eligible for 401(k):           %.1f%% (%d obs)\n",
            mean(df$e401) * 100, sum(df$e401)))
cat(sprintf("Participates in 401(k):        %.1f%% (%d obs)\n",
            mean(df$p401) * 100, sum(df$p401)))
cat(sprintf("Participation rate (eligible): %.1f%%\n",
            mean(df$p401[df$e401 == 1]) * 100))

# ── 9. Means by Participation Status (mirrors Table 1 of paper) ───────────────
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

# Add full-sample mean
table1$`Full Sample` <- colMeans(df[compare_vars])

print(table1 %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

# ── 10. Means by Eligibility Status ───────────────────────────────────────────
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

# ── 11. Save Cleaned Dataset & Config ─────────────────────────────────────────
write.csv(df, "401k_clean.csv", row.names = FALSE)

# Save variable lists as an R object for use in later steps
save(outcomes, treatment, instrument, covariates_full,
     file = "project_config.RData")

cat("\n✓ Cleaned dataset saved to:  401k_clean.csv\n")
cat(  "✓ Variable config saved to:  project_config.RData\n")
cat(sprintf("  Final shape: %d rows x %d columns\n", nrow(df), ncol(df)))
cat("\nStep 1 complete. Ready for Step 2 (Summary Statistics Table).\n")