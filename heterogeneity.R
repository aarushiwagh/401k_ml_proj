# =============================================================================
# Step 5: Heterogeneity Analysis
# 401(k) Project - Effects of 401(k) Participation on Wealth
#
# Uses individual-level treatment effects (tau_hat) from the Instrumental
# Forest in Step 4 to study HOW the treatment effect varies across:
#   (1) Income groups
#   (2) Education groups
#   (3) Age groups
# Also compares ML heterogeneity results to the 2SLS by-income results
# from Step 3 to triangulate findings.
# =============================================================================

library(tidyverse)

# ── 1. Load test set predictions from Step 4 ──────────────────────────────────
# This file was saved to your working directory at the end of Step 4
preds <- read.csv("test_set_predictions.csv")

cat(sprintf("Test set loaded: %d observations\n", nrow(preds)))
cat(sprintf("Columns: %s\n", paste(names(preds), collapse=", ")))

# Rebuild clean factor labels for grouping variables
preds <- preds %>%
  mutate(
    # Income group (readable labels, correct order)
    inc_group = case_when(
      inc <  10000                 ~ "<$10K",
      inc >= 10000 & inc < 20000   ~ "$10-20K",
      inc >= 20000 & inc < 30000   ~ "$20-30K",
      inc >= 30000 & inc < 40000   ~ "$30-40K",
      inc >= 40000 & inc < 50000   ~ "$40-50K",
      inc >= 50000 & inc <= 75000  ~ "$50-75K",
      inc >  75000                 ~ ">$75K"
    ),
    inc_group = factor(inc_group,
                       levels = c("<$10K","$10-20K","$20-30K","$30-40K",
                                  "$40-50K","$50-75K",">$75K")),

    # Education group
    educ_group = case_when(
      educ < 12                    ~ "No High School",
      educ == 12                   ~ "High School",
      educ >= 13 & educ <= 15      ~ "Some College",
      educ >= 16                   ~ "College+"
    ),
    educ_group = factor(educ_group,
                        levels = c("No High School","High School",
                                   "Some College","College+")),

    # Age group
    age_group = case_when(
      age < 30                     ~ "<30",
      age >= 30 & age <= 35        ~ "30-35",
      age >= 36 & age <= 44        ~ "36-44",
      age >= 45 & age <= 54        ~ "45-54",
      age >= 55                    ~ "55+"
    ),
    age_group = factor(age_group,
                       levels = c("<30","30-35","36-44","45-54","55+"))
  )

# ── 2. Helper: summarise treatment effects by group ───────────────────────────
te_by_group <- function(data, group_var, tau_var) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      n        = n(),
      mean_tau = mean(.data[[tau_var]], na.rm=TRUE),
      se_tau   = sd(.data[[tau_var]],   na.rm=TRUE) / sqrt(n()),
      ci_lo    = mean_tau - 1.96 * se_tau,
      ci_hi    = mean_tau + 1.96 * se_tau,
      pct_pos  = mean(.data[[tau_var]] > 0, na.rm=TRUE),
      .groups  = "drop"
    ) %>%
    rename(group = all_of(group_var))
}

# ── 3. Compute treatment effects by each grouping variable ────────────────────
te_inc_nfa  <- te_by_group(preds, "inc_group",  "tau_hat_nfa")
te_educ_nfa <- te_by_group(preds, "educ_group", "tau_hat_nfa")
te_age_nfa  <- te_by_group(preds, "age_group",  "tau_hat_nfa")
te_inc_tw   <- te_by_group(preds, "inc_group",  "tau_hat_tw")
te_educ_tw  <- te_by_group(preds, "educ_group", "tau_hat_tw")

# ── 4. Print heterogeneity tables ─────────────────────────────────────────────
print_te_table <- function(te_df, title) {
  cat("\n", strrep("-", 65), "\n", sep="")
  cat(title, "\n")
  cat(strrep("-", 65), "\n", sep="")
  cat(sprintf("%-18s  %5s  %9s  %9s  %9s  %6s\n",
              "Group", "N", "Mean TE ($)", "CI Lo ($)", "CI Hi ($)", "% Pos"))
  cat(strrep("-", 65), "\n", sep="")
  for (i in seq_len(nrow(te_df))) {
    cat(sprintf("%-18s  %5d  %9s  %9s  %9s  %5.0f%%\n",
                as.character(te_df$group[i]),
                te_df$n[i],
                formatC(round(te_df$mean_tau[i]), format="d", big.mark=","),
                formatC(round(te_df$ci_lo[i]),    format="d", big.mark=","),
                formatC(round(te_df$ci_hi[i]),    format="d", big.mark=","),
                te_df$pct_pos[i]*100))
  }
  cat(strrep("-", 65), "\n", sep="")
}

print_te_table(te_inc_nfa,  "TREATMENT EFFECT ON NET FINANCIAL ASSETS BY INCOME GROUP")
print_te_table(te_educ_nfa, "TREATMENT EFFECT ON NET FINANCIAL ASSETS BY EDUCATION")
print_te_table(te_age_nfa,  "TREATMENT EFFECT ON NET FINANCIAL ASSETS BY AGE GROUP")
print_te_table(te_inc_tw,   "TREATMENT EFFECT ON TOTAL WEALTH BY INCOME GROUP")
print_te_table(te_educ_tw,  "TREATMENT EFFECT ON TOTAL WEALTH BY EDUCATION")

# ── 5. Figure 6: Heterogeneity by Income Group ───────────────────────────────
p6 <- ggplot(te_inc_nfa, aes(x=group, y=mean_tau/1000)) +
  geom_col(fill="#2166ac", alpha=0.85, width=0.6) +
  geom_errorbar(aes(ymin=ci_lo/1000, ymax=ci_hi/1000),
                width=0.25, linewidth=0.8, color="gray30") +
  geom_hline(yintercept=0, linetype="dashed", color="gray50") +
  geom_text(aes(label=paste0("n=",n)),
            vjust=-0.5, size=3, color="gray30") +
  labs(
    title   = "Figure 6: Heterogeneous Treatment Effects by Income Group",
    subtitle = "Average individual-level treatment effect on Net Financial Assets\nestimated by Instrumental Forest (grf package)",
    x       = "Income Group",
    y       = "Mean Treatment Effect\n(thousands of 1991 USD)",
    caption = "Error bars = 95% confidence intervals.\nSource: 1991 SIPP test set (n=1,983)."
  ) +
  theme_bw(base_size=12) +
  theme(plot.title   = element_text(face="bold", size=11),
        axis.text.x  = element_text(angle=30, hjust=1))

ggsave("figure6_heterogeneity_income.png", p6, width=9, height=5, dpi=150)
cat("✓ Figure 6 saved\n")

# ── 6. Figure 7: Heterogeneity by Education ───────────────────────────────────
p7 <- ggplot(te_educ_nfa, aes(x=group, y=mean_tau/1000)) +
  geom_col(fill="#4dac26", alpha=0.85, width=0.6) +
  geom_errorbar(aes(ymin=ci_lo/1000, ymax=ci_hi/1000),
                width=0.25, linewidth=0.8, color="gray30") +
  geom_hline(yintercept=0, linetype="dashed", color="gray50") +
  geom_text(aes(label=paste0("n=",n)),
            vjust=-0.5, size=3, color="gray30") +
  labs(
    title   = "Figure 7: Heterogeneous Treatment Effects by Education Level",
    subtitle = "Average individual-level treatment effect on Net Financial Assets\nestimated by Instrumental Forest (grf package)",
    x       = "Education Level",
    y       = "Mean Treatment Effect\n(thousands of 1991 USD)",
    caption = "Error bars = 95% confidence intervals.\nSource: 1991 SIPP test set (n=1,983)."
  ) +
  theme_bw(base_size=12) +
  theme(plot.title=element_text(face="bold", size=11))

ggsave("figure7_heterogeneity_education.png", p7, width=8, height=5, dpi=150)
cat("✓ Figure 7 saved\n")

# ── 7. Figure 8: Distribution of individual treatment effects ─────────────────
p8 <- ggplot(preds, aes(x=tau_hat_nfa/1000)) +
  geom_histogram(bins=50, fill="#2166ac", alpha=0.75, color="white") +
  geom_vline(xintercept=mean(preds$tau_hat_nfa)/1000,
             color="#d6604d", linewidth=1.2, linetype="dashed") +
  annotate("text",
           x    = mean(preds$tau_hat_nfa)/1000 + 1,
           y    = Inf, vjust=1.5, hjust=0, size=3.5, color="#d6604d",
           label= sprintf("Mean ATE = $%s",
                          formatC(round(mean(preds$tau_hat_nfa)),
                                  format="d", big.mark=","))) +
  labs(
    title   = "Figure 8: Distribution of Individual Treatment Effects\n(Net Financial Assets)",
    subtitle = "Each observation's estimated causal effect of 401(k) participation",
    x       = "Individual Treatment Effect (thousands of 1991 USD)",
    y       = "Count",
    caption = "Instrumental Forest estimates. Source: 1991 SIPP test set (n=1,983)."
  ) +
  theme_bw(base_size=12) +
  theme(plot.title=element_text(face="bold", size=11))

ggsave("figure8_te_distribution.png", p8, width=8, height=5, dpi=150)
cat("✓ Figure 8 saved\n")

# ── 8. Figure 9: Combined — Income heterogeneity for NFA and TW ───────────────
combined <- bind_rows(
  te_inc_nfa %>% mutate(outcome="Net Financial Assets"),
  te_inc_tw  %>% mutate(outcome="Total Wealth")
)

p9 <- ggplot(combined, aes(x=group, y=mean_tau/1000, fill=outcome)) +
  geom_col(position="dodge", alpha=0.85, width=0.7) +
  geom_errorbar(aes(ymin=ci_lo/1000, ymax=ci_hi/1000),
                position=position_dodge(width=0.7),
                width=0.25, linewidth=0.7, color="gray30") +
  geom_hline(yintercept=0, linetype="dashed", color="gray50") +
  scale_fill_manual(values=c("Net Financial Assets"="#2166ac",
                             "Total Wealth"="#d6604d")) +
  labs(
    title   = "Figure 9: Treatment Effects on NFA vs. Total Wealth by Income Group",
    subtitle = "Divergence between NFA and Total Wealth effects indicates asset substitution",
    x       = "Income Group",
    y       = "Mean Treatment Effect\n(thousands of 1991 USD)",
    fill    = NULL,
    caption = "Error bars = 95% CI. Source: 1991 SIPP test set (n=1,983)."
  ) +
  theme_bw(base_size=12) +
  theme(legend.position="bottom",
        axis.text.x   = element_text(angle=30, hjust=1),
        plot.title    = element_text(face="bold", size=11))

ggsave("figure9_nfa_vs_tw_by_income.png", p9, width=9, height=5, dpi=150)
cat("✓ Figure 9 saved\n")

# ── 9. Save heterogeneity summary tables ─────────────────────────────────────
write.csv(te_inc_nfa,  "heterogeneity_income_nfa.csv",  row.names=FALSE)
write.csv(te_educ_nfa, "heterogeneity_educ_nfa.csv",    row.names=FALSE)
write.csv(te_age_nfa,  "heterogeneity_age_nfa.csv",     row.names=FALSE)
write.csv(te_inc_tw,   "heterogeneity_income_tw.csv",   row.names=FALSE)
write.csv(te_educ_tw,  "heterogeneity_educ_tw.csv",     row.names=FALSE)
cat("✓ Heterogeneity tables saved\n")

cat("\n=== KEY FINDINGS SUMMARY ===\n")
cat("Income heterogeneity (NFA):\n")
cat(sprintf("  Lowest treatment effect:  %s  ($%s)\n",
            as.character(te_inc_nfa$group[which.min(te_inc_nfa$mean_tau)]),
            formatC(round(min(te_inc_nfa$mean_tau)), format="d", big.mark=",")))
cat(sprintf("  Highest treatment effect: %s  ($%s)\n",
            as.character(te_inc_nfa$group[which.max(te_inc_nfa$mean_tau)]),
            formatC(round(max(te_inc_nfa$mean_tau)), format="d", big.mark=",")))
cat("Education heterogeneity (NFA):\n")
cat(sprintf("  Lowest treatment effect:  %s  ($%s)\n",
            as.character(te_educ_nfa$group[which.min(te_educ_nfa$mean_tau)]),
            formatC(round(min(te_educ_nfa$mean_tau)), format="d", big.mark=",")))
cat(sprintf("  Highest treatment effect: %s  ($%s)\n",
            as.character(te_educ_nfa$group[which.max(te_educ_nfa$mean_tau)]),
            formatC(round(max(te_educ_nfa$mean_tau)), format="d", big.mark=",")))

cat("\nStep 5 complete. All analysis done — ready to write the report.\n")