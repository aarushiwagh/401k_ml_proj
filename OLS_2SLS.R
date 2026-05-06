library(tidyverse)
library(AER)
library(sandwich)
library(lmtest)

df <- read.csv("401k.csv")

cat(sprintf("Loaded: %d rows x %d columns\n", nrow(df), ncol(df)))

df <- df %>%
  mutate(
    inc_cat = case_when(
      inc <  10000                 ~ "lt10K",
      inc >= 10000 & inc < 20000   ~ "10to20K",
      inc >= 20000 & inc < 30000   ~ "20to30K",
      inc >= 30000 & inc < 40000   ~ "30to40K",
      inc >= 40000 & inc < 50000   ~ "40to50K",
      inc >= 50000 & inc <= 75000  ~ "50to75K",
      inc >  75000                 ~ "gt75K"
    ),
    inc_cat = factor(inc_cat,
                     levels = c("lt10K","10to20K","20to30K","30to40K",
                                "40to50K","50to75K","gt75K"))
  )

df <- df %>%
  mutate(
    inc_10to20 = as.integer(inc_cat == "10to20K"),
    inc_20to30 = as.integer(inc_cat == "20to30K"),
    inc_30to40 = as.integer(inc_cat == "30to40K"),
    inc_40to50 = as.integer(inc_cat == "40to50K"),
    inc_50to75 = as.integer(inc_cat == "50to75K"),
    inc_gt75   = as.integer(inc_cat == "gt75K")
  )

df <- df %>%
  mutate(
    nohs  = as.integer(educ < 12),
    hs    = as.integer(educ == 12),
    smcol = as.integer(educ >= 13 & educ <= 15),
    col   = as.integer(educ >= 16)
  )

df <- df %>%
  mutate(
    age_lt30   = as.integer(age < 30),
    age_30_35  = as.integer(age >= 30 & age <= 35),
    age_36_44  = as.integer(age >= 36 & age <= 44),
    age_45_54  = as.integer(age >= 45 & age <= 54),
    age_55plus = as.integer(age >= 55)
  )

cat("Income category counts:\n")
print(table(df$inc_cat))
cat(sprintf("Participation: %.1f%% | Eligibility: %.1f%%\n\n",
            mean(df$p401)*100, mean(df$e401)*100))

controls_full <- c(
  "age_lt30","age_30_35","age_36_44","age_45_54",
  "inc_10to20","inc_20to30","inc_30to40","inc_40to50","inc_50to75","inc_gt75",
  "hs","smcol","col",
  "marr","fsize","twoearn","db","pira","hown"
)

controls_within <- c(
  "age_lt30","age_30_35","age_36_44","age_45_54",
  "hs","smcol","col",
  "marr","fsize","twoearn","db","pira","hown",
  "inc"
)

make_ols <- function(outcome, controls)
  as.formula(paste(outcome, "~ p401 +", paste(controls, collapse=" + ")))

make_iv <- function(outcome, controls)
  as.formula(paste(outcome, "~ p401 +", paste(controls, collapse=" + "),
                   "| e401 +",          paste(controls, collapse=" + ")))

make_fs <- function(controls)
  as.formula(paste("p401 ~ e401 +", paste(controls, collapse=" + ")))

run_models <- function(data, outcome, controls) {
  result <- list(n=nrow(data), fs_coef=NA, fs_se=NA,
                 ols_coef=NA, ols_se=NA, iv_coef=NA, iv_se=NA)

  if (nrow(data) < 50)     { message("  Skipped: n<50");               return(result) }
  if (var(data$e401) == 0) { message("  Skipped: no e401 variation");  return(result) }
  if (var(data$p401) == 0) { message("  Skipped: no p401 variation");  return(result) }

  tryCatch({
    fs <- lm(make_fs(controls), data=data)
    r  <- coeftest(fs, vcov=vcovHC(fs, type="HC1"))
    result$fs_coef <- r["e401","Estimate"]
    result$fs_se   <- r["e401","Std. Error"]
  }, error=function(e) message("  First stage error: ", e$message))

  tryCatch({
    m <- lm(make_ols(outcome, controls), data=data)
    r <- coeftest(m, vcov=vcovHC(m, type="HC1"))
    result$ols_coef <- r["p401","Estimate"]
    result$ols_se   <- r["p401","Std. Error"]
  }, error=function(e) message("  OLS error: ", e$message))

  tryCatch({
    m <- ivreg(make_iv(outcome, controls), data=data)
    r <- coeftest(m, vcov=vcovHC(m, type="HC1"))
    result$iv_coef <- r["p401","Estimate"]
    result$iv_se   <- r["p401","Std. Error"]
  }, error=function(e) message("  2SLS error: ", e$message))

  result
}

outcomes_list <- c("net_tfa","net_nifa","tw")

grp_levels <- c("lt10K","10to20K","20to30K","30to40K","40to50K","50to75K","gt75K")
grp_labels <- c("<$10K","$10-20K","$20-30K","$30-40K","$40-50K","$50-75K",">$75K")

cat("Running full sample regressions...\n")
full_results <- setNames(
  lapply(outcomes_list, function(y) run_models(df, y, controls_full)),
  outcomes_list)

cat("Running income-category regressions...\n")
inc_results <- lapply(grp_levels, function(grp) {
  sub <- df %>% filter(inc_cat == grp)
  cat(sprintf("  %-10s  n=%d\n", grp, nrow(sub)))
  setNames(lapply(outcomes_list, function(y) run_models(sub, y, controls_within)),
           outcomes_list)
})
names(inc_results) <- grp_levels

fc     <- function(x, w=8) { if(is.na(x)) return(formatC("NA",width=w));
                              formatC(round(x), format="d", big.mark=",", width=w) }
fse    <- function(x)       { if(is.na(x)) return("      NA");
                              paste0("(", formatC(round(x), format="d", big.mark=","), ")") }
ffs    <- function(x)       { if(is.na(x)) return("    NA");
                              formatC(round(x,3), format="f", digits=3, width=6) }

print_block <- function(lbl, n, rn, ri, rt) {
  cat(sprintf("%-9s %5d  %6s  %8s %8s  %8s %8s  %8s %8s\n",
              lbl, n, ffs(rn$fs_coef),
              fc(rn$ols_coef), fc(rn$iv_coef),
              fc(ri$ols_coef), fc(ri$iv_coef),
              fc(rt$ols_coef), fc(rt$iv_coef)))
  cat(sprintf("%-9s %5s  %6s  %8s %8s  %8s %8s  %8s %8s\n",
              "","","",
              fse(rn$ols_se), fse(rn$iv_se),
              fse(ri$ols_se), fse(ri$iv_se),
              fse(rt$ols_se), fse(rt$iv_se)))
}

div <- strrep("-", 90)
cat("\n", div, "\n", sep="")
cat("TABLE 3 REPLICATION — OLS AND 2SLS ESTIMATES OF 401(k) PARTICIPATION EFFECT\n")
cat(div, "\n", sep="")
cat(sprintf("%-9s %5s  %6s  %8s %8s  %8s %8s  %8s %8s\n",
            "Sample","N","1stSt",
            "NFA-OLS","NFA-2SLS","NNFA-OLS","NNFA-2SLS","TW-OLS","TW-2SLS"))
cat(div, "\n", sep="")

cat("A. Full Sample\n")
print_block("Full", nrow(df),
            full_results$net_tfa, full_results$net_nifa, full_results$tw)

cat("\nB. By Income Category\n")
for (i in seq_along(grp_levels)) {
  print_block(grp_labels[i], sum(df$inc_cat == grp_levels[i]),
              inc_results[[grp_levels[i]]]$net_tfa,
              inc_results[[grp_levels[i]]]$net_nifa,
              inc_results[[grp_levels[i]]]$tw)
}
cat(div, "\n", sep="")
cat("NFA=Net Financial Assets, NNFA=Net Non-401(k) Assets, TW=Total Wealth.\n")
cat("Robust standard errors (HC1) in parentheses.\n")
cat("2SLS instruments p401 with e401 (401k eligibility).\n")
cat("<$10K: all non-participants are ineligible — instrument has no variation.\n")

rows <- list()
for (y in outcomes_list) {
  r <- full_results[[y]]
  rows[[length(rows)+1]] <- data.frame(
    sample="Full Sample", n=r$n, outcome=y,
    fs_coef=r$fs_coef, ols_coef=r$ols_coef, ols_se=r$ols_se,
    iv_coef=r$iv_coef, iv_se=r$iv_se)
}
for (i in seq_along(grp_levels)) {
  for (y in outcomes_list) {
    r <- inc_results[[grp_levels[i]]][[y]]
    rows[[length(rows)+1]] <- data.frame(
      sample=grp_labels[i], n=r$n, outcome=y,
      fs_coef=r$fs_coef, ols_coef=r$ols_coef, ols_se=r$ols_se,
      iv_coef=r$iv_coef, iv_se=r$iv_se)
  }
}
results_df <- bind_rows(rows)
write.csv(results_df, "table3_replication.csv", row.names=FALSE)
cat("\n✓ Results saved to: table3_replication.csv\n")

plot_df <- results_df %>%
  filter(sample != "Full Sample") %>%
  mutate(
    sample        = factor(sample, levels=grp_labels),
    outcome_label = case_when(
      outcome == "net_tfa"  ~ "Net Financial Assets",
      outcome == "net_nifa" ~ "Net Non-401(k) Assets",
      outcome == "tw"       ~ "Total Wealth"
    )
  ) %>%
  pivot_longer(c(ols_coef, iv_coef), names_to="estimator", values_to="coef") %>%
  mutate(estimator = ifelse(estimator=="ols_coef","OLS","2SLS"))

p3 <- ggplot(plot_df, aes(x=sample, y=coef/1000, color=estimator, group=estimator)) +
  geom_line(linewidth=1, na.rm=TRUE) +
  geom_point(size=2.5, na.rm=TRUE) +
  facet_wrap(~outcome_label, scales="free_y") +
  scale_color_manual(values=c("OLS"="#d6604d","2SLS"="#2166ac")) +
  geom_hline(yintercept=0, linetype="dashed", color="gray50") +
  labs(title   = "Figure 3: OLS vs. 2SLS Estimates of 401(k) Participation Effect by Income Category",
       x="Income Category", y="Estimated Effect (thousands of 1991 USD)", color="Estimator",
       caption = paste0("Source: 1991 SIPP. Replication of Table 3, Panel A ",
                        "(Chernozhukov & Hansen, 2004).\n",
                        "<$10K omitted — instrument lacks sufficient variation.")) +
  theme_bw(base_size=12) +
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=30, hjust=1),
        plot.title=element_text(face="bold", size=11))

ggsave("figure3_ols_vs_2sls.png", p3, width=11, height=5, dpi=150)
cat("✓ Figure 3 saved to: figure3_ols_vs_2sls.png\n")
cat("\nStep 3 complete. Ready for Step 4 (ML Model).\n")
