# =============================================================================
# Step 4: ML Model — Random Forest + Instrumental Forest
# 401(k) Project - Effects of 401(k) Participation on Wealth
# =============================================================================

# install.packages(c("tidyverse","randomForest","grf"))

library(tidyverse)
library(randomForest)
library(grf)

set.seed(42)

# ── 1. Load and prepare data ──────────────────────────────────────────────────
df <- read.csv("401k.csv")

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
                                "40to50K","50to75K","gt75K")),
    nohs      = as.integer(educ < 12),
    hs        = as.integer(educ == 12),
    smcol     = as.integer(educ >= 13 & educ <= 15),
    col       = as.integer(educ >= 16),
    age_lt30  = as.integer(age < 30),
    age_30_35 = as.integer(age >= 30 & age <= 35),
    age_36_44 = as.integer(age >= 36 & age <= 44),
    age_45_54 = as.integer(age >= 45 & age <= 54),
    age_55plus= as.integer(age >= 55)
  )

# ── 2. Define feature matrices ────────────────────────────────────────────────
feature_cols <- c(
  "age","inc","fsize",
  "marr","twoearn","db","pira","hown",
  "nohs","hs","smcol","col",
  "age_lt30","age_30_35","age_36_44","age_45_54",
  "p401"          # included in RF for prediction; excluded in causal forest
)

X_with_treat <- df[, feature_cols]
X_no_treat   <- df[, setdiff(feature_cols, "p401")]
W  <- df$p401
Z  <- df$e401
Y_nfa <- df$net_tfa
Y_tw  <- df$tw

# ── 3. Train / Test Split (80/20) ─────────────────────────────────────────────
n         <- nrow(df)
train_idx <- sample(seq_len(n), size=floor(0.8*n), replace=FALSE)
test_idx  <- setdiff(seq_len(n), train_idx)

cat(sprintf("Train: %d | Test: %d\n", length(train_idx), length(test_idx)))

X_train     <- X_with_treat[train_idx, ]
X_test      <- X_with_treat[test_idx,  ]
Y_nfa_train <- Y_nfa[train_idx];  Y_nfa_test <- Y_nfa[test_idx]
Y_tw_train  <- Y_tw[train_idx];   Y_tw_test  <- Y_tw[test_idx]

# ── 4. Tune mtry via OOB error ────────────────────────────────────────────────
cat("\nTuning Random Forest (mtry)...\n")
p <- ncol(X_train)
mtry_candidates <- unique(c(floor(p/4), floor(p/3), floor(p/2), floor(sqrt(p))))

oob_errors <- sapply(mtry_candidates, function(m) {
  rf_tmp <- randomForest(x=X_train, y=Y_nfa_train,
                         ntree=300, mtry=m, importance=FALSE)
  tail(rf_tmp$mse, 1)
})

best_mtry <- mtry_candidates[which.min(oob_errors)]
cat(sprintf("  Best mtry: %d\n", best_mtry))

# ── 5. Train final RF models ──────────────────────────────────────────────────
cat("\nTraining Random Forest models (ntree=500)...\n")
rf_nfa <- randomForest(x=X_train, y=Y_nfa_train,
                       ntree=500, mtry=best_mtry, importance=TRUE)
rf_tw  <- randomForest(x=X_train, y=Y_tw_train,
                       ntree=500, mtry=best_mtry, importance=TRUE)
cat("  Done.\n")

# ── 6. Evaluate on test set ───────────────────────────────────────────────────
pred_nfa <- predict(rf_nfa, newdata=X_test)
pred_tw  <- predict(rf_tw,  newdata=X_test)

rmse <- function(a, p) sqrt(mean((a-p)^2))
r2   <- function(a, p) 1 - sum((a-p)^2)/sum((a-mean(a))^2)

rmse_nfa <- rmse(Y_nfa_test, pred_nfa);  r2_nfa <- r2(Y_nfa_test, pred_nfa)
rmse_tw  <- rmse(Y_tw_test,  pred_tw);   r2_tw  <- r2(Y_tw_test,  pred_tw)

cat("\n", strrep("-",55), "\n", sep="")
cat("MODEL PERFORMANCE ON TEST SET\n")
cat(strrep("-",55), "\n", sep="")
cat(sprintf("%-35s  %8s  %5s\n","Outcome","RMSE ($)","R²"))
cat(strrep("-",55), "\n", sep="")
cat(sprintf("%-35s  %8s  %5s\n","Net Financial Assets",
            formatC(round(rmse_nfa),format="d",big.mark=","), round(r2_nfa,3)))
cat(sprintf("%-35s  %8s  %5s\n","Total Wealth",
            formatC(round(rmse_tw), format="d",big.mark=","), round(r2_tw,3)))
cat(strrep("-",55), "\n", sep="")

# ── 7. Variable importance plot ───────────────────────────────────────────────
# Clean variable labels using case_when instead of recode()
relabel <- function(v) {
  case_when(
    v == "inc"      ~ "Income",
    v == "age"      ~ "Age",
    v == "fsize"    ~ "Family Size",
    v == "marr"     ~ "Married",
    v == "twoearn"  ~ "Two Earners",
    v == "db"       ~ "DB Pension",
    v == "pira"     ~ "IRA Participant",
    v == "hown"     ~ "Home Owner",
    v == "nohs"     ~ "Educ: No HS",
    v == "hs"       ~ "Educ: HS",
    v == "smcol"    ~ "Educ: Some College",
    v == "col"      ~ "Educ: College+",
    v == "p401"     ~ "401k Participant",
    v == "age_lt30" ~ "Age <30",
    v == "age_30_35"~ "Age 30-35",
    v == "age_36_44"~ "Age 36-44",
    v == "age_45_54"~ "Age 45-54",
    TRUE            ~ v
  )
}

imp_df <- importance(rf_nfa, type=1) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(importance = `%IncMSE`) %>%
  arrange(desc(importance)) %>%
  slice_head(n=12) %>%
  mutate(variable = relabel(variable))

p_imp <- ggplot(imp_df, aes(x=reorder(variable, importance), y=importance)) +
  geom_col(fill="#2166ac", alpha=0.85) +
  coord_flip() +
  labs(title    = "Figure 4: Variable Importance — Random Forest (Net Financial Assets)",
       subtitle = "Metric: % Increase in MSE when variable is permuted (out-of-bag)",
       x=NULL, y="% Increase in MSE",
       caption  = "Source: 1991 SIPP. Based on training set (n=7,932).") +
  theme_bw(base_size=12) +
  theme(plot.title=element_text(face="bold", size=11))

ggsave("figure4_variable_importance.png", p_imp, width=8, height=5, dpi=150)
cat("✓ Figure 4 saved\n")

# ── 8. Predicted vs Actual plot ───────────────────────────────────────────────
q05 <- quantile(Y_nfa_test, 0.05)
q95 <- quantile(Y_nfa_test, 0.95)

plot_fit <- data.frame(
  actual    = pmin(pmax(Y_nfa_test, q05), q95),
  predicted = pmin(pmax(pred_nfa,   q05), q95),
  treated   = factor(W[test_idx], labels=c("Non-Participant","Participant"))
)

p_fit <- ggplot(plot_fit, aes(x=actual/1000, y=predicted/1000, color=treated)) +
  geom_point(alpha=0.3, size=0.8) +
  geom_abline(slope=1, intercept=0, linetype="dashed", linewidth=1) +
  scale_color_manual(values=c("Non-Participant"="#d6604d","Participant"="#2166ac")) +
  labs(title    = "Figure 5: Predicted vs. Actual Net Financial Assets (Test Set)",
       subtitle = sprintf("R² = %.3f | RMSE = $%s",
                          r2_nfa, formatC(round(rmse_nfa),format="d",big.mark=",")),
       x="Actual (thousands USD)", y="Predicted (thousands USD)", color=NULL,
       caption  = "Winsorized at 5th/95th percentiles. Dashed line = perfect prediction.") +
  theme_bw(base_size=12) +
  theme(legend.position="bottom",
        plot.title=element_text(face="bold", size=11))

ggsave("figure5_predicted_vs_actual.png", p_fit, width=7, height=6, dpi=150)
cat("✓ Figure 5 saved\n")

# ── 9. Causal Forest — Instrumental Forest ────────────────────────────────────
cat("\nFitting Instrumental Forest for net_tfa...\n")
cat("(This may take 2-5 minutes)\n")

X_cf    <- as.matrix(X_no_treat)
X_cf_tr <- X_cf[train_idx, ]
X_cf_te <- X_cf[test_idx,  ]

cf_nfa <- instrumental_forest(
  X = X_cf_tr,
  Y = Y_nfa_train,
  W = W[train_idx],
  Z = Z[train_idx],
  num.trees       = 2000,
  tune.parameters = "all",
  seed            = 42
)
cat("  Done.\n")

ate_nfa     <- average_treatment_effect(cf_nfa)
tau_hat_nfa <- predict(cf_nfa, newdata=X_cf_te)$predictions

cat(sprintf("  ATE (net_tfa): $%s  (SE: $%s)\n",
            formatC(round(ate_nfa["estimate"]),format="d",big.mark=","),
            formatC(round(ate_nfa["std.err"]), format="d",big.mark=",")))

cat("\nFitting Instrumental Forest for tw...\n")
cf_tw <- instrumental_forest(
  X = X_cf_tr,
  Y = Y_tw_train,
  W = W[train_idx],
  Z = Z[train_idx],
  num.trees       = 2000,
  tune.parameters = "all",
  seed            = 42
)
cat("  Done.\n")

ate_tw     <- average_treatment_effect(cf_tw)
tau_hat_tw <- predict(cf_tw, newdata=X_cf_te)$predictions

cat(sprintf("  ATE (tw):      $%s  (SE: $%s)\n",
            formatC(round(ate_tw["estimate"]),format="d",big.mark=","),
            formatC(round(ate_tw["std.err"]), format="d",big.mark=",")))

# ── 10. Print ATE summary ─────────────────────────────────────────────────────
cat("\n", strrep("-",60), "\n", sep="")
cat("AVERAGE TREATMENT EFFECTS — INSTRUMENTAL FOREST\n")
cat(strrep("-",60), "\n", sep="")
cat(sprintf("%-35s  %9s  %9s\n","Outcome","ATE ($)","SE ($)"))
cat(strrep("-",60), "\n", sep="")
cat(sprintf("%-35s  %9s  %9s\n","Net Financial Assets",
            formatC(round(ate_nfa["estimate"]),format="d",big.mark=","),
            formatC(round(ate_nfa["std.err"]), format="d",big.mark=",")))
cat(sprintf("%-35s  %9s  %9s\n","Total Wealth",
            formatC(round(ate_tw["estimate"]), format="d",big.mark=","),
            formatC(round(ate_tw["std.err"]),  format="d",big.mark=",")))
cat(strrep("-",60), "\n", sep="")

# ── 11. Save outputs for Step 5 ───────────────────────────────────────────────
test_results <- df[test_idx, ] %>%
  mutate(
    pred_nfa    = pred_nfa,
    pred_tw     = pred_tw,
    tau_hat_nfa = tau_hat_nfa,
    tau_hat_tw  = tau_hat_tw
  )
write.csv(test_results, "test_set_predictions.csv", row.names=FALSE)

perf <- data.frame(
  outcome = c("net_tfa","tw"),
  rmse    = c(rmse_nfa, rmse_tw),
  r2      = c(r2_nfa,   r2_tw),
  ate     = c(ate_nfa["estimate"], ate_tw["estimate"]),
  ate_se  = c(ate_nfa["std.err"],  ate_tw["std.err"])
)
write.csv(perf, "model_performance.csv", row.names=FALSE)

cat("✓ test_set_predictions.csv saved\n")
cat("✓ model_performance.csv saved\n")
cat("\nStep 4 complete. Ready for Step 5 (Heterogeneity Analysis).\n")