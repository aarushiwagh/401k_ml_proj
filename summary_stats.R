library(tidyverse)

df <- read.csv("401k_clean.csv")
load("project_config.RData")

wealth_vars <- c("net_tfa", "net_nifa", "tw")
covariate_vars <- c("inc", "age", "fsize", "marr", "pira", "db", "hown",
                    "nohs", "hs", "smcol", "col")
all_vars <- c(wealth_vars, covariate_vars)

var_labels <- c(
  net_tfa  = "Net Financial Assets ($)",
  net_nifa = "Net Non-401(k) Financial Assets ($)",
  tw       = "Total Wealth ($)",
  inc      = "Income ($)",
  age      = "Age",
  fsize    = "Family Size",
  marr     = "Married",
  pira     = "IRA Participant",
  db       = "Defined Benefit Pension",
  hown     = "Home Owner",
  nohs     = "Education: No High School",
  hs       = "Education: High School",
  smcol    = "Education: Some College",
  col      = "Education: College+"
)

summarise_group <- function(data, vars) {
  data %>%
    summarise(across(all_of(vars),
                     list(mean   = ~ mean(.x,   na.rm = TRUE),
                          sd     = ~ sd(.x,     na.rm = TRUE),
                          median = ~ median(.x, na.rm = TRUE)),
                     .names = "{.col}__{.fn}"))
}

full_stats  <- summarise_group(df, all_vars)
part_stats  <- summarise_group(df %>% filter(p401 == 1), all_vars)
nopart_stats<- summarise_group(df %>% filter(p401 == 0), all_vars)
elig_stats  <- summarise_group(df %>% filter(e401 == 1), all_vars)
noelig_stats<- summarise_group(df %>% filter(e401 == 0), all_vars)

build_table <- function(stats_df, vars, labels) {
  stats_df %>%
    pivot_longer(everything(),
                 names_to  = c("variable", "stat"),
                 names_sep = "__") %>%
    filter(variable %in% vars) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(
      variable = factor(variable, levels = vars),
      label    = labels[variable]
    ) %>%
    arrange(variable) %>%
    select(label, mean, sd, median)
}

tbl_full   <- build_table(full_stats,   all_vars, var_labels)
tbl_part   <- build_table(part_stats,   all_vars, var_labels)
tbl_nopart <- build_table(nopart_stats, all_vars, var_labels)
tbl_elig   <- build_table(elig_stats,   all_vars, var_labels)
tbl_noelig <- build_table(noelig_stats, all_vars, var_labels)

summary_table <- tibble(
  Variable              = tbl_full$label,
  Full_Mean             = tbl_full$mean,
  Full_SD               = tbl_full$sd,
  Full_Median           = tbl_full$median,
  Part_Mean             = tbl_part$mean,
  Part_SD               = tbl_part$sd,
  Part_Median           = tbl_part$median,
  NoPart_Mean           = tbl_nopart$mean,
  NoPart_SD             = tbl_nopart$sd,
  NoPart_Median         = tbl_nopart$median,
  Elig_Mean             = tbl_elig$mean,
  Elig_SD               = tbl_elig$sd,
  Elig_Median           = tbl_elig$median,
  NoElig_Mean           = tbl_noelig$mean,
  NoElig_SD             = tbl_noelig$sd,
  NoElig_Median         = tbl_noelig$median
)

dollar_rows <- which(summary_table$Variable %in%
                       c("Net Financial Assets ($)",
                         "Net Non-401(k) Financial Assets ($)",
                         "Total Wealth ($)",
                         "Income ($)"))

summary_table <- summary_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
summary_table[dollar_rows, ] <- summary_table[dollar_rows, ] %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

cat("=============================================================\n")
cat("TABLE 1: MEANS, STANDARD DEVIATIONS, AND MEDIANS\n")
cat("=============================================================\n")
cat(sprintf("Sample sizes:  Full = %d | Participants = %d | Non-Participants = %d\n",
            nrow(df), sum(df$p401), sum(df$p401 == 0)))
cat(sprintf("               Eligible = %d | Non-Eligible = %d\n",
            sum(df$e401), sum(df$e401 == 0)))
cat("\n")

print_section <- function(rows, title) {
  cat("\n---", title, "---\n")
  sub <- summary_table[rows, ]
  cat(sprintf("%-40s %10s %10s %10s | %10s %10s %10s | %10s %10s %10s | %10s %10s %10s | %10s %10s %10s\n",
              "Variable",
              "F.Mean","F.SD","F.Med",
              "P.Mean","P.SD","P.Med",
              "NP.Mean","NP.SD","NP.Med",
              "E.Mean","E.SD","E.Med",
              "NE.Mean","NE.SD","NE.Med"))
  for (i in seq_len(nrow(sub))) {
    cat(sprintf("%-40s %10s %10s %10s | %10s %10s %10s | %10s %10s %10s | %10s %10s %10s | %10s %10s %10s\n",
                sub$Variable[i],
                format(sub$Full_Mean[i],   big.mark=","),
                format(sub$Full_SD[i],     big.mark=","),
                format(sub$Full_Median[i], big.mark=","),
                format(sub$Part_Mean[i],   big.mark=","),
                format(sub$Part_SD[i],     big.mark=","),
                format(sub$Part_Median[i], big.mark=","),
                format(sub$NoPart_Mean[i], big.mark=","),
                format(sub$NoPart_SD[i],   big.mark=","),
                format(sub$NoPart_Median[i],big.mark=","),
                format(sub$Elig_Mean[i],   big.mark=","),
                format(sub$Elig_SD[i],     big.mark=","),
                format(sub$Elig_Median[i], big.mark=","),
                format(sub$NoElig_Mean[i], big.mark=","),
                format(sub$NoElig_SD[i],   big.mark=","),
                format(sub$NoElig_Median[i],big.mark=",")))
  }
}

print_section(1:3,  "Outcome Variables (Wealth)")
print_section(4:7,  "Covariates: Continuous & Binary")
print_section(8:11, "Covariates: Education")

write.csv(summary_table, "summary_statistics.csv", row.names = FALSE)
cat("\n✓ Summary table saved to: summary_statistics.csv\n")

df_long <- df %>%
  select(p401, net_tfa, net_nifa, tw) %>%
  pivot_longer(-p401,
               names_to  = "wealth_type",
               values_to = "value") %>%
  mutate(
    participation = ifelse(p401 == 1, "Participants", "Non-Participants"),
    wealth_type   = recode(wealth_type,
                           net_tfa  = "Net Financial Assets",
                           net_nifa = "Net Non-401(k) Assets",
                           tw       = "Total Wealth")
  )

df_long <- df_long %>%
  group_by(wealth_type) %>%
  mutate(
    lo = quantile(value, 0.05),
    hi = quantile(value, 0.95),
    value_w = pmin(pmax(value, lo), hi)
  ) %>%
  ungroup()

p1 <- ggplot(df_long, aes(x = value_w / 1000, fill = participation)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  facet_wrap(~ wealth_type, scales = "free") +
  scale_fill_manual(values = c("Participants" = "#2166ac",
                               "Non-Participants" = "#d6604d")) +
  labs(
    title    = "Figure 1: Wealth Distributions by 401(k) Participation Status",
    subtitle = "Winsorized at 5th and 95th percentiles for clarity",
    x        = "Wealth (thousands of 1991 USD)",
    y        = "Count",
    fill     = NULL,
    caption  = "Source: 1991 Survey of Income and Program Participation (SIPP). N = 9,915."
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave("figure1_wealth_distributions.png", p1,
       width = 10, height = 5, dpi = 150)
cat("✓ Figure 1 saved to: figure1_wealth_distributions.png\n")

df_inc <- df %>%
  group_by(inc_cat, p401) %>%
  summarise(mean_net_tfa = mean(net_tfa),
            mean_tw      = mean(tw),
            .groups = "drop") %>%
  mutate(participation = ifelse(p401 == 1, "Participants", "Non-Participants"))

p2 <- ggplot(df_inc, aes(x = inc_cat, y = mean_net_tfa / 1000,
                          fill = participation)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("Participants"     = "#2166ac",
                               "Non-Participants" = "#d6604d")) +
  labs(
    title   = "Figure 2: Mean Net Financial Assets by Income Category and Participation",
    x       = "Income Category",
    y       = "Mean Net Financial Assets\n(thousands of 1991 USD)",
    fill    = NULL,
    caption = "Source: 1991 SIPP. N = 9,915."
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave("figure2_wealth_by_income.png", p2,
       width = 9, height = 5, dpi = 150)
cat("✓ Figure 2 saved to: figure2_wealth_by_income.png\n")

cat("\nStep 2 complete. Ready for Step 3 (Replication Exercise: OLS & 2SLS).\n")
