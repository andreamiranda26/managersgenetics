# ─────────────────────────────────────────────────────────────────────────────
# FULL ANALYSIS SCRIPT (PLOS ONE-ready reporting additions)
# - Keeps your analysis approach the same (binomial GLMs + Wald OR CIs)
# - Adds Q12 + Q15 + Q17 + Q19 descriptive summaries + exports to outputs/
# - Uses Option B for exports (explicit outputs folder inside working directory)
# - FIXES: item.x/item.y join issue AND the "Q15/Q19 stored as one column" issue
# ─────────────────────────────────────────────────────────────────────────────

# 0) Libraries
library(tidyverse)
library(broom)
library(forcats)
library(stringr)
library(ggplot2)
library(patchwork)
library(scales)

# 1) Working directory + data import
setwd("~/Library/CloudStorage/Box-Box/GeneticData")

df_raw <- readr::read_csv("Surveyresults_04_29_25.csv", show_col_types = FALSE)

# Remove first 2 rows (metadata)
df <- df_raw[-c(1, 2), ]

# Add a row id (useful for summaries)
df <- df %>% mutate(rowid = row_number())

# Optional: clean whitespace / blank strings
clean_chr <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    na_if("") %>%
    na_if("NA") %>%
    na_if("N/A")
}
df <- df %>% mutate(across(where(is.character), clean_chr))

# Helper functions
pct <- function(x) round(100 * x, 1)
cols_starting <- function(data, prefix) names(data)[str_starts(names(data), prefix)]

# Convert Qualtrics checkbox-style columns to 0/1
as_checked01 <- function(x) {
  x_chr <- as.character(x) %>% str_squish()
  case_when(
    is.na(x_chr) ~ 0L,
    x_chr %in% c("", "0", "FALSE", "False", "No") ~ 0L,
    TRUE ~ 1L
  )
}

# Split multi-select stored in a single cell
split_multi <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A")] <- NA
  
  # Split on commas that are NOT inside parentheses
  # This regex means: split on comma only if the number of "(" after it
  # equals the number of ")" after it
  str_split(x, ",(?=(?:[^()]*\\([^()]*\\))*[^()]*$)")
}


# Summarize multi-select questions stored either as:
# (A) multiple checkbox columns Q15_1, Q15_2, ...
# (B) a single delimited column Q15 with "opt1, opt2; opt3"
summarize_multiselect <- function(data, q_prefix, group_var = "familiar_label") {
  
  exact_col <- q_prefix
  
  # ONLY treat as checkbox columns if they are like Q15_1, Q15_2, Q15_10 (digits only)
  checkbox_cols <- names(data)[str_detect(names(data), paste0("^", q_prefix, "_\\d+$"))]
  
  has_exact <- exact_col %in% names(data)
  has_checkboxes <- length(checkbox_cols) > 0
  
  # ── Format A: true checkbox columns ─────────────────────────────
  if (has_checkboxes) {
    
    long <- data %>%
      select(all_of(group_var), all_of(checkbox_cols)) %>%
      pivot_longer(cols = all_of(checkbox_cols), names_to = "item", values_to = "val") %>%
      mutate(
        checked = case_when(
          is.na(val) ~ 0L,
          as.character(val) %in% c("", "0", "FALSE", "False", "No") ~ 0L,
          TRUE ~ 1L
        )
      )
    
    overall <- long %>%
      group_by(item) %>%
      summarise(
        n_selected = sum(checked, na.rm = TRUE),
        n_total = n_distinct(data$rowid),
        percent = pct(n_selected / n_total),
        .groups = "drop"
      ) %>%
      arrange(desc(n_selected))
    
    by_group <- long %>%
      group_by(.data[[group_var]], item) %>%
      summarise(
        n_selected = sum(checked, na.rm = TRUE),
        n_total = n(),
        percent = pct(n_selected / n_total),
        .groups = "drop"
      ) %>%
      rename(group = .data[[group_var]]) %>%
      arrange(group, desc(n_selected))
    
    return(list(overall = overall, by_group = by_group))
  }
  
  # ── Format B: single delimited column (YOUR CASE) ───────────────
  if (has_exact) {
    
    long <- data %>%
      select(all_of(group_var), all_of(exact_col)) %>%
      mutate(items = split_multi(.data[[exact_col]])) %>%
      unnest(items) %>%
      filter(!is.na(items)) %>%
      mutate(item = str_squish(items)) %>%
      select(-items)
    
    if (nrow(long) == 0) {
      overall <- tibble(item = character(), n_selected = integer(), n_total = integer(), percent = double())
      by_group <- tibble(group = character(), item = character(), n_selected = integer(), n_total = integer(), percent = double())
      return(list(overall = overall, by_group = by_group))
    }
    
    overall <- long %>%
      count(item, name = "n_selected") %>%
      mutate(
        n_total = n_distinct(data$rowid),
        percent = pct(n_selected / n_total)
      ) %>%
      arrange(desc(n_selected))
    
    by_group <- long %>%
      count(.data[[group_var]], item, name = "n_selected") %>%
      group_by(.data[[group_var]]) %>%
      mutate(
        n_total = sum(data[[group_var]] == unique(.data[[group_var]])),
        percent = pct(n_selected / n_total)
      ) %>%
      ungroup() %>%
      rename(group = .data[[group_var]]) %>%
      arrange(group, desc(n_selected))
    
    return(list(overall = overall, by_group = by_group))
  }
  
  stop(paste0("No columns found for ", q_prefix, " (neither ", q_prefix, " nor ", q_prefix, "_[digits])"))
}


# 2) Core recodes (your same logic)
df <- df %>%
  mutate(
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1L,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0L,
      TRUE ~ NA_integer_
    ),
    role = fct_lump_min(as.factor(Q1), min = 3),
    years_in_role = as.factor(Q2),
    state = fct_lump_min(as.factor(Q3), min = 3),
    org_type = fct_lump_min(as.factor(Q4), min = 3),
    education = as.factor(Q5),
    gender = as.factor(Q7),
    age_range = as.factor(Q8)
  ) %>%
  filter(!is.na(familiar_bin)) %>%
  mutate(
    familiar_label = factor(familiar_bin, levels = c(0, 1),
                            labels = c("Not familiar", "Familiar"))
  )

cat("\nN after familiarity recode:", nrow(df), "\n")

# ───────────────────────────────────────────────────────────────
# SECTION A: Predictors of familiarity (individual GLMs)
# ───────────────────────────────────────────────────────────────

predictors <- list(
  role      = familiar_bin ~ role,
  years     = familiar_bin ~ years_in_role,
  state     = familiar_bin ~ state,
  org       = familiar_bin ~ org_type,
  education = familiar_bin ~ education,
  gender    = familiar_bin ~ gender,
  age       = familiar_bin ~ age_range
)

models_fam <- imap(predictors, ~ glm(.x, data = df, family = binomial))

glm_results_filtered <- imap_dfr(
  models_fam,
  ~ tidy(.x, conf.int = TRUE, conf.method = "Wald", exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(predictor = .y) %>%
    select(predictor, term, estimate, conf.low, conf.high, p.value)
) %>%
  rename(
    odds_ratio = estimate,
    CI_low     = conf.low,
    CI_high    = conf.high
  )

print(glm_results_filtered)

# ───────────────────────────────────────────────────────────────
# SECTION B: Figure 1 (Years in role + Org type), filtered to state/federal
# ───────────────────────────────────────────────────────────────

df2 <- df %>%
  filter(org_type %in% c("State agency", "Federal agency")) %>%
  mutate(
    years_role = factor(
      years_in_role,
      levels = c("0-5 years", "6-10 years", "11-15 years", "16-20 years", "21+ years")
    ),
    org_type = factor(org_type, levels = c("State agency", "Federal agency"))
  )

mod_years <- glm(familiar_bin ~ years_role, data = df2, family = binomial)
mod_org   <- glm(familiar_bin ~ org_type,   data = df2, family = binomial)

years_full <- tidy(mod_years, conf.int = TRUE, conf.method = "Wald", exponentiate = TRUE) %>%
  mutate(
    panel = "A: Years in role",
    level = if_else(term == "(Intercept)", levels(df2$years_role)[1], str_remove(term, "years_role"))
  ) %>%
  select(panel, level, estimate, conf.low, conf.high)

org_full <- tidy(mod_org, conf.int = TRUE, conf.method = "Wald", exponentiate = TRUE) %>%
  mutate(
    panel = "B: Org type",
    level = if_else(term == "(Intercept)", levels(df2$org_type)[1], str_remove(term, "org_type"))
  ) %>%
  select(panel, level, estimate, conf.low, conf.high)

fig1_df <- bind_rows(years_full, org_full) %>%
  group_by(panel) %>%
  mutate(level = factor(level, levels = unique(level))) %>%
  ungroup()

p1 <- ggplot(fig1_df, aes(x = level, y = estimate, ymin = conf.low, ymax = conf.high, fill = panel)) +
  geom_crossbar(width = 0.6, color = "black", linewidth = 0.8, fatten = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  facet_wrap(~ panel, scales = "free_x", nrow = 1) +
  scale_y_log10() +
  labs(
    title = "Figure 1. Odds ratios (±95% CI) for predictors of familiarity",
    x = NULL, y = "Odds ratio (log scale)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.position = "none"
  )

print(p1)

# ───────────────────────────────────────────────────────────────
# SECTION C: Figure 2 (Familiarity → support expectations)
# ───────────────────────────────────────────────────────────────

df_f2 <- df %>%
  mutate(
    support_increase = if_else(Q13 == "Increase", 1L, 0L),
    funding_increase = if_else(Q14 == "Increase", 1L, 0L),
    familiar = factor(familiar_bin, levels = c(0, 1), labels = c("Not familiar", "Familiar"))
  )

model_sup  <- glm(support_increase ~ familiar, data = df_f2, family = binomial)
model_fund <- glm(funding_increase ~ familiar, data = df_f2, family = binomial)

sup_or <- tidy(model_sup, conf.int = TRUE, conf.method = "Wald", exponentiate = TRUE) %>%
  filter(term == "familiarFamiliar") %>%
  transmute(outcome = "Organizational support", OR = estimate, CI_low = conf.low, CI_high = conf.high, p_value = p.value)

fund_or <- tidy(model_fund, conf.int = TRUE, conf.method = "Wald", exponentiate = TRUE) %>%
  filter(term == "familiarFamiliar") %>%
  transmute(outcome = "Financial support", OR = estimate, CI_low = conf.low, CI_high = conf.high, p_value = p.value)

fig2b_df <- bind_rows(sup_or, fund_or) %>%
  mutate(outcome = factor(outcome, levels = c("Organizational support", "Financial support")))

pred_grid <- tibble(
  familiar = factor(c("Not familiar", "Familiar"), levels = c("Not familiar", "Familiar"))
)

make_odds_pred_df <- function(model, outcome_label) {
  p <- predict(model, newdata = pred_grid, type = "link", se.fit = TRUE)
  pred_grid %>%
    mutate(
      OR = exp(p$fit),
      CI_low = exp(p$fit - 1.96 * p$se.fit),
      CI_high = exp(p$fit + 1.96 * p$se.fit),
      outcome = outcome_label
    )
}

sup_odds  <- make_odds_pred_df(model_sup,  "Organizational support")
fund_odds <- make_odds_pred_df(model_fund, "Financial support")

fig2a_df <- bind_rows(sup_odds, fund_odds) %>%
  mutate(outcome = factor(outcome, levels = c("Organizational support", "Financial support")))

p2a <- ggplot(fig2a_df, aes(x = familiar, y = OR, ymin = CI_low, ymax = CI_high, fill = outcome)) +
  geom_crossbar(position = position_dodge(width = 0.7), width = 0.6,
                color = "black", linewidth = 0.6, fatten = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  scale_y_log10() +
  labs(title = "A: Familiarity → Predicted odds of support", x = NULL, y = "Odds (log scale)", fill = NULL) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        legend.position = "top")

p2b <- ggplot(fig2b_df, aes(x = OR, y = outcome, xmin = CI_low, xmax = CI_high, fill = outcome)) +
  geom_crossbar(width = 0.6, color = "black", linewidth = 0.6, fatten = 1.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  scale_x_log10() +
  labs(title = "B: Odds ratio of Familiar vs Not familiar", x = "Odds ratio (log scale)", y = NULL, fill = NULL) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 12),
        legend.position = "none")

p2 <- (p2a | p2b) + plot_annotation(title = "Figure 2. Effect of familiarity on support expectations")
print(p2)

# ───────────────────────────────────────────────────────────────
# SECTION D: Figure 3 (Familiarity → benefits, trust, concerns)
# ───────────────────────────────────────────────────────────────

df3 <- df %>%
  mutate(
    ben_pop  = as.integer(Q9_2  == "Highly beneficial"),
    ben_mgmt = as.integer(Q9_1  == "Highly beneficial"),
    ben_econ = as.integer(Q9_3  == "Highly beneficial"),
    trust_dec  = as.integer(Q11_1 %in% c("Very high","Somewhat high")),
    trust_inst = as.integer(Q11_2 %in% c("Very high","Somewhat high")),
    trust_dat  = as.integer(Q11_3 %in% c("Very high","Somewhat high")),
    ch_access     = as.integer(Q10_1 %in% c("Very high","Somewhat high")),
    ch_complexity = as.integer(Q10_2 %in% c("Very high","Somewhat high")),
    ch_datainteg  = as.integer(Q10_3 %in% c("Very high","Somewhat high")),
    ch_expertise  = as.integer(Q10_4 %in% c("Very high","Somewhat high")),
    ch_time       = as.integer(Q10_5 %in% c("Very high","Somewhat high")),
    ch_applic     = as.integer(Q10_6 %in% c("Very high","Somewhat high")),
    ch_funding    = as.integer(Q10_7 %in% c("Very high","Somewhat high")),
    ch_technology = as.integer(Q10_8 %in% c("Very high","Somewhat high"))
  )

models3 <- list(
  "Understanding population dynamics" = glm(ben_pop   ~ familiar_bin, data = df3, family = binomial),
  "Improving management strategies"   = glm(ben_mgmt  ~ familiar_bin, data = df3, family = binomial),
  "Enhanced conservation outcomes"    = glm(ben_econ  ~ familiar_bin, data = df3, family = binomial),
  "Trust in management decisions"     = glm(trust_dec ~ familiar_bin, data = df3, family = binomial),
  "Trust in institutional decisions"  = glm(trust_inst ~ familiar_bin, data = df3, family = binomial),
  "Trust in data-collection methods"  = glm(trust_dat ~ familiar_bin, data = df3, family = binomial),
  "Access to DNA samples"             = glm(ch_access     ~ familiar_bin, data = df3, family = binomial),
  "Analysis complexity"               = glm(ch_complexity ~ familiar_bin, data = df3, family = binomial),
  "Data integration"                  = glm(ch_datainteg  ~ familiar_bin, data = df3, family = binomial),
  "Team expertise"                    = glm(ch_expertise  ~ familiar_bin, data = df3, family = binomial),
  "Time for analysis"                 = glm(ch_time       ~ familiar_bin, data = df3, family = binomial),
  "Applicability to conservation"     = glm(ch_applic     ~ familiar_bin, data = df3, family = binomial),
  "Funding availability"              = glm(ch_funding    ~ familiar_bin, data = df3, family = binomial),
  "Technology access"                 = glm(ch_technology ~ familiar_bin, data = df3, family = binomial)
)

results3 <- imap_dfr(models3, ~ {
  tidy(.x, exponentiate = TRUE, conf.int = TRUE, conf.method = "Wald") %>%
    filter(term == "familiar_bin") %>%
    transmute(outcome = .y, OR = estimate, CI_low = conf.low, CI_high = conf.high, p_value = p.value)
})
print(results3)

fig3_df <- results3 %>%
  mutate(
    type = case_when(
      outcome %in% c("Understanding population dynamics","Improving management strategies","Enhanced conservation outcomes") ~ "Benefit",
      outcome %in% c("Trust in management decisions","Trust in institutional decisions","Trust in data-collection methods") ~ "Trust",
      TRUE ~ "Concern"
    ),
    outcome = factor(outcome, levels = rev(c(
      "Understanding population dynamics",
      "Improving management strategies",
      "Enhanced conservation outcomes",
      "Trust in management decisions",
      "Trust in institutional decisions",
      "Trust in data-collection methods",
      "Access to DNA samples",
      "Analysis complexity",
      "Data integration",
      "Team expertise",
      "Time for analysis",
      "Applicability to conservation",
      "Funding availability",
      "Technology access"
    ))),
    type = factor(type, levels = c("Benefit","Trust","Concern"))
  )

p3 <- ggplot(fig3_df, aes(x = OR, y = outcome, xmin = CI_low, xmax = CI_high, fill = type)) +
  geom_crossbar(color = "black", linewidth = 0.7, fatten = 1.5, width = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_log10(breaks = c(0.1, 0.3, 1, 3, 10, 30), labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "Figure 3. Effect of familiarity on perceived benefits, trust, and concerns",
       x = "Odds ratio (log scale)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold"),
        panel.grid = element_blank(),
        legend.position = "top")

print(p3)

# ───────────────────────────────────────────────────────────────
# SECTION E: Q12 + Q15 + Q17 + Q19 results (DESCRIPTIVE TABLES)
# ───────────────────────────────────────────────────────────────

# Q12 overall + by familiarity
q12_overall <- df %>%
  mutate(Q12 = str_squish(as.character(Q12))) %>%
  filter(!is.na(Q12)) %>%
  count(Q12, name = "n") %>%
  mutate(percent = pct(n / sum(n))) %>%
  arrange(desc(n))

q12_by_fam <- df %>%
  mutate(Q12 = str_squish(as.character(Q12))) %>%
  filter(!is.na(Q12)) %>%
  count(familiar_label, Q12, name = "n") %>%
  group_by(familiar_label) %>%
  mutate(percent = pct(n / sum(n))) %>%
  ungroup() %>%
  arrange(familiar_label, desc(n))

print(q12_overall)
print(q12_by_fam)

# Optional: simple test (Yes vs No/Unsure) by familiarity
df_q12_bin <- df %>%
  mutate(
    q12_clear_bin = case_when(
      Q12 == "Yes" ~ 1L,
      Q12 %in% c("No", "Unsure") ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(q12_clear_bin))

q12_tab <- table(df_q12_bin$familiar_label, df_q12_bin$q12_clear_bin)
q12_fisher <- fisher.test(q12_tab)
cat("\nQ12 Fisher p-value (Yes vs No/Unsure by familiarity):", q12_fisher$p.value, "\n")

# Q15 multiselect (works whether stored as Q15 or Q15_*)
q15_res <- summarize_multiselect(df, "Q15", group_var = "familiar_label")
q15_overall <- q15_res$overall
q15_by_fam  <- q15_res$by_group

# Q15 "Other" text presence (if column exists)
q15_other <- NULL
if ("Q15_10_TEXT" %in% names(df)) {
  q15_other <- df %>%
    mutate(other_text = str_squish(as.character(Q15_10_TEXT))) %>%
    summarise(
      n_with_other_text = sum(!is.na(other_text) & other_text != "", na.rm = TRUE),
      n_total = n(),
      percent = pct(n_with_other_text / n_total)
    )
}

print(q15_overall)
print(q15_by_fam)
if (!is.null(q15_other)) print(q15_other)

# Q17 ranking (keeps your approach)
q17_cols <- names(df)[str_starts(names(df), "Q17")]
cat("\nDetected Q17 columns:\n"); print(q17_cols)

q17_long <- df %>%
  select(familiar_label, all_of(q17_cols)) %>%
  mutate(across(all_of(q17_cols), ~ suppressWarnings(as.integer(as.character(.x))))) %>%
  pivot_longer(cols = all_of(q17_cols), names_to = "item_raw", values_to = "rank") %>%
  filter(!is.na(rank)) %>%
  mutate(item = item_raw)

q17_meanrank <- q17_long %>%
  group_by(item) %>%
  summarise(mean_rank = mean(rank, na.rm = TRUE),
            sd_rank   = sd(rank, na.rm = TRUE),
            n         = n(),
            .groups = "drop") %>%
  arrange(mean_rank)

q17_meanrank_byfam <- q17_long %>%
  group_by(familiar_label, item) %>%
  summarise(mean_rank = mean(rank, na.rm = TRUE),
            sd_rank   = sd(rank, na.rm = TRUE),
            n         = n(),
            .groups = "drop") %>%
  arrange(familiar_label, mean_rank)

print(q17_meanrank)
print(q17_meanrank_byfam)

# Q19 multiselect (works whether stored as Q19 or Q19_*)
q19_res <- summarize_multiselect(df, "Q19", group_var = "familiar_label")
q19_overall <- q19_res$overall
q19_by_fam  <- q19_res$by_group

print(q19_overall)
print(q19_by_fam)

# ───────────────────────────────────────────────────────────────
# SECTION F: Export EVERYTHING to outputs folder inside working directory
# ───────────────────────────────────────────────────────────────
out_dir <- file.path(getwd(), "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Export model tables
readr::write_csv(glm_results_filtered, file.path(out_dir, "glm_familiarity_predictors_OR.csv"))
readr::write_csv(fig2b_df,             file.path(out_dir, "glm_support_OR.csv"))
readr::write_csv(results3,             file.path(out_dir, "glm_benefit_trust_concern_OR.csv"))

# Export Q12/Q15/Q17/Q19 tables
readr::write_csv(q12_overall, file.path(out_dir, "Q12_policy_clarity_overall.csv"))
readr::write_csv(q12_by_fam,  file.path(out_dir, "Q12_policy_clarity_by_familiarity.csv"))

readr::write_csv(q15_overall, file.path(out_dir, "Q15_training_needs_overall.csv"))
readr::write_csv(q15_by_fam,  file.path(out_dir, "Q15_training_needs_by_familiarity.csv"))
if (!is.null(q15_other)) {
  readr::write_csv(q15_other, file.path(out_dir, "Q15_training_needs_othertext_summary.csv"))
}

readr::write_csv(q17_meanrank,       file.path(out_dir, "Q17_rank_meanrank_overall.csv"))
readr::write_csv(q17_meanrank_byfam, file.path(out_dir, "Q17_rank_meanrank_by_familiarity.csv"))

readr::write_csv(q19_overall, file.path(out_dir, "Q19_genetic_diversity_importance_overall.csv"))
readr::write_csv(q19_by_fam,  file.path(out_dir, "Q19_genetic_diversity_importance_by_familiarity.csv"))

# Export figures (optional)
ggsave(file.path(out_dir, "Figure1.png"), p1, width = 10, height = 4, dpi = 300)
ggsave(file.path(out_dir, "Figure2.png"), p2, width = 12, height = 5, dpi = 300)
ggsave(file.path(out_dir, "Figure3.png"), p3, width = 10, height = 7, dpi = 300)

# Save session info (reproducibility)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))

cat("\nAll outputs written to:\n", out_dir, "\n")
