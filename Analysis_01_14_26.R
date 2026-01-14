# ─────────────────────────────────────────────────────────────────────────────
# Genetics-in-management survey: Descriptive / exploratory analysis (NO GLMs)
# Includes Q12 (laws/regulations clarity)
# ─────────────────────────────────────────────────────────────────────────────

# Packages
library(tidyverse)
library(forcats)
library(janitor)
library(scales)
library(stringr)

# ─────────────────────────────────────────────────────────────────────────────
# 1) Read data
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Library/CloudStorage/Box-Box/GeneticData")
df_raw <- read_csv("Surveyresults_04_29_25.csv", show_col_types = FALSE)

# Drop Qualtrics metadata rows (you previously removed first two rows)
df <- df_raw[-c(1, 2), ] %>%
  clean_names()

# Quick sanity checks
cat("\n--- Column names (first 40) ---\n")
print(names(df)[1:min(40, length(names(df)))])
cat("\nN rows (after dropping metadata): ", nrow(df), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2) Recode key variables (keep Likert categories; no dichotomizing outcomes)
# ─────────────────────────────────────────────────────────────────────────────

# NOTE: this assumes question columns are named q1, q2, ..., q16 after clean_names().
# If your file uses different names, adjust below.

df <- df %>%
  mutate(
    # Familiarity (Q16): keep 5-level ordinal
    familiarity_5 = factor(
      q16,
      levels = c("Very unfamiliar", "Somewhat unfamiliar", "Neutral/Unsure",
                 "Somewhat familiar", "Very familiar"),
      ordered = TRUE
    ),
    
    # Collapsed familiarity for descriptive contrasts only
    familiarity_2 = case_when(
      q16 %in% c("Very familiar", "Somewhat familiar") ~ "Higher familiarity",
      q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ "Lower familiarity",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Lower familiarity", "Higher familiarity")),
    
    # Respondent characteristics (presentation-friendly; lump rare levels if desired)
    role          = as.factor(q1),
    years_in_role = factor(q2, levels = c("0-5 years","6-10 years","11-15 years","16-20 years","21+ years")),
    state         = as.factor(q3),
    org_type      = as.factor(q4),
    education     = as.factor(q5),
    gender        = as.factor(q7),
    age_range     = as.factor(q8)
  ) %>%
  filter(!is.na(familiarity_5))

# Optional: lump rare categories for cleaner display (purely descriptive)
df <- df %>%
  mutate(
    role     = fct_lump_min(role, min = 3),
    state    = fct_lump_min(state, min = 3),
    org_type = fct_lump_min(org_type, min = 3)
  )

# ─────────────────────────────────────────────────────────────────────────────
# 3) Helper functions
# ─────────────────────────────────────────────────────────────────────────────

count_pct <- function(data, var) {
  data %>%
    count({{ var }}, name = "n") %>%
    mutate(
      pct = n / sum(n),
      pct_label = percent(pct, accuracy = 0.1)
    )
}

count_pct_by <- function(data, group_var, var) {
  data %>%
    count({{ group_var }}, {{ var }}, name = "n") %>%
    group_by({{ group_var }}) %>%
    mutate(
      pct = n / sum(n),
      pct_label = percent(pct, accuracy = 0.1)
    ) %>%
    ungroup()
}

overall_distributions <- function(data, vars) {
  map_dfr(vars, \(v) {
    if (!v %in% names(data)) {
      return(tibble(item = v, response = NA_character_, n = NA_integer_, pct = NA_real_))
    }
    data %>%
      count(.data[[v]], name = "n") %>%
      mutate(
        pct = n / sum(n),
        item = v
      ) %>%
      rename(response = 1) %>%
      select(item, response, n, pct)
  }) %>%
    mutate(pct_label = percent(pct, accuracy = 0.1))
}

bygroup_distributions <- function(data, group_var, vars) {
  map_dfr(vars, \(v) {
    if (!v %in% names(data)) {
      return(tibble(item = v, !!rlang::as_name(rlang::enquo(group_var)) := NA, response = NA, n = NA, pct = NA))
    }
    count_pct_by(data, {{ group_var }}, .data[[v]]) %>%
      rename(response = 2) %>%
      mutate(item = v) %>%
      select(item, {{ group_var }}, response, n, pct, pct_label)
  })
}

plot_distribution_by_group <- function(data, group_var, item_var, title_text) {
  if (!item_var %in% names(data)) {
    warning(paste("Column not found:", item_var))
    return(NULL)
  }
  data %>%
    filter(!is.na(.data[[item_var]]), !is.na({{ group_var }})) %>%
    count({{ group_var }}, .data[[item_var]]) %>%
    group_by({{ group_var }}) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(x = {{ group_var }}, y = pct, fill = .data[[item_var]])) +
    geom_col(position = "fill", color = "grey20", linewidth = 0.2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = title_text,
      x = NULL, y = "Percent of respondents", fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(face = "bold")
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 4) Table 1: respondent characteristics (overall)
# ─────────────────────────────────────────────────────────────────────────────

table1_role     <- count_pct(df, role)
table1_years    <- count_pct(df, years_in_role)
table1_org      <- count_pct(df, org_type)
table1_state    <- count_pct(df, state)
table1_edu      <- count_pct(df, education)
table1_gender   <- count_pct(df, gender)
table1_age      <- count_pct(df, age_range)
table1_fam5     <- count_pct(df, familiarity_5)
table1_fam2     <- count_pct(df, familiarity_2)

cat("\n--- Table 1 preview: familiarity_5 ---\n")
print(table1_fam5)

# ─────────────────────────────────────────────────────────────────────────────
# 5) Q12: Laws/regulations clarity (Yes/No/Unsure) — requested policy content
# ─────────────────────────────────────────────────────────────────────────────

# Assumes Q12 exports as q12
if ("q12" %in% names(df)) {
  
  overall_q12 <- df %>%
    count(q12, name = "n") %>%
    mutate(pct = n / sum(n), pct_label = percent(pct, accuracy = 0.1)) %>%
    rename(response = q12)
  
  byfam_q12 <- df %>%
    filter(!is.na(familiarity_2)) %>%
    count(familiarity_2, q12, name = "n") %>%
    group_by(familiarity_2) %>%
    mutate(pct = n / sum(n), pct_label = percent(pct, accuracy = 0.1)) %>%
    ungroup() %>%
    rename(response = q12)
  
  cat("\n--- Q12 overall ---\n")
  print(overall_q12)
  
  cat("\n--- Q12 by familiarity_2 ---\n")
  print(byfam_q12)
  
  fig_q12 <- plot_distribution_by_group(
    df, familiarity_2, "q12",
    "Q12. Are laws and regulations clear regarding the use of genetic data?"
  )
  
} else {
  warning("q12 not found. Check column name for Q12.")
  overall_q12 <- tibble()
  byfam_q12 <- tibble()
  fig_q12 <- NULL
}

# ─────────────────────────────────────────────────────────────────────────────
# 6) Core perception items: Benefits, Trust, Concerns, Support expectations
# ─────────────────────────────────────────────────────────────────────────────

vars_benefits <- c("q9_1", "q9_2", "q9_3")
vars_trust    <- c("q11_1", "q11_2", "q11_3")
vars_concerns <- paste0("q10_", 1:8)
vars_support  <- c("q13", "q14")

overall_benefits <- overall_distributions(df, vars_benefits)
overall_trust    <- overall_distributions(df, vars_trust)
overall_concerns <- overall_distributions(df, vars_concerns)
overall_support  <- overall_distributions(df, vars_support)

byfam_benefits <- bygroup_distributions(df, familiarity_2, vars_benefits)
byfam_trust    <- bygroup_distributions(df, familiarity_2, vars_trust)
byfam_concerns <- bygroup_distributions(df, familiarity_2, vars_concerns)
byfam_support  <- bygroup_distributions(df, familiarity_2, vars_support)

# Preview a couple
cat("\n--- Overall support expectations (q13, q14) ---\n")
print(overall_support)

cat("\n--- By familiarity: support expectations (q13, q14) ---\n")
print(byfam_support)

# Example figures (edit titles as needed)
fig_support_org <- plot_distribution_by_group(
  df, familiarity_2, "q13",
  "Expected organizational support for genetic data (next decade)"
)

fig_support_fund <- plot_distribution_by_group(
  df, familiarity_2, "q14",
  "Expected financial support for genetic data (next decade)"
)

# ─────────────────────────────────────────────────────────────────────────────
# 7) Reviewer-requested: Q15, Q17, Q19 summaries
# NOTE: These may export as multiple columns. We detect them automatically.
# ─────────────────────────────────────────────────────────────────────────────

# --- Q15: checkbox (training/education needs) ---
q15_cols <- names(df) %>% str_subset("^q15(_|$)")
cat("\n--- Detected Q15 columns ---\n")
print(q15_cols)

q15_summary <- tibble()
q15_byfam   <- tibble()

if (length(q15_cols) > 0) {
  q15_long <- df %>%
    select(familiarity_2, all_of(q15_cols)) %>%
    pivot_longer(cols = all_of(q15_cols), names_to = "option", values_to = "selected")
  
  cat("\n--- Q15 unique values (inspect) ---\n")
  print(unique(q15_long$selected))
  
  # Filter "selected" in a tolerant way: keep TRUE/1/"Selected"/non-empty strings
  q15_selected <- q15_long %>%
    filter(
      !is.na(selected),
      selected != "",
      selected != "0",
      selected != "FALSE",
      selected != FALSE
    )
  
  q15_summary <- q15_selected %>%
    count(option, name = "n") %>%
    mutate(pct = n / nrow(df), pct_label = percent(pct, accuracy = 0.1)) %>%
    arrange(desc(n))
  
  q15_byfam <- q15_selected %>%
    filter(!is.na(familiarity_2)) %>%
    count(familiarity_2, option, name = "n") %>%
    group_by(familiarity_2) %>%
    mutate(pct = n / sum(n), pct_label = percent(pct, accuracy = 0.1)) %>%
    ungroup() %>%
    arrange(familiarity_2, desc(n))
  
  cat("\n--- Q15 summary (top) ---\n")
  print(head(q15_summary, 10))
}

# --- Q17: ranking (what genetics are used to address) ---
q17_cols <- names(df) %>% str_subset("^q17(_|$)")
cat("\n--- Detected Q17 columns ---\n")
print(q17_cols)

q17_median <- tibble()
q17_borda  <- tibble()

if (length(q17_cols) > 0) {
  q17_long <- df %>%
    select(familiarity_2, all_of(q17_cols)) %>%
    pivot_longer(cols = all_of(q17_cols), names_to = "item", values_to = "rank") %>%
    mutate(rank = suppressWarnings(as.numeric(rank))) %>%
    filter(!is.na(rank))
  
  # Median & mean rank per item (lower rank = more important)
  q17_median <- q17_long %>%
    group_by(item) %>%
    summarise(median_rank = median(rank), mean_rank = mean(rank), n = n(), .groups = "drop") %>%
    arrange(median_rank, mean_rank)
  
  # Borda points
  max_rank <- max(q17_long$rank, na.rm = TRUE)
  q17_borda <- q17_long %>%
    mutate(points = max_rank - rank + 1) %>%
    group_by(item) %>%
    summarise(borda = sum(points), n = n(), .groups = "drop") %>%
    arrange(desc(borda))
  
  cat("\n--- Q17 median rank ---\n")
  print(q17_median)
  
  cat("\n--- Q17 Borda scores ---\n")
  print(q17_borda)
}

# --- Q19: checkbox (genetic diversity knowledge items) ---
q19_cols <- names(df) %>% str_subset("^q19(_|$)")
cat("\n--- Detected Q19 columns ---\n")
print(q19_cols)

q19_summary <- tibble()
q19_byfam   <- tibble()

if (length(q19_cols) > 0) {
  q19_long <- df %>%
    select(familiarity_2, all_of(q19_cols)) %>%
    pivot_longer(cols = all_of(q19_cols), names_to = "option", values_to = "selected")
  
  cat("\n--- Q19 unique values (inspect) ---\n")
  print(unique(q19_long$selected))
  
  q19_selected <- q19_long %>%
    filter(
      !is.na(selected),
      selected != "",
      selected != "0",
      selected != "FALSE",
      selected != FALSE
    )
  
  q19_summary <- q19_selected %>%
    count(option, name = "n") %>%
    mutate(pct = n / nrow(df), pct_label = percent(pct, accuracy = 0.1)) %>%
    arrange(desc(n))
  
  q19_byfam <- q19_selected %>%
    filter(!is.na(familiarity_2)) %>%
    count(familiarity_2, option, name = "n") %>%
    group_by(familiarity_2) %>%
    mutate(pct = n / sum(n), pct_label = percent(pct, accuracy = 0.1)) %>%
    ungroup() %>%
    arrange(familiarity_2, desc(n))
  
  cat("\n--- Q19 summary (top) ---\n")
  print(head(q19_summary, 10))
}

# ─────────────────────────────────────────────────────────────────────────────
# 8) Familiarity distribution figure
# ─────────────────────────────────────────────────────────────────────────────

fig_familiarity <- df %>%
  count(familiarity_5) %>%
  ggplot(aes(x = familiarity_5, y = n)) +
  geom_col(color = "grey20", linewidth = 0.2) +
  labs(x = NULL, y = "Number of respondents",
       title = "Self-rated familiarity with genetic data") +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold", angle = 20, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# Print key figures
print(fig_familiarity)
if (!is.null(fig_q12)) print(fig_q12)
if (!is.null(fig_support_org)) print(fig_support_org)
if (!is.null(fig_support_fund)) print(fig_support_fund)

# ─────────────────────────────────────────────────────────────────────────────
# 9) Export tables for manuscript/SI
# ─────────────────────────────────────────────────────────────────────────────

write_csv(table1_role,   "Table1_role.csv")
write_csv(table1_years,  "Table1_years_in_role.csv")
write_csv(table1_org,    "Table1_org_type.csv")
write_csv(table1_state,  "Table1_state.csv")
write_csv(table1_edu,    "Table1_education.csv")
write_csv(table1_gender, "Table1_gender.csv")
write_csv(table1_age,    "Table1_age_range.csv")
write_csv(table1_fam5,   "Table1_familiarity_5.csv")
write_csv(table1_fam2,   "Table1_familiarity_2.csv")

write_csv(overall_q12, "SI_overall_q12_policy_clarity.csv")
write_csv(byfam_q12,   "SI_byfamiliarity_q12_policy_clarity.csv")

write_csv(overall_benefits, "SI_overall_benefits.csv")
write_csv(overall_trust,    "SI_overall_trust.csv")
write_csv(overall_concerns, "SI_overall_concerns.csv")
write_csv(overall_support,  "SI_overall_support.csv")

write_csv(byfam_benefits, "SI_byfamiliarity_benefits.csv")
write_csv(byfam_trust,    "SI_byfamiliarity_trust.csv")
write_csv(byfam_concerns, "SI_byfamiliarity_concerns.csv")
write_csv(byfam_support,  "SI_byfamiliarity_support.csv")

write_csv(q15_summary, "SI_q15_training_summary.csv")
write_csv(q15_byfam,   "SI_q15_training_byfamiliarity.csv")

write_csv(q17_median, "SI_q17_rank_median.csv")
write_csv(q17_borda,  "SI_q17_rank_borda.csv")

write_csv(q19_summary, "SI_q19_knowledge_summary.csv")
write_csv(q19_byfam,   "SI_q19_knowledge_byfamiliarity.csv")

# Optional: save figures
# ggsave("Fig_familiarity.png", fig_familiarity, width = 6, height = 4, dpi = 300)
# if (!is.null(fig_q12)) ggsave("Fig_q12_policy_clarity.png", fig_q12, width = 6, height = 4, dpi = 300)
# ggsave("Fig_support_org.png", fig_support_org, width = 6, height = 4, dpi = 300)
# ggsave("Fig_support_fund.png", fig_support_fund, width = 6, height = 4, dpi = 300)

cat("\n✅ Done. Tables exported as CSV files in your working directory.\n")
