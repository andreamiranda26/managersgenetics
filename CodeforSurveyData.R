# Load libraries
library(tidyverse)
library(broom)
library(forcats)
library(ggplot2)

# Set working directory and read the CSV
setwd("C:/Users/andre/Box/GeneticData")
df_raw <- read_csv("Surveyresults_04_29_25.csv")

# Remove rows 2 and 1 (rows 1 and 2 of metadata) this was because I only need the header and the data 
df <- df_raw[-c(1, 2), ]

# Recode outcome variable (Q16 = familiarity)
df <- df %>%
  mutate(
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0,
      TRUE ~ NA_real_
    ),
    role = fct_lump_min(as.factor(Q1), min = 3),
    years_in_role = as.factor(Q2),
    state = fct_lump_min(as.factor(Q3), min = 3),
    org_type = fct_lump_min(as.factor(Q4), min = 3),
    education = as.factor(Q5),
    gender = as.factor(Q7),
    age_range = as.factor(Q8)
  ) %>%
  filter(!is.na(familiar_bin))

# Create a named list of predictor formulas
predictors <- list(
  role = familiar_bin ~ role,
  years = familiar_bin ~ years_in_role,
  state = familiar_bin ~ state,
  org = familiar_bin ~ org_type,
  education = familiar_bin ~ education,
  gender = familiar_bin ~ gender,
  age = familiar_bin ~ age_range
)

# Run GLMs individually and tidy the results
glm_results <- map_dfr(
  predictors,
  ~ tidy(glm(.x, data = df, family = binomial)),
  .id = "predictor"
)

# Show only coefficients (drop intercepts)
glm_results_filtered <- glm_results %>%
  filter(term != "(Intercept)") %>%
  select(predictor, term, estimate, std.error, statistic, p.value)

# View results
print(glm_results_filtered)
#############################

#examine whether years in role (Q2) predicts familiarity with genetic data (Q16)

library(tidyverse)

# Recode familiarity (Q16): 1 = Familiar, 0 = Not familiar
df$familiar_binary <- ifelse(
  df$Q16 %in% c("Very familiar", "Somewhat familiar"), 1, 0
)

# Recode years in role (Q2) as ordered factor
df$years_in_role <- factor(
  df$Q2,
  levels = c("0-5 years", "6-10 years", "11-15 years", "16-20 years", "21+ years"),
  ordered = TRUE
)

# Recode organization type (Q4) if not already done
df$org_binary <- case_when(
  df$Q4 == "Federal agency" ~ "Federal",
  df$Q4 == "State agency" ~ "State",
  TRUE ~ NA_character_
)

# Fit binomial GLM: Does experience predict familiarity?
model <- glm(familiar_binary ~ years_in_role, data = df, family = binomial)

# Print model summary
summary(model)

# Get odds ratios and 95% confidence intervals
exp(cbind(OR = coef(model), confint(model)))

# Predict probabilities for each category
new_data <- data.frame(
  years_in_role = factor(
    levels(df$years_in_role),
    levels = c("0-5 years", "6-10 years", "11-15 years", "16-20 years", "21+ years"),
    ordered = TRUE
  )
)
new_data$predicted_prob <- predict(model, newdata = new_data, type = "response")

# Plot predicted probabilities
ggplot(new_data, aes(x = years_in_role, y = predicted_prob)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    x = "Years in Role",
    y = "Predicted Probability of Familiarity",
    title = "Predicted Familiarity with Genetic Data by Years in Role"
  ) +
  ylim(0, 1)

# Create table of counts by years in role and organization type
count_table <- df %>%
  filter(!is.na(years_in_role), !is.na(org_binary)) %>%
  count(years_in_role, org_binary, name = "n") %>%
  pivot_wider(
    names_from = org_binary,
    values_from = n,
    values_fill = 0
  )

# View count table
print(count_table)

# Optional: save table as CSV
write.csv(count_table, "experience_counts_by_org.csv", row.names = FALSE)


######################################
#state vs federal agency with familiarity
# Load packages
library(tidyverse)

# Clean org_type and familiarity
df <- df %>%
  mutate(
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0,
      TRUE ~ NA_real_
    ),
    org_binary = case_when(
      Q4 == "Federal agency" ~ "Federal",
      Q4 == "State agency" ~ "State",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(familiar_bin) & !is.na(org_binary))  # remove missing

# Recode to factor
df$org_binary <- factor(df$org_binary, levels = c("State", "Federal"))

# Run binomial GLM
glm_org <- glm(familiar_bin ~ org_binary, data = df, family = binomial)
summary(glm_org)

# Optional: odds ratio
exp(cbind(OR = coef(glm_org), confint(glm_org)))

ggplot(df, aes(x = org_binary, fill = factor(familiar_bin))) +
  geom_bar(position = "fill") +
  labs(
    title = "Familiarity with Genetic Data by Organization Type",
    x = "Organization Type",
    y = "Proportion",
    fill = "Familiar"
  ) +
  scale_fill_manual(values = c("0" = "gray70", "1" = "darkgreen"),
                    labels = c("Not Familiar", "Familiar")) +
  theme_minimal()

# Option 1: Basic table
table(df$org_binary, df$familiar_bin)

# Option 2: Tidy table for printing/export
familiarity_counts <- df %>%
  group_by(org_binary, familiar_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Familiarity = ifelse(familiar_bin == 1, "Familiar", "Not Familiar")) %>%
  select(Organization = org_binary, Familiarity, n)

# Print tidy table
print(familiarity_counts)

####################################

#familiarity vs organizational support and funding support over the next 10 years

# Recode variables 
df <- df %>%
  mutate(
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0,
      TRUE ~ NA_real_
    ),
    support_increase = case_when(
      Q13 == "Increase" ~ 1,
      Q13 %in% c("Decrease", "Stay the same", "Unsure") ~ 0,
      TRUE ~ NA_real_
    ),
    funding_increase = case_when(
      Q14 == "Increase" ~ 1,
      Q14 %in% c("Decrease", "Stay the same", "Unsure") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(familiar_bin))  # Drop rows with missing outcome

# Drop NAs for each model separately
df_support <- df %>% filter(!is.na(support_increase))
df_funding <- df %>% filter(!is.na(funding_increase))

# GLM: Does familiarity predict belief in increasing support?
glm_support <- glm(support_increase ~ familiar_bin, data = df_support, family = binomial)
summary(glm_support)

# GLM: Does familiarity predict belief in increasing funding?
glm_funding <- glm(funding_increase ~ familiar_bin, data = df_funding, family = binomial)
summary(glm_funding)

# Odds ratios
exp(cbind(OR = coef(glm_support), confint(glm_support)))
exp(cbind(OR = coef(glm_funding), confint(glm_funding)))



# Prepare the data with readable labels
plot_data <- df %>%
  filter(!is.na(familiar_bin), !is.na(support_increase)) %>%
  mutate(
    familiar_label = factor(familiar_bin, labels = c("Not Familiar", "Familiar")),
    support_label = factor(support_increase, labels = c("Does NOT Expect Increase", "Expects Increase"))
  )

# Side-by-side bar plot (count)
ggplot(plot_data, aes(x = familiar_label, fill = support_label)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Familiarity with Genetic Data vs. Belief in Organizational Support Growth",
    x = "Familiarity with Genetic Data",
    y = "Number of Respondents",
    fill = "Support Outlook"
  ) +
  scale_fill_manual(values = c("gray70", "darkgreen")) +
  theme_minimal(base_size = 13)



# Extract odds ratios and 95% CI for both models
or_support <- exp(cbind(OR = coef(glm_support), confint(glm_support)))[2, ]
or_funding <- exp(cbind(OR = coef(glm_funding), confint(glm_funding)))[2, ]

# Combine into a data frame
support_forest_df <- data.frame(
  Outcome = c(
    "Belief in Increased Organizational Support (Q13)",
    "Belief in Increased Financial Support (Q14)"
  ),
  OR = c(or_support["OR"], or_funding["OR"]),
  CI_lower = c(or_support["2.5 %"], or_funding["2.5 %"]),
  CI_upper = c(or_support["97.5 %"], or_funding["97.5 %"])
)

# Plot
library(ggplot2)

ggplot(support_forest_df, aes(x = OR, y = reorder(Outcome, OR))) +
  geom_point(color = "red", size = 3) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_log10() +
  labs(
    title = "Effect of Familiarity on Beliefs About Support for Genetic Data",
    x = "Odds Ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 13)






# Create prediction dataset
predict_df <- data.frame(familiar_bin = c(0, 1))

# Get predicted probabilities with confidence intervals
predict_df <- predict_df %>%
  mutate(
    predicted_prob = predict(glm_support, newdata = ., type = "response"),
    label = factor(familiar_bin, labels = c("Not Familiar", "Familiar"))
  )

# Plot predicted probabilities
library(ggplot2)

ggplot(predict_df, aes(x = label, y = predicted_prob)) +
  geom_col(fill = "darkgreen", width = 0.6) +
  labs(
    title = "Predicted Probability of Belief in Organizational Support Growth",
    x = "Familiarity with Genetic Data",
    y = "Predicted Probability"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 13)



df %>%
  filter(!is.na(familiar_bin), !is.na(support_increase)) %>%
  mutate(
    Familiarity = factor(familiar_bin, labels = c("Not Familiar", "Familiar")),
    Support_Outlook = factor(support_increase, labels = c("Does NOT Expect Increase", "Expects Increase"))
  ) %>%
  count(Familiarity, Support_Outlook)


###########################################

#Familiarity and perceived benefits in using GD in management 

# Recode benefit ratings into binary outcomes
df <- df %>%
  mutate(
    benefit_manage = ifelse(Q9_1 %in% c("Highly beneficial"), 1, 0),
    benefit_population = ifelse(Q9_2 %in% c("Highly beneficial"), 1, 0),
    benefit_conservation = ifelse(Q9_3 %in% c("Highly beneficial"), 1, 0),
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(familiar_bin))  # Remove missing familiarity responses

# GLM: Does familiarity predict high perceived benefit?
glm_manage <- glm(benefit_manage ~ familiar_bin, data = df, family = binomial)
glm_population <- glm(benefit_population ~ familiar_bin, data = df, family = binomial)
glm_conservation <- glm(benefit_conservation ~ familiar_bin, data = df, family = binomial)

# View results
summary(glm_manage)
summary(glm_population)
summary(glm_conservation)

# Odds ratios
exp(cbind(OR = coef(glm_manage), confint(glm_manage)))
exp(cbind(OR = coef(glm_population), confint(glm_population)))
exp(cbind(OR = coef(glm_conservation), confint(glm_conservation)))



# Extract ORs and CIs for each model
or_manage <- exp(cbind(OR = coef(glm_manage), confint.default(glm_manage)))[2, ]
or_population <- exp(cbind(OR = coef(glm_population), confint.default(glm_population)))[2, ]
or_conservation <- exp(cbind(OR = coef(glm_conservation), confint.default(glm_conservation)))[2, ]


# Create a summary dataframe
or_df <- data.frame(
  Benefit = c("Improved management strategies", 
              "Understanding of population dynamics", 
              "Enhanced conservation outcomes"),
  OR = c(or_manage["OR"], or_population["OR"], or_conservation["OR"]),
  CI_lower = c(or_manage["2.5 %"], or_population["2.5 %"], or_conservation["2.5 %"]),
  CI_upper = c(or_manage["97.5 %"], or_population["97.5 %"], or_conservation["97.5 %"])
)

# Plot the odds ratios
library(ggplot2)

ggplot(or_df, aes(x = OR, y = reorder(Benefit, OR))) +
  geom_point(size = 3, color = "red") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Effect of Familiarity on Perceived Benefits of Genetic Data",
    x = "Odds Ratio (log scale)",
    y = "Benefit Type"
  ) +
  scale_x_log10() +
  theme_minimal(base_size = 13)

##################################

#Part 2 of Familiarity and perceived benefits in using GD in management 

# Load required packages
library(tidyverse)
library(MASS)  # for polr()
library(broom) # for tidy model output

# Recode perceived benefit ratings into ordered factors
df <- df %>%
  mutate(
    benefit_manage = factor(Q9_1, levels = c("Not beneficial", "Sometimes beneficial", "Highly beneficial"), ordered = TRUE),
    benefit_population = factor(Q9_2, levels = c("Not beneficial", "Sometimes beneficial", "Highly beneficial"), ordered = TRUE),
    benefit_conservation = factor(Q9_3, levels = c("Not beneficial", "Sometimes beneficial", "Highly beneficial"), ordered = TRUE),
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(familiar_bin))  # Remove rows with missing familiarity

# Fit ordinal logistic regression models
model_manage <- polr(benefit_manage ~ familiar_bin, data = df, Hess = TRUE)
model_population <- polr(benefit_population ~ familiar_bin, data = df, Hess = TRUE)
model_conservation <- polr(benefit_conservation ~ familiar_bin, data = df, Hess = TRUE)

# Extract ORs and confidence intervals
or_manage <- exp(cbind(OR = coef(model_manage), confint.default(model_manage)))
or_population <- exp(cbind(OR = coef(model_population), confint.default(model_population)))
or_conservation <- exp(cbind(OR = coef(model_conservation), confint.default(model_conservation)))

# Combine results into a summary dataframe
or_df <- data.frame(
  Benefit = c("Improved management strategies",
              "Understanding of population dynamics",
              "Enhanced conservation outcomes"),
  OR = c(or_manage["familiar_bin", "OR"], or_population["familiar_bin", "OR"], or_conservation["familiar_bin", "OR"]),
  CI_lower = c(or_manage["familiar_bin", "2.5 %"], or_population["familiar_bin", "2.5 %"], or_conservation["familiar_bin", "2.5 %"]),
  CI_upper = c(or_manage["familiar_bin", "97.5 %"], or_population["familiar_bin", "97.5 %"], or_conservation["familiar_bin", "97.5 %"])
)

# Plot odds ratios
ggplot(or_df, aes(x = OR, y = reorder(Benefit, OR))) +
  geom_point(size = 3, color = "red") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Effect of Familiarity on Perceived Benefits of Genetic Data",
    x = "Odds Ratio (log scale)",
    y = "Benefit Type"
  ) +
  scale_x_log10() +
  theme_minimal(base_size = 13)

##For pvalues now 

# Extract summary objects
summary_manage <- summary(model_manage)
summary_population <- summary(model_population)
summary_conservation <- summary(model_conservation)

# Calculate p-values from t-values
pval_manage <- pnorm(abs(coef(summary_manage)[, "t value"]), lower.tail = FALSE) * 2
pval_population <- pnorm(abs(coef(summary_population)[, "t value"]), lower.tail = FALSE) * 2
pval_conservation <- pnorm(abs(coef(summary_conservation)[, "t value"]), lower.tail = FALSE) * 2

# Combine with OR results
results_df <- data.frame(
  Benefit = c("Improved management strategies", 
              "Understanding of population dynamics", 
              "Enhanced conservation outcomes"),
  OR = c(or_manage["familiar_bin", "OR"], 
         or_population["familiar_bin", "OR"], 
         or_conservation["familiar_bin", "OR"]),
  CI_lower = c(or_manage["familiar_bin", "2.5 %"], 
               or_population["familiar_bin", "2.5 %"], 
               or_conservation["familiar_bin", "2.5 %"]),
  CI_upper = c(or_manage["familiar_bin", "97.5 %"], 
               or_population["familiar_bin", "97.5 %"], 
               or_conservation["familiar_bin", "97.5 %"]),
  p_value = c(pval_manage["familiar_bin"], 
              pval_population["familiar_bin"], 
              pval_conservation["familiar_bin"])
)

print(results_df)


##################################

#familiarity and question 11 about trust 

# Recode binary trust outcomes from Q11 items
df <- df %>%
  mutate(
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0,
      TRUE ~ NA_real_
    ),
    trust_manage = ifelse(Q11_1 %in% c("Very high", "Somewhat high"), 1, 0),
    trust_institution = ifelse(Q11_2 %in% c("Very high", "Somewhat high"), 1, 0),
    trust_methods = ifelse(Q11_3 %in% c("Very high", "Somewhat high"), 1, 0)
  ) %>%
  filter(!is.na(familiar_bin))  # Only keep rows with familiarity scored

# Run GLMs
glm_trust_manage <- glm(trust_manage ~ familiar_bin, data = df, family = binomial)
glm_trust_institution <- glm(trust_institution ~ familiar_bin, data = df, family = binomial)
glm_trust_methods <- glm(trust_methods ~ familiar_bin, data = df, family = binomial)

# View summaries
summary(glm_trust_manage)
summary(glm_trust_institution)
summary(glm_trust_methods)

# Odds ratios with 95% confidence intervals
exp(cbind(OR = coef(glm_trust_manage), confint(glm_trust_manage)))
exp(cbind(OR = coef(glm_trust_institution), confint(glm_trust_institution)))
exp(cbind(OR = coef(glm_trust_methods), confint(glm_trust_methods)))


# Extract ORs and CIs for each GLM
or_trust_manage <- exp(cbind(OR = coef(glm_trust_manage), confint(glm_trust_manage)))[2, ]
or_trust_institution <- exp(cbind(OR = coef(glm_trust_institution), confint(glm_trust_institution)))[2, ]
or_trust_methods <- exp(cbind(OR = coef(glm_trust_methods), confint(glm_trust_methods)))[2, ]

# Create dataframe for plotting
trust_or_df <- data.frame(
  Trust_Context = c(
    "Suitability for managing wildlife", 
    "Institutional decision-making", 
    "Collection and analysis methods"
  ),
  OR = c(or_trust_manage["OR"], or_trust_institution["OR"], or_trust_methods["OR"]),
  CI_lower = c(or_trust_manage["2.5 %"], or_trust_institution["2.5 %"], or_trust_methods["2.5 %"]),
  CI_upper = c(or_trust_manage["97.5 %"], or_trust_institution["97.5 %"], or_trust_methods["97.5 %"])
)

# Plot
ggplot(trust_or_df, aes(x = OR, y = reorder(Trust_Context, OR))) +
  geom_point(size = 3, color = "red") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Effect of Familiarity on Trust in Genetic Data",
    x = "Odds Ratio (log scale)",
    y = "Trust Context"
  ) +
  scale_x_log10() +
  theme_minimal(base_size = 13)
#########################

#question 10 regarding concerns and using familiarity 

# Recode familiarity (Q16)
df <- df %>%
  mutate(
    familiar_bin = case_when(
      Q16 %in% c("Very familiar", "Somewhat familiar") ~ 1,
      Q16 %in% c("Neutral/Unsure", "Somewhat unfamiliar", "Very unfamiliar") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Recode challenge items (Q10_1 to Q10_8) into high concern (1) vs not (0)
df <- df %>%
  mutate(
    challenge_access = ifelse(Q10_1 %in% c("Very high", "Somewhat high"), 1, 0),
    challenge_complexity = ifelse(Q10_2 %in% c("Very high", "Somewhat high"), 1, 0),
    challenge_integration = ifelse(Q10_3 %in% c("Very high", "Somewhat high"), 1, 0),
    challenge_expertise = ifelse(Q10_4 %in% c("Very high", "Somewhat high"), 1, 0),
    challenge_time = ifelse(Q10_5 %in% c("Very high", "Somewhat high"), 1, 0),
    challenge_applicability = ifelse(Q10_6 %in% c("Very high", "Somewhat high"), 1, 0),
    challenge_funding = ifelse(Q10_7 %in% c("Very high", "Somewhat high"), 1, 0),
    challenge_technology = ifelse(Q10_8 %in% c("Very high", "Somewhat high"), 1, 0)
  ) %>%
  filter(!is.na(familiar_bin))

# Run GLMs
glm_challenges <- list(
  access = glm(challenge_access ~ familiar_bin, data = df, family = binomial),
  complexity = glm(challenge_complexity ~ familiar_bin, data = df, family = binomial),
  integration = glm(challenge_integration ~ familiar_bin, data = df, family = binomial),
  expertise = glm(challenge_expertise ~ familiar_bin, data = df, family = binomial),
  time = glm(challenge_time ~ familiar_bin, data = df, family = binomial),
  applicability = glm(challenge_applicability ~ familiar_bin, data = df, family = binomial),
  funding = glm(challenge_funding ~ familiar_bin, data = df, family = binomial),
  technology = glm(challenge_technology ~ familiar_bin, data = df, family = binomial)
)

# View all summaries
lapply(glm_challenges, summary)

# Extract ORs and CIs from each GLM
or_challenges <- map_dfr(glm_challenges, ~ {
  or_data <- exp(cbind(OR = coef(.x), confint(.x)))[2, ]  # 2 = familiar_bin
  tibble(
    OR = or_data["OR"],
    CI_lower = or_data["2.5 %"],
    CI_upper = or_data["97.5 %"]
  )
}, .id = "Challenge")

# Clean up challenge labels
or_challenges$Challenge <- c(
  "Access to DNA samples",
  "Analysis complexity",
  "Data integration",
  "Team expertise",
  "Time for analysis",
  "Applicability to conservation",
  "Funding availability",
  "Technology access"
)

ggplot(or_challenges, aes(x = OR, y = reorder(Challenge, OR))) +
  geom_point(size = 3, color = "firebrick") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_log10() +
  labs(
    title = "Effect of Familiarity on Concern About Genetic Data Challenges",
    x = "Odds Ratio (log scale)",
    y = "Challenge Type"
  ) +
  theme_minimal(base_size = 13)
######################################

##since the above was not significant then I ran this for question 10 regarding concerns and using familiarity 

# Count responses
q10_summary <- q10_long %>%
  group_by(Challenge, Concern) %>%
  summarize(count = n(), .groups = "drop")

# Optional: Order levels for consistent plotting
q10_summary$Concern <- factor(
  q10_summary$Concern,
  levels = c("Very low", "Somewhat low", "Neutral/Unsure", "Somewhat high", "Very high")
)

# Plot
library(ggplot2)

ggplot(q10_summary, aes(x = count, y = Challenge, fill = Concern)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "Overall Reported Concern About Genetic Data Challenges",
    x = "Number of Respondents",
    y = "Challenge",
    fill = "Level of Concern"
  ) +
  theme_minimal(base_size = 13)

q10_long %>%
  group_by(Challenge) %>%
  summarize(total_responses = n())

sum(is.na(df$Q10_4))


######################################

#demographics
# Load required libraries
library(dplyr)
library(knitr)
library(kableExtra)

# Step 1: Select and rename demographic variables
demographics <- dplyr::select(
  df,
  role = Q1,
  years_in_role = Q2,
  state = Q3,
  org_type = Q4,
  education = Q5,
  gender = Q7,
  age_range = Q8
)

# Step 2: Create summarizing function
summarize_demo <- function(var, var_name) {
  demographics %>%
    count({{var}}) %>%
    mutate(
      percent = round(100 * n / sum(n), 1),
      Variable = var_name
    ) %>%
    rename(Category = {{var}})
}

# Step 3: Apply function to each variable
summary_role <- summarize_demo(role, "Role")
summary_years <- summarize_demo(years_in_role, "Years in Role")
summary_state <- summarize_demo(state, "State")
summary_org <- summarize_demo(org_type, "Organization Type")
summary_edu <- summarize_demo(education, "Education")
summary_gender <- summarize_demo(gender, "Gender")
summary_age <- summarize_demo(age_range, "Age Range")

# Step 4: Combine all summaries
demo_summary <- bind_rows(
  summary_role,
  summary_years,
  summary_state,
  summary_org,
  summary_edu,
  summary_gender,
  summary_age
) %>%
  dplyr::select(Variable, Category, n, percent)

# Step 5: Print nicely
kable(demo_summary, align = "lcc", caption = "Demographic Summary of Survey Respondents") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover"))

###########################################


