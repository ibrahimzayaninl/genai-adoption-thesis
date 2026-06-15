library(tidyverse)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- 1. LOAD RAW DATA ----

t0_raw <- read_excel("Microsoft Copilot User Adoption Survey T0.xlsx")
t1_raw <- read_excel("Microsoft Copilot User Adoption Survey Post-Experiment.xlsx")

cat("Raw T0:", nrow(t0_raw), "rows\n")
cat("Raw T1:", nrow(t1_raw), "rows\n")


# ---- 2. CLEAN COLUMN NAMES ----


rename_to_codes <- function(df, wave) {
  df %>%
    rename(
      id            = Id,
      consent       = `I give my consent`,
      dept          = `Which department or function do you primarily work in?`,
      role          = `Which best describes your role?`,
      tenure        = `How long have you worked at the company?`,
      copilot_access = `Do you currently have access to Microsoft Copilot in your work environment?`,
      genai_use     = `How often have you used public GenAI tools such as ChatGPT for work in the past month?`,
      copilot_use   = `How often have you used Microsoft Copilot for work in the past month?`,
      prior_training = `Before this study, have you already received any Copilot-related guidance, training, or demonstration?`,
      age           = `What is your age group?`,
      gender        = `What is your gender?`,
      TR1 = starts_with("Trust in Microsoft Copilot.I consider"),
      TR2 = starts_with("Trust in Microsoft Copilot.I trust"),
      TR3 = starts_with("Trust in Microsoft Copilot.Microsoft Copilot behaves"),
      TR4 = starts_with("Trust in Microsoft Copilot.I feel confident"),
      IU1 = starts_with("Intention to Use Microsoft Copilot.I intend"),
      IU2 = starts_with("Intention to Use Microsoft Copilot.I expect"),
      IU3 = starts_with("Intention to Use Microsoft Copilot.I plan"),
      PU1 = starts_with("Perceived Usefulness.Using Microsoft Copilot would improve"),
      PU2 = starts_with("Perceived Usefulness.Using Microsoft Copilot would help"),
      PU3 = starts_with("Perceived Usefulness.Using Microsoft Copilot would increase"),
      PEOU1 = starts_with("Perceived Ease of Use.Learning"),
      PEOU2 = starts_with("Perceived Ease of Use.It would be easy"),
      PEOU3 = starts_with("Perceived Ease of Use.My interaction"),
      SI1 = starts_with("Social Influence.People"),
      SI2 = starts_with("Social Influence.My manager"),
      FC1 = starts_with("Facilitating Conditions.I have the resources"),
      FC2 = starts_with("Facilitating Conditions.I can get the help")
    )
}

t0_raw <- rename_to_codes(t0_raw, "T0")
t1_raw <- rename_to_codes(t1_raw, "T1")

# Rename T1-specific columns
t1_raw <- t1_raw %>%
  rename(
    attention_check = starts_with("Attention Check"),
    newsletter_read = starts_with("Did you read the Copilot newsletter"),
    workshop_attended = starts_with("Did you attend the Copilot workshop"),
    MC1 = starts_with("Manipulation Checks.I learned practical"),
    MC2 = starts_with("Manipulation Checks.During the workshop"),
    MC3 = starts_with("Manipulation Checks.The workshop helped"),
    MC4 = starts_with("Manipulation Checks.I feel more confident"),
    license_intent = starts_with("Do you intend to request")
  )


# ---- 3. CONVERT LIKERT TEXT TO NUMERIC ----

likert_to_num <- function(x) {
  as.numeric(str_extract(as.character(x), "^[0-9]"))
}

likert_items <- c("TR1","TR2","TR3","TR4",
                  "IU1","IU2","IU3",
                  "PU1","PU2","PU3",
                  "PEOU1","PEOU2","PEOU3",
                  "SI1","SI2",
                  "FC1","FC2")

t0_raw <- t0_raw %>%
  mutate(across(all_of(likert_items), likert_to_num))

t1_raw <- t1_raw %>%
  mutate(across(all_of(c(likert_items, "MC1","MC2","MC3","MC4")), likert_to_num))

# Also convert attention check in T1
t1_raw <- t1_raw %>%
  mutate(attention_check_num = likert_to_num(attention_check))


# ---- 4. EXCLUSIONS: T0 ----

# Step 1: Remove no-consent rows
t0_consent <- t0_raw %>% filter(consent == "Yes, I agree")
cat("\nT0 after removing no-consent:", nrow(t0_consent), "rows\n")

# Step 2: Remove pilot/test entries (submitted before May 5, 2026)

pilot_ids <- c(1, 2, 3)  # Pre-launch entries (April 22-23); other 2 pilots in separate file

t0_clean <- t0_consent %>%
  filter(!id %in% pilot_ids)

cat("T0 after removing pilot entries (IDs", paste(pilot_ids, collapse=", "), "):", nrow(t0_clean), "rows\n")

# Step 3: Handle missing Likert items

missing_check <- t0_clean %>%
  select(id, all_of(likert_items)) %>%
  mutate(n_missing = rowSums(is.na(across(all_of(likert_items))))) %>%
  filter(n_missing > 0)

cat("\nT0 rows with missing Likert values:\n")
print(missing_check %>% select(id, n_missing))


# ---- 5. EXCLUSIONS: T1 ----

# Step 1: Remove respondent who did not attend the workshop (ID 33)
t1_workshop <- t1_raw %>% filter(workshop_attended == "Yes")
cat("\nT1 after removing non-workshop attendee:", nrow(t1_workshop), "rows\n")

# Step 2: Remove attention check failures (instructed response = 5)

t1_clean <- t1_workshop %>% filter(attention_check_num == 5)
cat("T1 after removing attention check failures:", nrow(t1_clean), "rows\n")

# Report which IDs were excluded and why
excluded_att <- t1_workshop %>%
  filter(attention_check_num != 5) %>%
  select(id, attention_check, attention_check_num)
cat("Excluded for attention check failure:\n")
print(excluded_att)

# Step 3: Handle missing items in T1


t1_clean <- t1_clean %>%
  mutate(
    PEOU_mean_available = rowMeans(select(., PEOU1, PEOU2, PEOU3), na.rm = TRUE),
    PEOU1 = ifelse(is.na(PEOU1), PEOU_mean_available, PEOU1),
    PEOU2 = ifelse(is.na(PEOU2), PEOU_mean_available, PEOU2),
    PEOU3 = ifelse(is.na(PEOU3), PEOU_mean_available, PEOU3)
  ) %>%
  select(-PEOU_mean_available)

cat("\nT1 missing values after imputation:\n")
print(t1_clean %>%
  select(id, all_of(likert_items)) %>%
  summarise(across(everything(), ~sum(is.na(.)))))


# ---- 6. COMPUTE COMPOSITE SCORES ----

compute_composites <- function(df) {
  df %>%
    mutate(
      TR_comp   = rowMeans(select(., TR1, TR2, TR3, TR4),       na.rm = TRUE),
      IU_comp   = rowMeans(select(., IU1, IU2, IU3),            na.rm = TRUE),
      PU_comp   = rowMeans(select(., PU1, PU2, PU3),            na.rm = TRUE),
      PEOU_comp = rowMeans(select(., PEOU1, PEOU2, PEOU3),      na.rm = TRUE),
      SI_comp   = rowMeans(select(., SI1, SI2),                  na.rm = TRUE),
      FC_comp   = rowMeans(select(., FC1, FC2),                  na.rm = TRUE)
    )
}

t0_clean <- compute_composites(t0_clean)
t1_clean <- compute_composites(t1_clean)


# ---- 7. ADD WAVE INDICATOR ----

t0_clean <- t0_clean %>% mutate(wave = 0)
t1_clean <- t1_clean %>% mutate(wave = 1)


# ---- 8. FINAL SAMPLE SIZES ----

cat("\n========== FINAL SAMPLE SIZES ==========\n")
cat("T0 clean N:", nrow(t0_clean), "\n")
cat("T1 clean N:", nrow(t1_clean), "\n")

cat("\nT0 composite score summary:\n")
print(t0_clean %>%
  select(TR_comp, IU_comp, PU_comp, PEOU_comp, SI_comp, FC_comp) %>%
  summary())

cat("\nT1 composite score summary:\n")
print(t1_clean %>%
  select(TR_comp, IU_comp, PU_comp, PEOU_comp, SI_comp, FC_comp) %>%
  summary())


# ---- 9. SAVE CLEANED DATASETS ----

write_csv(t0_clean, "t0_clean.csv")
write_csv(t1_clean, "t1_clean.csv")


# ---- STANDARDISE CATEGORICAL VARIABLES ----
 
standardise_categories <- function(df) {
  df %>%
    mutate(
      # Collapse departments: Sales / IT / Finance / Other
      # ikam (International Key Account Management) grouped with Sales
      dept_std = case_when(
        str_to_lower(dept) %in% c(
          "sales",
          "sales / account support / finance",
          "ikam"
        ) ~ "Sales",
        str_to_lower(dept) == "it"      ~ "IT",
        str_to_lower(dept) == "finance" ~ "Finance",
        TRUE                            ~ "Other"
      ),
      # Collapse age: Under 25 and 25-34 merged into Under 35
      age_std = case_when(
        age %in% c("Under 25", "25-34") ~ "Under 35",
        age == "35-44"                  ~ "35-44",
        age == "45-54"                  ~ "45-54",
        age == "55+"                    ~ "55+"
      ),
      dept_std = factor(dept_std, levels = c("Sales", "IT", "Finance", "Other")),
      age_std  = factor(age_std,  levels = c("Under 35", "35-44", "45-54", "55+")),
      role     = factor(role,     levels = c("Employee", "Manager", "Specialist", "Other")),
      tenure   = factor(tenure,   levels = c("0-2 Years", "3-7 Years", "7+ Years")),
      gender   = factor(gender,   levels = c("Female", "Male"))
    )
}

t0_clean <- standardise_categories(t0_clean)
t1_clean <- standardise_categories(t1_clean)

# Re-save with standardised columns included
write_csv(t0_clean, "t0_clean.csv")
write_csv(t1_clean, "t1_clean.csv")