library(tidyverse)
library(data.table)
library(haven)

# Load data
data <- read_dta("data/transition_0207.dta")
psi <- read_dta("data/akm.dta")

# Keep only the psis we successfully computed
psi <- psi %>%
  pivot_longer(starts_with("psi"), names_to = "gender") %>%
  filter(!is.na(value)) %>%
  group_by(index1, gender, value) %>%
  slice_max(n, with_ties = FALSE) %>%
  pivot_wider(id_cols = c("index1"), names_from = gender, values_from = value)

# Compute male and female shares in each firm
g_level <- data %>%
  select(index1, male) %>%
  group_by(index1) %>%
  summarise(
    shareMale = sum(male),
    shareFemale = sum(1 - male)
  ) %>%
  mutate(
    shareMale = shareMale / sum(shareMale),
    shareFemale = shareFemale / sum(shareFemale)
  )

# Merge data frame and compute gender gap
df <- psi %>%
  left_join(g_level)

df %>%
  ungroup() %>%
  summarise(
    avgPsiMale = weighted.mean(psi_male, shareMale, na.rm = TRUE),
    avgPsifemale = weighted.mean(psi_female, shareFemale, na.rm = TRUE)
  )

df %>%
  ungroup() %>%
  filter(!is.na(psi_female & !is.na(psi_male))) %>%
  summarise(
    avgPsiMale = weighted.mean(psi_male, shareMale, na.rm = TRUE),
    avgPsifemale = weighted.mean(psi_female, shareFemale, na.rm = TRUE)
  )

df %>%
  ungroup() %>%
  filter(!is.na(psi_female) & !is.na(psi_male)) %>%
  mutate(
    shareMale = shareMale / sum(shareMale),
    shareFemale = shareFemale / sum(shareFemale)
  ) %>%
  summarise(
    sortingMaleWeights = sum(psi_male * (shareMale - shareFemale)),
    discMaleWeights = summarise((psi_male - psi_female) * shareFemale),
    sortingMaleWeights = sum(-1 * psi_female * (shareFemale - shareMale)),
    discMaleWeights = summarise(-1 * (psi_female - psi_male) * shareMale)
  )

data %>%
  group_by(nninouv, male) %>%
  summarise(logyh1 = mean(logyh1)) %>%
  group_by(male) %>%
  summarise(logyh1 = mean(logyh1))