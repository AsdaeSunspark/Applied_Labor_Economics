df <- read_dta("data/akm.dta")

df <- read_csv("data/akm.csv")

dfV <- tibble(
  firm_id = ID_r,
  V = log(exptV_r)
)

index <- read_csv2("data/index.csv")

df %>%
  filter(!is.na(psi_male) | !is.na(psi_female)) %>%
  pivot_longer(cols = starts_with("psi"), names_to = "gender") %>%
  filter(!is.na(value)) %>%
  distinct(index1, gender, value) %>%
  pivot_wider(id_cols = index1, names_from = gender, values_from = value) %>%
  left_join(index %>% mutate(firm_id = row_number()) %>% rename(index1 = x)) %>%
  left_join(dfV) %>% 
  select(psi_male, psi_female, V) %>%
  write_csv("data/akm.csv")

df <- read_csv("data/akm.csv")

df %>%
  filter(!is.na(psi_female), !is.na(V))
