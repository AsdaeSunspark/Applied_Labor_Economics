library(tidyverse)
library(data.table)

index <- read.csv2("data/index.csv")

df <- data %>%
  left_join(index %>% mutate(newIndex = row_number()), by = c("index1" = "x"))

df <- df %>%
  filter(newIndex %in% ID_r)

df <- data.table(df)

lm(logyh1 ~ age1 + factor(nninouv), data = df)

data %>% View()

df <- df %>%
  select("nninouv", "index1", "male", "age1", "logyh1")

df <- df %>%
  mutate(
    age1 = (age1 - 40) / 40,
    age2 = age1^2,
    age3 = age1^3
  )

df %>%
  write.csv("data/dfAKM.csv")
