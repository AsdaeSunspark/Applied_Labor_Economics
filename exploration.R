library(tidyverse)
library(haven)
library(arrow)

data <- read_dta("data/transition_0207.dta")
# save(data, file = "data/transition_0207.rds")
# 
# data <- readRDS("data/transition_0207.rds")

pq_path <- "data/transition_0207"
data %>%
  group_by(an) %>%
  write_dataset(path = pq_path, format = "parquet")

rm(data)
gc()
data <- open_dataset(pq_path)
data %>%
  filter(an == 2007) %>%
  head(10) %>%
  pull()
  collect()
