library(tidyverse)
library(data.table)

# Load data
data <- read_dta("data/transition_0207.dta")

# Load functions
source("tilde_exptV.R")
source("power_method.R")
source("compute_M.R")
source("sparse_to_csr.R")
source("sconn_ee_function.R")
source("scomponents.R")
source("estimable_subset.R")
source("given_lambda_ee_gap_specific.R")

# Get rid of singleton elements
workerAppearanceCount <- table(data$nninouv)
listSingletonWorkers <- workerAppearanceCount[workerAppearanceCount == 1]
listSingletonWorkers <- names(listSingletonWorkers)
rankWorkers <- frank(data$nninouv, ties.method = "random")
indexWorkers <- sort(data$nninouv)
listSingletonWorkers <- sort(listSingletonWorkers)

j <- 1
keepWorker <- rep(TRUE, length(indexWorkers))
for (i in 1:length(listSingletonWorkers)) {
  while (listSingletonWorkers[i] != indexWorkers[j]) {
    j <- j + 1
  }
  while ((j <= length(indexWorkers)) & (indexWorkers[j] == listSingletonWorkers[i])) {
    keepWorker[j] <- FALSE
    j <- j + 1
  }
}

keepWorker <- keepWorker[rankWorkers]
data <- data[keepWorker,]

rm("indexWorkers", "keepWorker", "listSingletonWorkers", "rankWorkers", "workerAppearanceCount")
gc()

# dataM <- data %>% filter(male == 1)
# dataF <- data %>% filter(male == 0)

# data <- dataF

# Build sector (sparse) matrix
sector <- tibble(
  firm_id = data$index1,
  sector = data$sector1a
)

sector <- sector %>%
  filter(!str_equal(sector, "") & !is.na(sector)) %>%
  count(firm_id, sector) %>%
  group_by(firm_id) %>%
  slice_max(n, n = 1, with_ties = FALSE)

sector <- sector %>%
  select("firm_id", "sector") %>%
  ungroup() %>%
  arrange(firm_id)

# Extract interesting variables
year1 <- data$year1
year2 <- data$year2
ee <- data$ee
ene <- data$ene
oldIndex1 <- data$index1
oldIndex2 <- data$index2

rm("data")
gc()

# Transform character indices into integer indices
rankOldIndex1 <- frank(oldIndex1, ties.method = "random")
rankOldIndex2 <- frank(oldIndex2, ties.method = "random")
index1 <- rep(0, length(rankOldIndex1))
index1[rankOldIndex1] <- oldIndex1
index2 <- rep(0, length(rankOldIndex2))
index2[rankOldIndex2] <- oldIndex2

index <- c("Unemployment", sort(unique(c(index1, index2))))
write.csv2(index, file = "data/index.csv", row.names = FALSE)

maxindex <- length(index)

sectorList <- rep(NA, length(index))
j <- 1
for (i in 1:length(index)) {
  if (str_equal(sector$firm_id[j], index[i])) {
    sectorList[i] <- sector$sector[j]
    j <- j + 1
  }
  if (j > length(sector$sector)) {
    break
  }
}

indexSector <- sort(unique(sectorList))
for (i in 1:length(sectorList)) {
  for (j in 1:length(indexSector)) {
    if (!is.na(sectorList[i]) & (sectorList[i] == indexSector[j])) {
      sectorList[i] <- j
    }
  }
}

sectorList[!is.na(sectorList) & (sectorList == 15)] <- 14

write.csv2(sectorList, "data/sector.csv", row.names = FALSE)
write.csv2(indexSector, "data/indexSector.csv", row.names = FALSE)

j1 <- 1
j2 <- 1
for (i in 1:length(index)) {
  while ((j1 <= length(index1)) & (index[i] == index1[j1])) {
    index1[j1] <- i
    j1 <- j1 + 1
  }
  while ((j2 <= length(index1)) & (index[i] == index2[j2])) {
    index2[j2] <- i
    j2 <- j2 + 1
  }
}

index1 <- as.integer(index1)[rankOldIndex1]
index2 <- as.integer(index2)[rankOldIndex2]

rm("rankOldIndex1", "rankOldIndex2", "oldIndex1", "oldIndex2", "index", "sector")
gc()


# Initialise the exogenous weight

### Compute employer size each year

ee_store <- rep(0, 6*maxindex)
en_store <- rep(0, 6*maxindex)
size_store <- rep(0, 6*maxindex)

for (i in 1:length(index1)) {
  if (!is.na(ee[i]) & (ee[i] == 1)) {
    ee_store[index1[i] + maxindex*(year1[i] - 2002)] <- ee_store[index1[i] + maxindex*(year1[i] - 2002)] + 1
  }
  if (!is.na(ene[i]) & (ene[i] == 1)) {
    en_store[index1[i] + maxindex*(year1[i] - 2002)] <- en_store[index1[i] + maxindex*(year1[i] - 2002)] + 1
  }
  size_store[index1[i] + maxindex*(year1[i] - 2002)] <- size_store[index1[i] + maxindex*(year1[i] - 2002)] + 1
}

ee_store <- matrix(ee_store, ncol = 6)
en_store <- matrix(en_store, ncol = 6)
size_store <- matrix(size_store, ncol = 6)

### Compute g

g_level <- rowSums(size_store[, 1:5])
W <- sum(g_level)
g_share <- g_level / W

### Compute growth rate by index by year

dhs_growth <- matrix(rep(0, 5 * maxindex), ncol = 5)

for (i in 1:5) {
  dhs_growth[, i] <- 2 * (size_store[, i + 1] - size_store[, i]) / max(size_store[, i] + size_store[, i + 1], 0.05)
}

contract_indicator <- (dhs_growth < 0)
grow_indicator <- (dhs_growth >= 0)

rm("dhs_growth")
gc()

### Compute EE and EN rate growth

size_temp <- rowSums(size_store[, 1:5] * grow_indicator)
size_temp <- if_else(size_temp == 0, 1, size_temp)

ee_rate_growth <- rowSums(ee_store[, 1:5] * grow_indicator) / size_temp
en_rate_growth <- rowSums(en_store[, 1:5] * grow_indicator) / size_temp

rm("grow_indicator", "size_temp")
gc()

# Compute excess
size_temp <- size_store[, 1:5] * contract_indicator
size_temp <- if_else(size_temp == 0, 1, size_temp)

ee_rate_contract <- ee_store[, 1:5] * contract_indicator / size_temp
en_rate_contract <- en_store[, 1:5] * contract_indicator / size_temp

rm("contract_indicator", "size_temp")
gc()

# Get sector-wise counterfactuals EE and EN rates

ee_counterfactual <- ee_rate_growth
en_counterfactual <- en_rate_growth
g_level_sector <- rep(0, length(indexSector) - 1) # We merged sector 15 (one company) with sector 14
mean_ee <- rep(0, length(indexSector) - 1)
mean_en <- rep(0, length(indexSector) - 1)

for (i in 1:length(g_level)) {
  if (!is.na(sectorList[i])) {
    mean_ee[sectorList[i]] <- mean_ee[sectorList[i]] + ee_counterfactual[i] * g_level[i]
    mean_en[sectorList[i]] <- mean_en[sectorList[i]] + en_counterfactual[i] * g_level[i]
    g_level_sector[sectorList[i]] <- g_level_sector[sectorList[i]] + g_level[i]
  }
}

mean_ee <- mean_ee / g_level_sector
mean_en <- mean_en / g_level_sector

for (i in 1:length(g_level)) {
  if (!is.na(sectorList[i])) {
    ee_counterfactual[i] <- mean_ee[sectorList[i]]
    en_counterfactual[i] <- mean_en[sectorList[i]]
  }
}

# Saving workspace for the first time

# Initialise

answer <- compute_M(ee_rate_contract, en_rate_contract, ee, ene, index1, index2, year1, maxindex, W, g_level, ee_counterfactual, en_counterfactual)
M <- answer$M
Mo <- answer$Mo
rho_i_rate <- answer$rho_i_rate
delta_i_rate <- answer$delta_i_rate
fo_level <- answer$fo_level

sconn_ee <- sconn_ee_function(answer$M, maxindex)

answer <- tilde_exptV(M, maxindex)
ID <- answer$ID
exptV <- answer$exptV

Mo_emp <- Mo[(Mo[, 1] != 1) & (Mo[, 2] != 1), ]
answer <- tilde_exptV(Mo_emp, maxindex)
ID_ee_all <- answer$ID
exptV_ee_all <- answer$exptV

answer <- estimable_subset(M, exptV, ID, g_level, sconn_ee, fo_level, rho_i_rate, delta_i_rate, sectorList, maxindex)

M_r <- answer$M_r
exptV_r <- answer$exptV_r
g_level_r <- answer$g_level_r
fo_level_r <- answer$fo_level_r
ID_r <- answer$ID_r
rho_r <- answer$rho_r
delta_r <- answer$delta_r
rho_sector_r <- answer$rho_sector_r
delta_sector_r <- answer$delta_sector_r
sectorList_r <- answer$sectorList_r

write.csv(ID_r, "data/ID_r.csv")
write.csv(ID, "data/ID.csv")


