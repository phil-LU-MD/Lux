library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)


LU_25 <- read_excel("input/rnrpp-nationalite-commune.xlsx")


LU <- LU_25 %>% group_by(COMMUNE_NOM) %>% 
  slice_max(order_by = NOMBRE_TOTAL, n = 1) %>% #keeps only the row with the highest count in each group.
  ungroup()

LU_top <- LU_25 %>% group_by(COMMUNE_NOM) %>% 
  slice_max(order_by = NOMBRE_TOTAL, n = 2) %>%
  ungroup()

LU_top2 <- LU_25 %>%
  group_by(COMMUNE_NOM) %>%
  arrange(desc(NOMBRE_TOTAL)) %>%
  slice(2) %>%
  ungroup()

LU_top3 <- LU_25 %>%
  group_by(COMMUNE_NOM) %>%
  arrange(desc(NOMBRE_TOTAL)) %>%
  slice(3) %>%
  ungroup()
