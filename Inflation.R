library(readr)
library(tidyr)
library(dplyr)

setwd("C:/Users/cepe-s4-05/Documents/Révisions_20250512")

df <- read_tsv("estat_ei_cphi_m.tsv" )
head(df)

# Séparation de la première colonne en plusieurs
col_names <- strsplit(names(df), ",")[[1]]
df_sep <- df %>% 
  separate(`freq,unit,s_adj,indic,geo\\TIME_PERIOD`, into = col_names, sep = ",")

colnames(df_sep)[5] <- "geo"

# Suppression des "d"
df_clean <- df_sep %>%
  mutate(across(-c(1:5), ~ parse_number(.)))

sapply(df_clean, class)
table(df_clean[,"freq"])
table(df_clean[,"unit"])
table(df_clean[,"s_adj"])
table(df_clean[,"indic"])
table(df_clean[,"geo"])
