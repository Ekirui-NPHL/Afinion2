setwd("C:/Users/Elvis/Documents/2026/FnQ/VL")

rm(list = ls())
library(tidyverse)
library(janitor)
library(tidyverse)
library(tidyverse)
library(janitor)
library(readr)
library(lubridate)
files <- list.files(pattern = "VL TEST OUTCOMES FOR.*IN.*csv", full.names = TRUE)
all_data <- files %>% map_df(~read_csv(.x) %>% mutate(source = basename(.x)))

write_csv(all_data, "combined_VL_data.csv")
cat("Combined", length(files), "files,", nrow(all_data), "total rows")

VL_2024 <- all_data|> clean_names()
names(VL_2024)
