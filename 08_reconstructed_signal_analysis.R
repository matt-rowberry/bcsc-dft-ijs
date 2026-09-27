#####

## POOLE'S CAVERN — RECONSTRUCTED SIGNAL
## 45.5–50.5 cpd ONLY
## Based on the original processing pipeline

#####

#####

## 01. LOAD LIBRARIES

library(openxlsx)
library(tidyr)
library(dplyr)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)
library(writexl)
library(extrafont)
library(patchwork)
library(cowplot)

#####

## 02. LOAD DATA

df <- read.xlsx("data/kalman_residuals_full_imp.xlsx")

df$dttm <- as.POSIXct(
    (df$dttm - 25569) * 86400,
    origin = "1970-01-01",
    tz = "UTC"
)

period <- df$period

df <- subset(
    df,
    select = -c(period)
)

options(digits = 16)

Sys.setlocale(
    "LC_TIME",
    "English_United Kingdom"
)

fs_cpd <- 144

source("bandpass_functions.R")

#####

## 03. PROCESS DATA

res <- process_day_night_multiband(
    df,
    timestamp_col = "dttm",
    fs_cpd = fs_cpd
)

#####

## 04. RESTORE ACCESS-REGIME INFORMATION

res$full_series$period <- period

#####

## 05. RETAIN ONLY THE 45.5–50.5 CPD SIGNAL

reconstructed_full <- res$full_series %>%
    select(
        timestamp,
        period,
        daynight,
        ent_residual_nb3,
        ten_residual_nb3,
        sin_residual_nb3,
        brg_residual_nb3,
        mqs_residual_nb3,
        top_residual_nb3
    ) %>%
    rename(
        dttm = timestamp
    )

#####

## 06. CHECK OUTPUT

names(reconstructed_full)

message(
    "Rows: ",
    nrow(reconstructed_full)
)

summary(
    reconstructed_full$ent_residual_nb3
)

sd(
    reconstructed_full$ent_residual_nb3,
    na.rm = TRUE
)

#####

## 07. SAVE

saveRDS(
    reconstructed_full,
    "data/reconstructed_45_5_50_5_cpd.rds"
)

#####
