# ---- lab_prep.R : cleans the ANTH306 lab dataset (synthetic) ----
# Students build this file step by step in R Lab 1 and R Lab 2.
# Any chapter's "Run It in Lab" section can start with:  source("lab_prep.R")
library(tidyverse)
library(readxl)

mh_raw <- read_excel("data/ANTH306_LayConceptionsMH_SYNTHETIC.xlsx", sheet = "data")

mh_clean <- mh_raw |>
  distinct(ResponseId, .keep_all = TRUE) |>                     # drop the duplicate submission
  filter(SERIOUS == 1, MIAQ_CHECK == 2) |>                      # keep serious + attentive responses
  mutate(across(c(where(is.numeric), -POLITICALBELIEFS),          # decline (-99) / don't know (-50) -> NA
                ~ replace(.x, .x %in% c(-99, -50), NA))) |>     # (POLITICALBELIEFS keeps its codes for the ggplot2 lab)
  mutate(
    AGE      = if_else(AGE < 18 | AGE > 100, NA, AGE),          # implausible ages -> NA
    STIG2_r  = 6 - STIG2,                                       # reverse-score item 2
    STIGMA   = rowMeans(across(c(STIG1, STIG2_r, STIG3)), na.rm = TRUE),
    EFFICACY = rowMeans(across(c(EFFICACY1, EFFICACY2, EFFICACY3)), na.rm = TRUE),
    SOCIAL   = rowMeans(across(c(LC_SOC1, LC_SOC2, LC_SOC3)), na.rm = TRUE),
    MEDICAL  = rowMeans(across(c(LC_MED1, LC_MED2, LC_MED3)), na.rm = TRUE),
    MIAQ_SUBSTANCE = rowMeans(across(c(MIAQ_SUB1, MIAQ_SUB2, MIAQ_SUB3)), na.rm = TRUE),
    WELLBEING = rowMeans(across(WELLBEING1:WELLBEING8), na.rm = TRUE),   # 8-item scales
    STRESS    = rowMeans(across(STRESS1:STRESS8), na.rm = TRUE),
    SUPPORT   = rowMeans(across(SUPPORT1:SUPPORT8), na.rm = TRUE),
    DISTRESS  = rowMeans(across(DISTRESS1:DISTRESS10), na.rm = TRUE),    # Kessler K10
    GENDER_2 = factor(case_when(GENDER == 1 ~ "Woman", GENDER == 2 ~ "Man"),    # other answers -> NA (too few people)
                      levels = c("Woman", "Man"))
  )
