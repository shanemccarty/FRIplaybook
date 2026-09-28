# ---- lab_prep.R : cleans the ANTH306 lab dataset (synthetic) ----
# Your instructor gives you this file with the lab data. Keep it in your project folder
# (next to your .Rproj, NOT inside data/). Every Lab from Transforming Your Data onward
# starts with:  source("lab_prep.R")   which runs this whole file and creates mh_clean.
# It imports the data, keeps serious and attentive responses, turns -99/-50 into NA,
# reverses the _R items, averages each multi-item measure, and builds the categorical
# variables from Transforming Your Data. Add your own lines at the bottom as the Labs ask.
library(tidyverse)
library(readxl)

mh_raw <- read_excel("data/ANTH306_LayConceptionsMH_SYNTHETIC.xlsx")   # the data are on the first sheet (Sheet1), so no sheet = is needed

mh_clean <- mh_raw |>
  distinct(ResponseId, .keep_all = TRUE) |>                     # drop the duplicate submission
  filter(SERIOUS == 1, MIAQ_CHECK == 2) |>                      # keep serious + attentive responses
  mutate(across(where(is.numeric),                              # decline (-99) / don't know (-50) -> NA, everywhere
                ~ replace(.x, .x %in% c(-99, -50), NA))) |>     # (the same step you did by hand in Import Data Once)
  mutate(
    AGE      = if_else(AGE < 18 | AGE > 100, NA, AGE),          # implausible ages -> NA
    across(ends_with("_R"), ~ 6 - .x, .names = "{.col}ev"),      # reverse items (_R) -> *_Rev, in the same direction as the rest
    STIGMA_PUB  = rowMeans(across(c(STIGMA_PUB1, STIGMA_PUB2, STIGMA_PUB3, STIGMA_PUB4_Rev)), na.rm = TRUE),
    STIGMA_SELF = rowMeans(across(c(STIGMA_SELF1_Rev, STIGMA_SELF2, STIGMA_SELF3, STIGMA_SELF4_Rev)), na.rm = TRUE),
    STIGMA      = rowMeans(across(c(STIGMA_PUB1, STIGMA_PUB2, STIGMA_PUB3, STIGMA_PUB4_Rev,
                                    STIGMA_SELF1_Rev, STIGMA_SELF2, STIGMA_SELF3, STIGMA_SELF4_Rev)), na.rm = TRUE),
    EFFICACY = rowMeans(across(c(EFFICACY1, EFFICACY2, EFFICACY3)), na.rm = TRUE),
    SOCIAL   = rowMeans(across(c(LC_SOC1, LC_SOC2, LC_SOC3)), na.rm = TRUE),
    MEDICAL  = rowMeans(across(c(LC_MED1, LC_MED2, LC_MED3)), na.rm = TRUE),
    MIAQ_SUBSTANCE = rowMeans(across(c(MIAQ_SUB1, MIAQ_SUB2, MIAQ_SUB3)), na.rm = TRUE),
    WELLBEING = rowMeans(across(WELLBEING1:WELLBEING8), na.rm = TRUE),   # 8-item scales
    STRESS    = rowMeans(across(STRESS1:STRESS8), na.rm = TRUE),
    SUPPORT   = rowMeans(across(SUPPORT1:SUPPORT8), na.rm = TRUE),
    DISTRESS  = rowMeans(across(DISTRESS1:DISTRESS10), na.rm = TRUE),    # Kessler K10, item mean (1-5)
    K10_TOTAL = rowSums(across(DISTRESS1:DISTRESS10))                     # Kessler K10, total (10-50) as scored by Kessler et al.
  ) |>
  # ---- categorical variables built in Transforming Your Data (Plays 2 and 3) ----
  mutate(
    DISTRESS_4CAT = factor(case_when(K10_TOTAL <= 19 ~ "Well",              # K10 bands (Andrews & Slade, 2001)
                                     K10_TOTAL <= 24 ~ "Mild",
                                     K10_TOTAL <= 29 ~ "Moderate",
                                     K10_TOTAL >= 30 ~ "Severe"),
                           levels = c("Well", "Mild", "Moderate", "Severe")),
    DISTRESS_01CAT = if_else(K10_TOTAL >= 20, 1, 0),                        # 1 = likely distressed (20+)
    RACIALIZED_N   = rowSums(!is.na(across(RACIALIZED_1:RACIALIZED_8))),    # how many boxes were checked (99 = declined, not counted)
    RACIALIZED_6CAT = factor(case_when(RACIALIZED_N >= 2       ~ "Two or more",
                                       !is.na(RACIALIZED_7)    ~ "White",
                                       !is.na(RACIALIZED_2)    ~ "Asian",
                                       !is.na(RACIALIZED_3)    ~ "Black",
                                       !is.na(RACIALIZED_4)    ~ "Hispanic or Latine",
                                       RACIALIZED_N == 1       ~ "Another identity"),  # codes 1, 5, 6, 8: too few to stand alone
                             levels = c("White", "Asian", "Black", "Hispanic or Latine", "Another identity", "Two or more")),
    RACIALIZED_01CAT = case_when(RACIALIZED_6CAT == "White" ~ 0,          # 0 = racialized as white
                                 !is.na(RACIALIZED_6CAT)    ~ 1)           # 1 = racialized as a person of color
  )
