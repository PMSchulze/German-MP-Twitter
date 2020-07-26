library(ggplot2)
library(bayesplot)
library(rstanarm)
library(stm)
devtools::install_github("PMSchulze/stmprevalence", force = TRUE)
library(stmprevalence)
theme_set(bayesplot::theme_default())

# ----------------------------------------------------------------------------------------------

# set working directory
setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")
# setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')
# load data
data <- readRDS("./data/topic_preprocessing/preprocessed_monthly.rds")
# load fitted topic model
mod_prev <- readRDS("./data/4/mod_prev_monthly.rds")
# load topic labels
topic_labels <- list(
  Topic1 = "Right/Nationalist",
  Topic2 = "Miscellaneous 1",
  Topic3 = "Climate Economics",
  Topic4 = "Social/Housing",
  Topic5 = "Digital/Future",
  Topic6 = "Climate Protection",
  Topic7 = "Europe",
  Topic8 = "Corona",
  Topic9 = "Left/Anti-war",
  Topic10 = "Twitter/Politics 1",
  Topic11 = "Twitter/Politics 2",
  Topic12 = "Miscellaneous 2",
  Topic13 = "Twitter/Politics 3",
  Topic14 = "Right-wing Extremism",
  Topic15 = "Society/Solidarity"
)
# load list of prevalence covariates
varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
# load full names of prevalence covariates
varlist_fullnames <- c(
  "Time", "Party", "Federal State", "Immigrants (%)", "GDP per capita", 
  "Unemployement Rate (%)", "Vote share (%)"
)
# select topics 1,4,6 and covariates
formula <- c(1,4,6)~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
metadata <- data$meta[varlist]
# factorize
metadata[sapply(metadata, is.character)] <- lapply(metadata[sapply(metadata, is.character)], 
                                                   as.factor)

# ----------------------------------------------------------------------------------------------

# Obtain MAP estimates of nsims Bayesian beta regressions
mod_betaregs <- stmprevalence::beta_bayes(mod_prev, formula, metadata, nsims = 2)

# Draw from posterior predictive distribution of previously obtained Beta regressions
topic_preds <- stmprevalence::posterior_predict_props(mod_betaregs, "t", formula, 
                                                      metadata, 0.025, 0.975)

# ----------------------------------------------------------------------------------------------

# Store sampled values
saveRDS(topic_preds, "./data/5_1/topic_preds.rds")
