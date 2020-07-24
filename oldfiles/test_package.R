devtools::install_github("PMSchulze/stmprevalence")
library(stmprevalence)
# ----------------------------------------------------------------------------------------------

# set working directory
setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")
# load data
data <- readRDS("./data/preprocessed_monthly.rds")
# load fitted topic model
mod_prev <- readRDS("./data/mod_prev_monthly.rds")
# load topic labels
topic_labels <- list(
  Topic1 = "right/nationalist",
  Topic2 = "miscellaneous_1",
  Topic3 = "green/climate",
  Topic4 = "social/housing",
  Topic5 = "Europe_english",
  Topic6 = "mobility",
  Topic7 = "Europe",
  Topic8 = "corona",
  Topic9 = "left/anti-war",
  Topic10 = "Twitter/politics_1",
  Topic11 = "Twitter/politics_2",
  Topic12 = "miscellaneous_2",
  Topic13 = "Twitter/politics_3",
  Topic14 = "right-wing extremism",
  Topic15 = "social/health"
)
# load list of prevalence covariates
varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
# load full names of prevalence covariates
varlist_fullnames <- c(
  "Time", "Party", "Federal State", "Immigrants (%)", "GDP per capita", 
  "Unemployement Rate (%)", "vote share (%)"
)

# For sample_props_logisticn we have to use the same formula as in stm
formula1 <- 1:15~Partei+ Bundesland + s(t) + s(Struktur_4) + 
  s(Struktur_22) + s(Struktur_42) + s(Struktur_54)
# For sample_coefs we can use a subset of this formula
formula2 <- 1:15~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5)

# ----------------------------------------------------------------------------------------------

props_logisticn <- sample_props_logisticn(mod_prev, "t", formula1, data$meta)
all_betas <- sample_coefs(mod_prev, formula2, type = "beta",
                          data$meta, nsims = 2, seed = 123)
all_quasibin <- sample_coefs(mod_prev, formula2, type = "quasibinomial",
                             data$meta, nsims = 2, seed = 123)
preds_beta <- predict_props(all_betas, "t", formula2, data$meta)
preds_quasibin <- predict_props(all_quasibin, "t", formula2, data$meta)
