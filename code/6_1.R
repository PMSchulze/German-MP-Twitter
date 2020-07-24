# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "betareg", "ggcorrplot", "grid", "gridExtra", "huge", "knitr", "mvtnorm", 
  "quanteda", "reshape2", "scales", "stm", "stmprevalence", "stringi", "tidyverse", "tm"
)
not_installed <- packages_required[!packages_required %in%
                                     installed.packages()[, "Package"]]
if (length(not_installed) > 0) {
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = itype
  )
}
lapply(packages_required, library, character.only = TRUE)

# set working directory
setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')
# setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")

# load data
data <- readRDS("./data/preprocessed_monthly.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Model Fitting ---------------------------------------
# ----------------------------------------------------------------------------------------------

# choose number of topics
K <- 15

# # fit model (no prevalence structure, thus also no gamma prior)
# mod_ctm <- stm::stm(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = K,
#   # prevalence = prevalence,
#   # gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 300,
#   init.type = "Spectral")
# saveRDS(mod_ctm, "../data/mod_ctm_monthly.rds")

mod_ctm <- readRDS("./data/mod_ctm_monthly.rds")
mod_prev <- readRDS("./data/mod_prev_monthly.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Comparison with STM ---------------------------------
# ----------------------------------------------------------------------------------------------

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
  "Unemployement Rate (%)", "vote share (%)"
)

formula <- 1:15~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)

# load prevalence structure of mod_prev
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) +
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~"))

# comparison of heldout likelihoods
## prepare dataframe of heldout documents
heldout <- make.heldout(
  documents = data$documents,
  vocab = data$vocab,
  N = floor(0.1 * length(data$documents)), # hold out words from 10% of all documents
  proportion = 0.5, # for each of the documents within these 10%, hold out 50% of all words
  seed = 123)

## train STM and CTM on non-heldout documents
# mod_prev_heldout <- stm::stm(
#                       documents = heldout$documents,
#                       vocab = heldout$vocab,
#                       data = data$meta,
#                       K = K,
#                       prevalence = prevalence,
#                       gamma.prior = 'L1',
#                       seed = 123,
#                       max.em.its = 300,
#                       init.type = "Spectral")
# saveRDS(mod_prev_heldout, "../data/mod_prev_heldout_monthly.rds")
mod_prev_heldout <- readRDS("./data/mod_prev_heldout_monthly.rds")

# mod_ctm_heldout <- stm::stm(
#                       documents = heldout$documents,
#                       vocab = heldout$vocab,
#                       data = data$meta,
#                       K = K,
#                       # prevalence = prevalence,
#                       # gamma.prior = 'L1',
#                       seed = 123,
#                       max.em.its = 300,
#                       init.type = "Spectral")
# saveRDS(mod_ctm_heldout, "../data/mod_ctm_heldout_monthly.rds")
mod_ctm_heldout <- readRDS("./data/mod_ctm_heldout_monthly.rds")

## compare heldout likelihoods of STM and CTM
eval.heldout(mod_prev_heldout, heldout$missing)[["expected.heldout"]] # heldout likelihood of STM (model with prevalence)
eval.heldout(mod_ctm_heldout, heldout$missing)[["expected.heldout"]] # heldout likelihood of CTM (model without prevalence)

# comparison of global topic proportions
diff_matrix <- mod_prev$theta - mod_ctm$theta # matrix of differences between (document-level) topic proportions
abs_diff_matrix <- abs(diff_matrix) # absolute differences

scales::percent(mean(rowMeans(abs_diff_matrix)), accuracy = 0.01) # average absolute difference in topic proportions per topic, averaged across all documents
scales::percent(mean(abs(colMeans(diff_matrix))), accuracy = 0.01) # average absolute difference in global (!) topic proportions per topic (topic proportions simply averaged across all documents)

