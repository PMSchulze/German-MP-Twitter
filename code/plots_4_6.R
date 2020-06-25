# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "betareg", "ggcorrplot", "grid", "gridExtra", "huge", "knitr", "mvtnorm", 
  "quanteda", "reshape2", "scales", "stm", "stringi", "tidyverse", "tm"
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
# setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')
setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse/code")

# ----------------------------------------------------------------------------------------------
# load data
data <- readRDS("../data/preprocessed_monthly.rds")
data_train <- readRDS("../data/preprocessed_monthly_train.rds")
data_test <- readRDS("../data/preprocessed_monthly_test.rds")
colnames_table <- read.csv(file = "../data/topic_monthly_colnames.csv")

# data_corpus <- readRDS("../data/prep_monthly.rds")
# data_aggregated <- readRDS("../data/preprocessed.rds") # MP-level (non-monthly) data

K <- 15

# choose covariates and number of topics
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~")) 

# ----------------------------------------------------------------------------------------------

# # fit model on training data
# mod_train <- stm::stm(
#   documents = data_train$documents,
#   vocab = data_train$vocab,
#   data = data_train$meta,
#   K = K,
#   prevalence = prevalence,
#   gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 300,
#   init.type = "Spectral")
# saveRDS(mod_train, "../data/mod_monthly_train.rds")

mod_train <- readRDS("../data/mod_monthly_train.rds")

# ----------------------------------------------------------------------------------------------

# label topics
n <- 10
topic_words_train <- labelTopics(mod_train, n = n)

topic_labels_train <- list(
  Topic1 = "Arms Industry/War",
  Topic2 = "English",
  Topic3 = "Twitter/Politics 1",
  Topic4 = "Twitter/Politics 2",
  Topic5 = "Miscellaneous 1",
  Topic6 = "Right/Nationalist 1",
  Topic7 = "Twitter/Politics 3",
  Topic8 = "Europe",
  Topic9 = "Green/Climate",
  Topic10 = "Income/Taxation",
  Topic11 = "Twitter/Politics 3",
  Topic12 = "Emancipation",
  Topic13 = "Left/Social",
  Topic14 = "Corona",
  Topic15 = "Right/Nationalist 2"
)

# ----------------------------------------------------------------------------------------------

# align corpus
data_test <- stm::alignCorpus(new = data_test, old.vocab = mod_train$vocab, verbose = TRUE)

# fit new documents
test <- stm::fitNewDocuments(
  model = mod_train, 
  documents = data_test$documents, 
  # newData = data_test$meta,
  origData = data_train$meta,
  prevalence = prevalence,
  prevalencePrior = "Average",
  returnPosterior = FALSE,
  returnPriors = FALSE, 
  designMatrix = NULL, 
  test = TRUE,
  verbose = TRUE
)

# comparison plots training vs test data
## prepare dataframes
topic_props_train <- make.dt(
  mod_train, data_train$meta[c("Partei", "Bundesland", "Datum", "t", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")]
)

topic_props_test <- make.dt(
  test, data_test$meta[c("Partei", "Bundesland", "Datum", "t", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")]
)

# ----------------------------------------------------------------------------------------------

## topic proportions by party
topic_number <- 1
topic_number_long <- paste0("Topic", topic_number)
covariate <- "Partei"

plot1_train_party <- topic_props_train %>%
  ggplot(aes(!!as.symbol(covariate), !!as.symbol(topic_number_long))) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "topic proportion", 
       title = "'arms industry/war' - training data") +
  ylim(0,1) +
  # scale_color_manual(values = c("blue", "green", "black", "purple", "yellow", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot1_test_party <-  topic_props_test %>%
  ggplot(aes(!!as.symbol(covariate), !!as.symbol(topic_number_long))) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "topic proportion", 
       title = "'arms industry/war' - test data") +
  ylim(0,1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(plot1_train_party, plot1_test_party, ncol = 2)

# ----------------------------------------------------------------------------------------------

topic_number <- 9
topic_number_long <- paste0("Topic", topic_number)
covariate <- "Partei"

plot9_train_party <- topic_props_train %>%
  ggplot(aes(!!as.symbol(covariate), !!as.symbol(topic_number_long))) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "topic proportion", 
       title = "'green/climate' - training data") +
  ylim(0,1) +
  # scale_color_manual(values = c("blue", "green", "black", "purple", "yellow", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot9_test_party <-  topic_props_test %>%
  ggplot(aes(!!as.symbol(covariate), !!as.symbol(topic_number_long))) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "topic proportion", 
       title = "'green/climate' - test data") +
  ylim(0,1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(plot9_train_party, plot9_test_party, ncol = 2)

# ----------------------------------------------------------------------------------------------

## topic proportions over time (averaged monthly across all corresponding documents)
topic_number <- 9
topic_number_long <- paste0("Topic", topic_number)
covariate <- "t"

## topic proportions over time (averaged monthly across all corresponding documents)
plot9_train_time <-  topic_props_train %>%
  select(,topic_number+1) %>%
  aggregate(list(topic_props_train[[covariate]]), median) %>%
  ggplot(aes(Group.1, !!as.symbol(topic_number_long))) +
  geom_point() +
  labs(x = "time", y = "topic proportion", 
       title = "'green/climate' - training data") +
  ylim(0, 0.10) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot9_test_time <-   topic_props_test %>%
  select(,topic_number+1) %>%
  aggregate(list(topic_props_test[[covariate]]), median) %>%
  ggplot(aes(Group.1, !!as.symbol(topic_number_long))) +
  geom_point() +
  labs(x = "time", y = "topic proportion", 
       title = "'green/climate' - test data") +
  ylim(0, 0.10) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(plot9_train_time, plot9_test_time, ncol = 2)

# ----------------------------------------------------------------------------------------------

## topic proportions over time (averaged monthly across all corresponding documents)
topic_number <- 14
topic_number_long <- paste0("Topic", topic_number)
covariate <- "t"

## topic proportions over time (averaged monthly across all corresponding documents)
plot14_train_time <-  topic_props_train %>%
  select(,topic_number+1) %>%
  aggregate(list(topic_props_train[[covariate]]), median) %>%
  ggplot(aes(Group.1, !!as.symbol(topic_number_long))) +
  geom_point() +
  labs(x = "time", y = "topic proportion", 
       title = "'corona' - training data") +
  ylim(0, 0.60) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot14_test_time <-   topic_props_test %>%
  select(,topic_number+1) %>%
  aggregate(list(topic_props_test[[covariate]]), median) %>%
  ggplot(aes(Group.1, !!as.symbol(topic_number_long))) +
  geom_point() +
  labs(x = "time", y = "topic proportion", 
       title = "'corona' - test data") +
  ylim(0, 0.60) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(plot14_train_time, plot14_test_time, ncol = 2)

# ----------------------------------------------------------------------------------------------

library(stmprevalence)

formula <- 1:15~Partei + Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)

gamma <- mod_train$mu$gamma
Sigma <- mod_train$sigma
xmat <- stm::makeDesignMatrix(formula, data_train$meta, data_test$meta)
mu <- xmat %*% gamma
preds_theta <- stmprevalence::sim_theta(mu, Sigma, nsims = 1000, ci_lower = 0.025, 
                 ci_upper = 0.975)

# ----------------------------------------------------------------------------------------------

cbind(preds_theta$Topic1, data_test$meta) %>% 
  ggplot(aes(Partei, proportion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "topic proportion", 
       title = "'arms industry/war' - training data") +
  ylim(0,1) +
  # scale_color_manual(values = c("blue", "green", "black", "purple", "yellow", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
  
# ----------------------------------------------------------------------------------------------

cbind(preds_theta$Topic9, data_test$meta) %>% 
  ggplot(aes(Partei, proportion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "topic proportion", 
       title = "'green/climate' - test data") +
  ylim(0,1) +
  # scale_color_manual(values = c("blue", "green", "black", "purple", "yellow", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# ----------------------------------------------------------------------------------------------

topic_number <- 9
topic_number_long <- paste0("Topic", topic_number)
covariate <- "t"

preds_theta[[topic_number_long]]$proportion %>% 
  aggregate(list(topic_props_test[[covariate]]), median) %>% 
  setNames(c(covariate, topic_number_long)) %>%
  ggplot(aes(!!as.symbol(covariate), !!as.symbol(topic_number_long))) +
  geom_point() +
  labs(x = "time", y = "topic proportion", 
       title = "'green/climate' - test data") +
  ylim(0, 0.15) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
